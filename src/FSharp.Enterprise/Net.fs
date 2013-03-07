namespace FSharp.Enterprise

module Net = 
    
    open System
    open System.IO
    open System.Net

    let findFirstFreePort (defaultPort : int) =
        let usedports = NetworkInformation.IPGlobalProperties.GetIPGlobalProperties().GetActiveTcpListeners() |> Seq.map (fun x -> x.Port)
        let ports = seq { for port in defaultPort .. defaultPort + 2048 do yield port }
        (ports |> Seq.find (fun p -> not <| Seq.exists (fun x -> x = p) usedports))

    let getBaseAddress(port : string option) =
        let port = 
             if port.IsNone || String.IsNullOrEmpty(port.Value)
             then (findFirstFreePort 8080).ToString()
             else port.Value
        sprintf "http://%s:%s/" Environment.MachineName port

    let getDefaultProxy(credentials) =
        let p = System.Net.WebRequest.DefaultWebProxy
        p.Credentials <- defaultArg credentials (System.Net.CredentialCache.DefaultNetworkCredentials :> ICredentials)
        p

    module Request = 
        
        open System.Net.Security

        type T<'a> = {
              Url : string
              Method : string
              Credentials : ICredentials
              Proxy : IWebProxy
              Timeout : TimeSpan
              Context : 'a
           }
           with 
               static member Create(uri, methd, ?context, ?credentials, ?proxy, ?timeout) =
                   {
                       Url = uri
                       Method = methd
                       Credentials = defaultArg credentials null
                       Proxy = defaultArg proxy (getDefaultProxy(None))
                       Timeout = defaultArg timeout (TimeSpan.FromSeconds(30.))
                       Context = defaultArg context Unchecked.defaultof<_>
                   }
        
        type Result<'a, 'b> = 
             | Success of T<'a> * 'b
             | Failure of T<'a> * exn
        
        let run configureRequest handler (req : T<'a>)=  
               async {
                   try 
                        let r = WebRequest.Create(req.Url)
                        r.Method <- req.Method
                        r.Proxy <- req.Proxy
                        r.Credentials <- req.Credentials
                        r.Timeout <- (req.Timeout.TotalMilliseconds |> int)
                        configureRequest(r)
                        use! response = r.AsyncGetResponse()
                        let result = handler req.Context (response.GetResponseStream())
                        response.Close()
                        return Success(req, result)
                    with e ->
                        return Failure(req, e)
               }
        
    module Ftp =
        
        type Entry = {
             Directory : string
             File : string
             FullPath : string
        }
        with
           static member Create(directory, file) =
               {
                   Directory = directory
                   File = file
                   FullPath = directory.TrimEnd('/') + "/" + file
               }
               
        let list url credentials = 
            let parseFtpList (context : unit) (stream : Stream) : Entry[] =
                use sr = new StreamReader(stream, Text.Encoding.Default)
                match sr.Peek() |> char with
                | '<' -> 
                        Html.Dom.parse sr
                        |> Html.Dom.descendantsBy (fun e -> e.Name = "a")
                        |> Seq.choose (fun e -> e.TryGetAttribute "href")         
                        |> Seq.map (fun link -> Entry.Create(url, link.Value))
                        |> Seq.toArray
                | _ ->
                    seq {
                        let line = ref (sr.ReadLine())
                        while !line <> null do
                            yield Entry.Create(url, !line)
                            line := sr.ReadLine()
                    } |> Seq.toArray
            async {
                let! result = 
                        Request.T<unit>.Create(url, WebRequestMethods.Ftp.ListDirectory, ?credentials = credentials)
                        |> Request.run (fun _ -> ()) parseFtpList
        
                match result with
                | Request.Success(_,r) -> return r
                | Request.Failure(_,err) -> return raise(err)
            } 
    
