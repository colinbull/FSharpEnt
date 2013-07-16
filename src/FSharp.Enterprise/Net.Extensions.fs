namespace FSharp.Enterprise

[<AutoOpen>]
module NetExtensions =
    
    open System
    open FSharp.Enterprise
    open System.IO
    open System.Net
    open System.Threading
    open System.Text

    type FtpFileEntry = {
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

    type HttpListener with

      member x.AsyncGetContext() = 
        Async.FromBeginEnd(x.BeginGetContext, x.EndGetContext)
    
      static member Start(url, f) = 
        let tokenSource = new CancellationTokenSource()
        Async.Start
          ( ( async { 
                use listener = new HttpListener()
                listener.Prefixes.Add(url)
                listener.Start()
                while true do 
                  let! context = listener.AsyncGetContext()
                  Async.Start(f context, tokenSource.Token) }),
            cancellationToken = tokenSource.Token)
        tokenSource
          
      static member StartSynchronous(url, f) =
        HttpListener.Start(url, f >> async.Return)

    type WebRequest with
        
        member this.WriteAsync(body:string, ?contentType:string) =
            async {
                match contentType with
                | Some(ct) -> this.ContentType <- ct
                | None -> this.ContentType <- if String.IsNullOrEmpty(this.ContentType) then "application/x-www-form-urlencoded" else this.ContentType
                let postBytes = Encoding.UTF8.GetBytes(body)
                use! output = Async.FromBeginEnd(this.BeginGetRequestStream, this.EndGetRequestStream)
                do! output.AsyncWrite(postBytes, 0, postBytes.Length)
                output.Flush()
            }

        static member CreateAsync(uri:string, methd, handler, ?credentials, ?proxy, ?timeout, ?configure) =
            let configure = defaultArg configure (fun a -> async { return a })
            async {
                     let r = WebRequest.Create(uri)
                     r.Method <- methd
                     r.Proxy <- defaultArg proxy (Net.getDefaultProxy(None))
                     r.Credentials <- defaultArg credentials null
                     r.Timeout <- defaultArg timeout (TimeSpan.FromSeconds(30.)) |> (fun t -> t.TotalMilliseconds |> int)
                     let! req = configure(r)
                     use! response = req.AsyncGetResponse()
                     use stream = response.GetResponseStream()
                     return! handler(stream)
            }

        static member AsyncRequest(uri:string, methd, handler, ?headers, ?body) =
            let configure = 
                (fun (r : WebRequest) ->
                    async {
                        headers |> Option.iter(fun headers -> 
                                        for (key:string,value) in headers do
                                            r.Headers.Add(key,value)
                                    )
                        match body with
                        | Some(body) -> 
                            do! r.WriteAsync(body)
                        | None -> ()
                        return r
                    }
                )
            WebRequest.CreateAsync(uri, methd, handler, configure = configure)

        static member Request(uri:string, methd, handler, ?headers, ?body) =
            WebRequest.AsyncRequest(uri, methd, handler, ?headers = headers, ?body = body) |> Async.RunSynchronously

        static member ListFtpAsync(uri:string, ?credentials, ?proxy) = 
             let parseFtpList (stream : IO.Stream)=
                 async {
                     use sr = new StreamReader(stream, Text.Encoding.Default)
                     return 
                        match sr.Peek() |> char with
                        | '<' -> 
                                Html.Dom.parse sr
                                |> Html.Dom.descendantsBy (fun e -> e.Name = "a")
                                |> Seq.choose (fun e -> e.TryGetAttribute "href")         
                                |> Seq.map (fun link -> FtpFileEntry.Create(uri, link.Value))
                                |> Seq.toArray
                        | _ ->
                            seq {
                                let line = ref (sr.ReadLine())
                                while !line <> null do
                                    yield FtpFileEntry.Create(uri, !line)
                                    line := sr.ReadLine()
                            } |> Seq.toArray
                 }
             WebRequest.CreateAsync(uri, WebRequestMethods.Ftp.ListDirectory, parseFtpList, ?credentials = credentials, ?proxy = proxy)

        static member ListFtp(uri:string, ?credentials, ?proxy) =
            WebRequest.ListFtpAsync(uri, ?credentials = credentials, ?proxy = proxy) |> Async.RunSynchronously

        static member DownloadFileFtpAsync(uri:string, ?credentials, ?proxy) =
            WebRequest.CreateAsync(uri, WebRequestMethods.Ftp.DownloadFile, String.ofStreamAsync, ?credentials = credentials, ?proxy = proxy)

        static member DownloadFileFtp(uri:string, ?credentials, ?proxy) =
            WebRequest.DownloadFileFtpAsync(uri, ?credentials = credentials, ?proxy = proxy) |> Async.RunSynchronously


