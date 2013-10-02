namespace FSharp.Enterprise

module Net = 
    
    open System
    open System.IO
    open System.Net
    open System.Security.Principal

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

    let setUrlAcl port = 
        let cmd, args =
            if Environment.OSVersion.Version.Major > 5
            then "netsh", String.Format(@"http add urlacl url=http://*:{0}/ user=""{1}""", port, WindowsIdentity.GetCurrent().Name);
            else "httpcfg", String.Format(@"set urlacl /u http://*:{0}/ /a D:(A;;GX;;;""{1}"")", port, WindowsIdentity.GetCurrent().User);
        match Process.executeElevated (fun si -> si.Arguments <- args; si.FileName <- cmd) (TimeSpan.FromSeconds(5.)) Process.Silent with
        | 0 -> ()
        | a -> failwithf "Failed to grant rights for listening to http, exit code: %d" a

    let canListenOnPort port = 
        try
            let httpListener = new HttpListener()
            httpListener.Prefixes.Add("http://+:" + port + "/")
            httpListener.Start()
            httpListener.Stop()
            true
        with
        | :? HttpListenerException as e ->
            if e.ErrorCode <> 5
            then raise(InvalidOperationException("Could not listen to port " + port, e))
            false
    
