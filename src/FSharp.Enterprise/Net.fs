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

    let getDefaultProxy() =
        let p = System.Net.WebRequest.DefaultWebProxy
        p.Credentials <- System.Net.CredentialCache.DefaultNetworkCredentials
        p
    
