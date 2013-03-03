namespace FSharp.Enterprise

module Environment =
    
    open System
    open System.Reflection
    open System.Configuration

    let connectionStrings =
        lazy 
        [for cs in ConfigurationManager.ConnectionStrings do
            yield (cs.Name, cs.ConnectionString)] |> Map.ofList

    let appSettings =
        lazy
        [for cs in ConfigurationManager.AppSettings do
            yield (cs, ConfigurationManager.AppSettings.Get(cs))] |> Map.ofList
    
    let tryFindAppSetting (name : string) = 
        appSettings.Value |> Map.tryFind name 
    
    let tryFindConnectionString (name : string) = 
        connectionStrings.Value |> Map.tryFind name

    let getAppsettingOrDefault (defaultValue : string) (name : string) =
        match tryFindAppSetting name with
        | Some(value) -> value
        | None -> defaultValue
     
    let applicationVersion =  
        lazy
            Assembly.GetEntryAssembly().GetName().Version

    let processName = 
        lazy
            System.Diagnostics.Process.GetCurrentProcess().ProcessName

    let clrVersion = 
        lazy
            Environment.Version

    let processorCount = 
        lazy
            Environment.ProcessorCount

