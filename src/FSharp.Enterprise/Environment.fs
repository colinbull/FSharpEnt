namespace FSharp.Enterprise

open System
open System.Reflection
open System.Configuration

module Environment = 
    
    let connectionStrings =
        lazy 
        [for cs in ConfigurationManager.ConnectionStrings do
            yield (cs.Name, cs.ConnectionString)] |> Map.ofList

    let appSettings =
        lazy
        [for cs in ConfigurationManager.AppSettings do
            yield (cs, ConfigurationManager.AppSettings.Get(cs))] |> Map.ofList
    
    let tryFindAppSetting (name : string) = 
        appSettings.Value |> Map.tryPick (fun k a -> if k.ToLower() = name.ToLower() then Some(a) else None) 
    
    let tryFindConnectionString (name : string) = 
        connectionStrings.Value |> Map.tryPick (fun k a -> if k.ToLower() = name.ToLower() then Some(a) else None) 

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

