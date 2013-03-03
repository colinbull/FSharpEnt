// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r @"..\..\packages\Newtonsoft.Json.4.5.11\lib\net40\Newtonsoft.Json.dll"
#load "Serialisation.fs"
open FSharp.Enterprise.Serialisation

let serialiser = Json.ByteSerialiser

let isByteSerialiser = 
    match serialiser with
    | :? ISerialiser<byte[]> -> true
    | _ -> false 

