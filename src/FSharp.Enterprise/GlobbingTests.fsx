// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#r "System.Runtime.Caching.dll"
#r "System.Runtime.Serialization.dll"
#r @"..\..\packages\Newtonsoft.Json.4.5.11\lib\net40\Newtonsoft.Json.dll"

open System
open System.IO

#load "Json.fs"
#load "Serialisation.fs"
#load "Caching.fs"
#load "FileSystem.fs"

open FSharp.Enterprise

let relativePath = "D:/Appdev/FSharp.Enterprise/**/*.fs"
let relativePathWithFileName = "D:/Appdev/FSharp.Enterprise/**/A*.fs"
let fullPath = "D:\Appdev\FSharp.Enterprise\src\FSharp.Enterprise\Agent.fs"


FileSystem.Search.findFiles relativePath
|> Seq.iter (printfn "%A")

FileSystem.Search.findFiles relativePathWithFileName
|> Seq.iter (printfn "%A")

FileSystem.Search.findFiles fullPath
|> Seq.iter (printfn "%A")
