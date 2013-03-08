// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

open System
open System.IO

let idealSearchPath = "D:/Appdev/FSharp.Enterprise/**/*.fs"

type SearchParameters = {
    Recurse : bool
    Root : string
    FilePattern : string
}

let parse (str:string) = 
    let filePattern = Path.GetFileName(str)
    let dir = Path.GetDirectoryName(str)
    let isRecursive = dir.EndsWith("**")
    let root = Path.GetFullPath(if isRecursive then dir.Replace("**", "").Trim([|'\\';'/'|]) else dir) 
    { Recurse = isRecursive; Root = root; FilePattern = filePattern }

let searchDirectory pattern =
    let searchParams = parse pattern
    if searchParams.Recurse
    then Directory.EnumerateFileSystemEntries(searchParams.Root, searchParams.FilePattern, SearchOption.AllDirectories)
    else Directory.EnumerateFileSystemEntries(searchParams.Root, searchParams.FilePattern)

searchDirectory idealSearchPath
|> Seq.iter (printfn "%A")

