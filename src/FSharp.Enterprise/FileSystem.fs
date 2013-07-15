namespace FSharp.Enterprise

module FileSystem = 
    
    open System
    open System.IO
    open System.Runtime.Caching
    open FSharp.Enterprise
    open Caching
    open IO

    let fullPathRelativeTo rootDir file = 
        match Path.IsPathRooted(file) with
        | true -> file
        | false -> 
             match rootDir with
             | Some(root) -> Path.Combine(root, file)
             | None -> Path.GetFullPath(file)
        |> fun x -> new FileInfo(x)

    let tryReadFile deserialiser (path:string) =
        if File.Exists(path)
        then Some (File.ReadAllText(path) |> deserialiser)
        else None

    let write serialiser (path:string,payload:'a) =
        File.WriteAllText(path, serialiser payload)
    
    [<AutoOpen>]
    module Search = 
        
        type T = {
            Recurse : bool
            Root : string
            FilePattern : string
        }

        let private parse (str:string) = 
            let dir, filePattern = 
                let fname = Path.GetFileName(str)
                if Directory.Exists(str)
                then 
                    let fi = FileInfo(str)
                    if fi.Attributes = FileAttributes.Directory
                    then fi.FullName, "*.*" 
                    else fi.Directory.FullName, fname
                else Path.GetDirectoryName(str), fname
            let isRecursive = dir.EndsWith("**")
            let root = Path.GetFullPath(if isRecursive then dir.Replace("**", "").Trim([|'\\';'/'|]) else dir) 
            let result =  { Recurse = isRecursive; Root = root; FilePattern = filePattern }
            result
           

        let DefaultSearcher (searchParams:T) = 
            if searchParams.Recurse
            then Directory.EnumerateFileSystemEntries(searchParams.Root, searchParams.FilePattern, SearchOption.AllDirectories)
            else Directory.EnumerateFileSystemEntries(searchParams.Root, searchParams.FilePattern)

        let findFiles searcher pattern  =
            parse pattern |> (defaultArg searcher DefaultSearcher)

    let FileIO serialise deserialise = 
        IO
            (tryReadFile deserialise)
            (write serialise)  
            File.Delete 
            (findFiles None)

    let CachedFileIO serialise deserialise expiry = 
        CachedIO 
            expiry
            (tryReadFile deserialise)
            (write serialise)  
            File.Delete 
            (findFiles None)

