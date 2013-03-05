namespace FSharp.Enterprise

module FileSystem = 
    
    open System
    open System.IO
    open System.Runtime.Caching
    open Serialisation

    let private cache : MemoryCache = MemoryCache.Default;
    let mutable private  slidingExpiration = TimeSpan.FromSeconds(15.)
     
    let private itemRemovedHandler (f  : string -> obj -> unit) = 
        new CacheEntryRemovedCallback(
                          fun x -> 
                            match x.RemovedReason with
                            | CacheEntryRemovedReason.Expired ->
                                            f x.CacheItem.Key x.CacheItem.Value
                            | _ -> ())
     
    let private doWrite (serialiser : ISerialiser<string>) (path : string) (payload : 'a) =
            let payload = serialiser.Serialise(payload)
            File.WriteAllText(path, payload)

    let private addToWriteCache (serialiser : ISerialiser<string>) (path : string) (payload : 'a) (cache : MemoryCache) =
        cache.Set(path, payload, new CacheItemPolicy(SlidingExpiration = slidingExpiration, RemovedCallback = itemRemovedHandler (doWrite serialiser)))
    
    let setExpiration expiration = 
        slidingExpiration <- expiration

    let write (serialiser : ISerialiser<string>) path payload =
        addToWriteCache serialiser path payload cache

    let read<'a> path (serialiser : ISerialiser<string>) =
        if cache.Contains(path)
        then unbox<'a> (cache.Get(path)) |> Some
        else
            if IO.File.Exists(path)
            then
                use fs = File.Open(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
                use sr = new StreamReader(fs)
                serialiser.Deserialise<'a> (sr.ReadToEnd()) |> Some
            else None

    let readAllFiltered<'a> predicate path (serialiser : ISerialiser<string>) =
        seq {
            for file in Directory.EnumerateFiles(path) |> Seq.filter predicate do
                match read<'a> file serialiser with
                | Some(a) -> yield a 
                | None -> ()
        }

    let readAll<'a> = readAllFiltered<'a> (fun _ -> true)

    let delete path = 
        if cache.Contains(path) then cache.Remove(path) |> ignore
        if File.Exists(path) then File.Delete(path)

    let fullPathFrom rootDir file = 
        match Path.IsPathRooted(file) with
        | true -> file
        | false -> 
             match rootDir with
             | Some(root) -> Path.Combine(root, file)
             | None -> Path.GetFullPath(file)
        |> fun x -> new FileInfo(x)

