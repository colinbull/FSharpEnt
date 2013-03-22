namespace FSharp.Enterprise

module IO =
    
    open System
    open System.Runtime.Caching
    open FSharp.Enterprise
    open Serialisation
    open Caching

    type SearchPattern = string
    type Filter = (string -> bool)

    type IIO =
        abstract member Write : string * 'a -> unit
        abstract member Read : string -> 'a
        abstract member ReadAll : string * Filter -> seq<'a>
        abstract member ReadAll : seq<SearchPattern> -> seq<'a>
        abstract member Delete : string -> unit
    
    let IO (serialiser:ISerialiser<'output>) readOp writeOp deleteOp searchOp =
        { new IIO with
            member f.Write(path, payload) = (path,serialiser.Serialise(payload)) |> writeOp 
            member f.Read(path) = readOp path |> serialiser.Deserialise
            member f.ReadAll(path, filter) = 
                    seq {
                        for file in (searchOp path) |> Seq.filter filter do
                            match f.Read(file) with
                            | Some(a) -> yield a 
                            | None -> ()
                    }
            member f.ReadAll(patterns) = 
                patterns
                |> Seq.collect searchOp
                |> Seq.map f.Read
            member f.Delete(path) = deleteOp path
        }

    let CachedIO (serialiser:ISerialiser<'output>) (expiry:TimeSpan) readOp writeOp deleteOp searchOp =
        let onRemoved reason path payload =
            match reason with
            | CacheEntryRemovedReason.Expired -> (path,serialiser.Serialise(payload)) |> writeOp
            | _ -> ()
        let cache : ICache<string,_> = Caching.ObjectCache onRemoved MemoryCache.Default
        { new IIO with
            member f.Write(path, payload) = cache.Set(path, SlidingExpiry(box payload,expiry))
            member f.Read(path) =
                match cache.TryGet(path) with
                | Some(v) -> unbox<_> v
                | None -> readOp path |> serialiser.Deserialise
            member f.ReadAll(path, filter) = 
                seq {
                    for file in (searchOp path) |> Seq.filter filter do
                        match f.Read(file) with
                        | Some(a) -> yield a 
                        | None -> ()
                }
            member f.ReadAll(patterns) = 
                patterns
                |> Seq.collect (searchOp)
                |> Seq.map f.Read
            member f.Delete(path) =
               cache.Remove(path) |> ignore
               deleteOp (path) 
        }

