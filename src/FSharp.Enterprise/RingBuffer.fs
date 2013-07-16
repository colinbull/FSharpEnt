namespace FSharp.Enterprise
#nowarn "44"

module RingBuffer=

    open System
    
    type T<'a>(position:int, values:seq<'a>) =
        let buffer =
            match values with
            | :? array<'a> -> values :?> array<'a>
            | _ -> values |> Seq.toArray   //I'm not sure this a good idea may be Array.Copy  
        let mutable position = position

        member private x.Buffer with get() = buffer
        member x.Position with get() = position and private set(value) = position <- value
        member x.Length with get() = x.Buffer.Length
        member private x.IsOffsetInBounds offset = offset >= 0 && offset < x.Length
        member private x.IndexOffset(i, offset) = (i + offset) % x.Buffer.Length
 
        member x.Item
            with get(offset) =
                if not(x.IsOffsetInBounds offset) then raise(IndexOutOfRangeException())
                let i = x.IndexOffset(x.Position, offset)
                x.Buffer.[i]
            and set offset value =
                if not(x.IsOffsetInBounds offset) then raise(IndexOutOfRangeException())
                let i = x.IndexOffset(x.Position, offset)
                x.Buffer.[i] <- value

        member x.TryItem
            with get(offset) =
                if x.IsOffsetInBounds offset then
                    let i = x.IndexOffset(x.Position, offset)
                    Some(x.Buffer.[i])
                else
                    None
            and set offset value =
                if x.IsOffsetInBounds offset then
                    let i = x.IndexOffset(x.Position, offset)
                    x.Buffer.[i] <- value

        /// Tries to advance the position of the RingBuffer by the offset.
        /// Returns None if offset is negative, otherwise Some containing 
        /// the position of the RingBuffer.    
        member x.TryAdvance(offset) =
            if offset >= 0 then
                for i in 0 .. offset - 1 do
                    x.Buffer.[x.IndexOffset(x.Position, i)] <- Unchecked.defaultof<'a>
                x.Position <- x.IndexOffset(x.Position, offset)
                Some(x.Position)
            else
                None

        /// Advances the position of the RingBuffer by the offset.
        /// Returns the position of the RingBuffer. Throws an ArgumentException if
        /// the offset is negative.
        member x.Advance(offset) =
            match x.TryAdvance(offset) with
            | Some(position) -> position
            | None -> invalidArg "offset" "the offset must be greater than or equal to zero"

        member x.ToArray() =
            [|
                for i in 0 .. x.Buffer.Length - 1 do
                    yield x.Buffer.[x.IndexOffset(x.Position, i)]
            |]
             
        member x.Normalize() = 
            x.Buffer.[0 .. x.Buffer.Length - 1] <- x.ToArray() 
            x.Position <- 0

        member x.Clone() = 
            T<'a>(x.Position, x.ToArray())

    let create(position,values) = T(position, values)

    let empty<'T>(size) = T<'T>(0, Array.zeroCreate size)
