namespace FSharp.Enterprise
#nowarn "44"

module Timeseries = 

    open System
    open FSharp.Enterprise.OptionOperators
    
    module Point = 
    
        type T<'a> = {
            X : DateTimeOffset
            Y : 'a
        }
        
        let create x y = { X = x; Y = y } 

    type T<'a>(startDate:DateTimeOffset, interval:TimeSpan, position:int, values:seq<'a>) = 
        let buffer = RingBuffer.create(position, values)
        let mutable startDate = startDate
    
        let offset (a:DateTimeOffset) (b:DateTimeOffset) = 
            (b.Subtract(a).TotalMinutes / interval.TotalMinutes) |> floor |> int
            
        member x.Length with get() = buffer.Length
        member x.Interval with get() = interval
        member x.StartDate with get() = startDate and private set(value) = startDate <- value
        member x.EndDate with get() = startDate.AddMinutes(float (buffer.Length - 1) * x.Interval.TotalMinutes)
        member x.Indices with get() = [|0 .. buffer.Length - 1|] |> Array.map (fun i -> x.StartDate.AddMinutes(float i * x.Interval.TotalMinutes))
        member private x.IsDateInBounds date = date >= x.StartDate && date <= x.EndDate
        
        member x.Item
            with get(date) =
                if not(x.IsDateInBounds date) then raise(IndexOutOfRangeException())
                let offsetIndex = offset x.StartDate date 
                buffer.[offsetIndex]
            and set date value =
                if not(x.IsDateInBounds date) then raise(IndexOutOfRangeException())
                let offsetIndex = offset x.StartDate date
                buffer.[offsetIndex] <- value

        member x.TryItem
            with get(date) =
                if x.IsDateInBounds date then
                    let offsetIndex = offset x.StartDate date
                    Some(buffer.[offsetIndex])
                else
                    None
            and set date value =
                if x.IsDateInBounds date then
                    let offsetIndex = offset x.StartDate date
                    buffer.[offsetIndex] <- value

        /// Tries to advance the start date of the Timeseries to toDate.
        /// Returns None if toDate is before the start date of the Timeseries, 
        /// otherwise Some containing the start date of the Timeseries.
        member x.TryAdvance(toDate:DateTimeOffset) = 
            let offsetIndex = offset x.StartDate toDate            
            match buffer.TryAdvance(offsetIndex) with
            | Some(_) ->
                if offsetIndex > 0 then
                    x.StartDate <- toDate
                Some(x.StartDate)
            | None -> None
    
        /// Advances the start date of the Timeseries to toDate. Throws an 
        /// ArgumentException if toDate is before the Timeseries start date.
        member x.Advance(toDate:DateTimeOffset) = 
            match x.TryAdvance(toDate) with
            | Some(startDate) -> startDate
            | None -> invalidArg "toDate" "the toDate must be greater than or equal to the start date of the Timeseries"
                   
        member x.ToArray() =
            buffer.ToArray()

        member x.Clone() = 
            T<'a>(x.StartDate, x.Interval, buffer.Position, buffer.ToArray())

    let inline create(startDate, interval, values) = 
        T(startDate, interval, 0, values)

    let inline empty<'T>(startDate, interval, size) : T<'T> =
        create(startDate, interval, Array.zeroCreate size)       

    let inline checkNonNull argName arg = 
        match box arg with 
        | null -> nullArg argName 
        | _ -> ()

    let private convertToInterval (targetGran:TimeSpan) (currGran:TimeSpan) (values:seq<'a>) =
        if currGran = targetGran then 
            values 
        elif currGran > targetGran then
            let ratio = currGran.TotalMinutes / targetGran.TotalMinutes |> int
            FSharpx.Collections.Seq.grow ratio values
        else
            let ratio = targetGran.TotalMinutes / currGran.TotalMinutes |> int
            Seq.contract ratio values

    let toInterval (interval:TimeSpan) (timeseries:T<'T>) =
        if interval = timeseries.Interval then 
            timeseries
        else 
            create(timeseries.StartDate, interval, timeseries.ToArray() |> convertToInterval interval timeseries.Interval)           

    [<CompiledName("Map")>]
    let inline map (f: 'T -> 'U) (timeseries:T<'T>) =
        checkNonNull "timeseries" timeseries
        let res = empty(timeseries.StartDate, timeseries.Interval, timeseries.Length)
        for date in timeseries.Indices do 
            res.[date] <- f timeseries.[date]
        res

    [<CompiledName("MapIndexed")>]
    let mapi (f : DateTimeOffset -> 'T -> 'U) (timeseries:T<'T>) =
        checkNonNull "timeseries" timeseries
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        let res = empty(timeseries.StartDate, timeseries.Interval, timeseries.Length)
        for date in timeseries.Indices do 
            res.[date] <- f.Invoke(date,timeseries.[date])
        res

    [<CompiledName("Map2")>]
    let map2 f (timeseries1:T<'T>) (timeseries2:T<'U>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        if timeseries1.Length <> timeseries2.Length then invalidArg "timeseries2" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries2.StartDate then invalidArg "timeseries2" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do  
            res.[date] <- f.Invoke(timeseries1.[date], timeseries2.[date])
        res

    [<CompiledName("Map3")>]
    let map3 f (timeseries1:T<'T>) (timeseries2:T<'U>) (timeseries3:T<'V>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        checkNonNull "timeseries3" timeseries3
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
        if timeseries1.Length <> timeseries2.Length then invalidArg "timeseries2" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries2.StartDate then invalidArg "timeseries2" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        if timeseries1.Length <> timeseries3.Length then invalidArg "timeseries3" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries3.StartDate then invalidArg "timeseries3" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries3.Interval then invalidArg "timeseries3" "timeseries have different intervals";
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do  
            res.[date] <- f.Invoke(timeseries1.[date], timeseries2.[date], timeseries3.[date])
        res

    [<CompiledName("Map4")>]
    let map4 f (timeseries1:T<'T>) (timeseries2:T<'U>) (timeseries3:T<'V>) (timeseries4:T<'W>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        checkNonNull "timeseries3" timeseries3
        checkNonNull "timeseries4" timeseries4
        let f = OptimizedClosures.FSharpFunc<_,_,_,_,_>.Adapt(f)
        if timeseries1.Length <> timeseries2.Length then invalidArg "timeseries2" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries2.StartDate then invalidArg "timeseries2" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        if timeseries1.Length <> timeseries3.Length then invalidArg "timeseries3" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries3.StartDate then invalidArg "timeseries3" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries3.Interval then invalidArg "timeseries3" "timeseries have different intervals";
        if timeseries1.Length <> timeseries4.Length then invalidArg "timeseries4" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries4.StartDate then invalidArg "timeseries4" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries4.Interval then invalidArg "timeseries4" "timeseries have different intervals";
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do  
            res.[date] <- f.Invoke(timeseries1.[date], timeseries2.[date], timeseries3.[date], timeseries4.[date])
        res

    [<CompiledName("MapIndexed2")>]
    let mapi2 f (timeseries1:T<'T>) (timeseries2:T<'U>) = 
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
        if timeseries1.Length <> timeseries2.Length then invalidArg "timeseries2" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries2.StartDate then invalidArg "timeseries2" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do 
            res.[date] <- f.Invoke(date, timeseries1.[date], timeseries2.[date])
        res

    [<CompiledName("TryMap2")>]
    let tryMap2 f (timeseries1:T<'T>) (timeseries2:T<'U>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do  
            res.[date] <- f.Invoke(timeseries1.[date], timeseries2.TryItem(date))
        res

    [<CompiledName("TryMap3")>]
    let tryMap3 f (timeseries1:T<'T>) (timeseries2:T<'U>) (timeseries3:T<'V>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        checkNonNull "timeseries3" timeseries3
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        if timeseries1.Interval <> timeseries3.Interval then invalidArg "timeseries3" "timeseries have different intervals";
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do  
            res.[date] <- f.Invoke(timeseries1.[date], timeseries2.TryItem(date), timeseries3.TryItem(date))
        res

    [<CompiledName("TryMap5")>]
    let tryMap5 f (timeseries1:T<'T>) (timeseries2:T<'U>) (timeseries3:T<'V>) (timeseries4:T<'W>) (timeseries5:T<'X>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        checkNonNull "timeseries3" timeseries3
        checkNonNull "timeseries4" timeseries4
        checkNonNull "timeseries5" timeseries5
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        if timeseries1.Interval <> timeseries3.Interval then invalidArg "timeseries3" "timeseries have different intervals";
        if timeseries1.Interval <> timeseries4.Interval then invalidArg "timeseries4" "timeseries have different intervals";
        if timeseries1.Interval <> timeseries5.Interval then invalidArg "timeseries5" "timeseries have different intervals";
        let f = OptimizedClosures.FSharpFunc<_,_,_,_,_,_>.Adapt(f)
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do  
            res.[date] <- f.Invoke(timeseries1.[date], timeseries2.TryItem(date), timeseries3.TryItem(date), timeseries4.TryItem(date), timeseries5.TryItem(date))
        res

    [<CompiledName("TryMapIndexed2")>]
    let tryMapi2 f (timeseries1:T<'T>) (timeseries2:T<'U>) = 
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do 
            res.[date] <- f.Invoke(date, timeseries1.[date], timeseries2.TryItem(date))
        res

    [<CompiledName("TryMapIndexed3")>]
    let tryMapi3 f (timeseries1:T<'T>) (timeseries2:T<'U>) (timeseries3:T<'W>) = 
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        checkNonNull "timeseries3" timeseries3
        let f = OptimizedClosures.FSharpFunc<_,_,_,_,_>.Adapt(f)
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        if timeseries1.Interval <> timeseries3.Interval then invalidArg "timeseries3" "timeseries have different intervals";
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do 
            res.[date] <- f.Invoke(date, timeseries1.[date], timeseries2.TryItem(date), timeseries3.TryItem(date))
        res

    [<CompiledName("Iterate")>]
    let inline iter f (timeseries:T<'T>) = 
        checkNonNull "timeseries" timeseries
        for date in timeseries.Indices do 
            f timeseries.[date]

    [<CompiledName("IterateIndexed")>]
    let iteri f (timeseries:T<'T>) =
        checkNonNull "timeseries" timeseries
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        for date in timeseries.Indices do 
            f.Invoke(date, timeseries.[date])

    [<CompiledName("Iterate2")>]
    let iter2 f (timeseries1:T<'T>) (timeseries2:T<'U>) = 
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        if timeseries1.Length <> timeseries2.Length then invalidArg "timeseries2" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries2.StartDate then invalidArg "timeseries2" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        for date in timeseries1.Indices do  
            f.Invoke(timeseries1.[date], timeseries2.[date])

    [<CompiledName("IterateIndexed2")>]
    let iteri2 f (timeseries1:T<'T>) (timeseries2:T<'U>) = 
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
        if timeseries1.Length <> timeseries2.Length then invalidArg "timeseries2" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries2.StartDate then invalidArg "timeseries2" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        for date in timeseries1.Indices do 
            f.Invoke(date,timeseries1.[date], timeseries2.[date])

    [<CompiledName("TryIterate2")>]
    let tryIter2 f (timeseries1:T<'T>) (timeseries2:T<'U>) = 
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        for date in timeseries1.Indices do  
            f.Invoke(timeseries1.[date], timeseries2.TryItem(date))

    [<CompiledName("TryIterateIndexed2")>]
    let tryIteri2 f (timeseries1:T<'T>) (timeseries2:T<'U>) = 
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        for date in timeseries1.Indices do 
            f.Invoke(date,timeseries1.[date], timeseries2.TryItem(date))

    [<CompiledName("Zip")>]
    let zip (timeseries1:T<'T>) (timeseries2:T<'U>) = 
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        if timeseries1.Length <> timeseries2.Length then invalidArg "timeseries2" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries2.StartDate then invalidArg "timeseries2" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do  
            res.[date] <- (timeseries1.[date],timeseries2.[date])
        res

    [<CompiledName("TryZip")>]
    let tryZip (timeseries1:T<'T>) (timeseries2:T<'U>) = 
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        let res = empty(timeseries1.StartDate, timeseries1.Interval, timeseries1.Length)
        for date in timeseries1.Indices do  
            res.[date] <- (timeseries1.[date],timeseries2.TryItem(date))
        res

    [<CompiledName("Unzip")>]
    let unzip (timeseries:T<_>) = 
        checkNonNull "timeseries" timeseries
        let res1 = empty(timeseries.StartDate, timeseries.Interval, timeseries.Length)
        let res2 = empty(timeseries.StartDate, timeseries.Interval, timeseries.Length)
        for date in timeseries.Indices do 
            let x,y = timeseries.[date] 
            res1.[date] <- x;
            res2.[date] <- y;
        res1,res2

    [<CompiledName("Fold")>]
    let fold<'T,'State> (f : 'State -> 'T -> 'State) (acc: 'State) (timeseries:T<'T>) =
        checkNonNull "timeseries" timeseries
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        let mutable state = acc 
        for date in timeseries.Indices do 
            state <- f.Invoke(state,timeseries.[date])
        state

    [<CompiledName("Fold2")>]
    let fold2<'T1,'T2,'State>  f (acc: 'State) (timeseries1:T<'T1>) (timeseries2:T<'T2>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
        let mutable state = acc 
        if timeseries1.Length <> timeseries2.Length then invalidArg "timeseries2" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries2.StartDate then invalidArg "timeseries2" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        for date in timeseries1.Indices do 
            state <- f.Invoke(state,timeseries1.[date],timeseries2.[date])
        state

    [<CompiledName("TryFold2")>]
    let tryFold2<'T1,'T2,'State>  f (acc: 'State) (timeseries1:T<'T1>) (timeseries2:T<'T2>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let f = OptimizedClosures.FSharpFunc<_,_,_,_>.Adapt(f)
        let mutable state = acc 
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        for date in timeseries1.Indices do 
            state <- f.Invoke(state,timeseries1.[date],timeseries2.TryItem(date))
        state
                                            
    [<CompiledName("MergeMany")>]
    let mergeMany<'T> f (startDate:DateTimeOffset, interval:TimeSpan, size:int) (manyTimeseries:seq<T<'T>>) =
        for timeseries in manyTimeseries do
            if size <> timeseries.Length then invalidArg "manyTimeseries" "timeseries have different lengths";
            if startDate <> timeseries.StartDate then invalidArg "manyTimeseries" "timeseries have different start dates";
            if interval <> timeseries.Interval then invalidArg "manyTimeseries" "timeseries have different intervals";
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        let res = empty<'T>(startDate, interval, size)
        for date in res.Indices do    
            for timeseries in manyTimeseries do
                res.[date] <- f.Invoke(res.[date],timeseries.[date])
        res
                  
    [<CompiledName("TryMergeMany")>]
    let tryMergeMany<'T> f (startDate:DateTimeOffset, interval:TimeSpan, size:int) (manyTimeseries:seq<T<'T>>) =
        for timeseries in manyTimeseries do
            if interval <> timeseries.Interval then invalidArg "manyTimeseries" "timeseries have different intervals";
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        let res = empty<'T>(startDate, interval, size)
        for date in res.Indices do    
            for timeseries in manyTimeseries do
                res.[date] <- f.Invoke(res.[date],timeseries.TryItem(date))
        res
                  
    [<CompiledName("MergeInto")>]
    let mergeInto<'T> f (timeseries1:T<'T>) (timeseries2:T<'T>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        if timeseries1.Length <> timeseries2.Length then invalidArg "timeseries2" "timeseries have different lengths";
        if timeseries1.StartDate <> timeseries2.StartDate then invalidArg "timeseries2" "timeseries have different start dates";
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        for date in timeseries1.Indices do    
            timeseries1.[date] <- f.Invoke(timeseries1.[date],timeseries2.[date])
                  
    [<CompiledName("TryMergeInto")>]
    let tryMergeInto<'T> f (timeseries1:T<'T>) (timeseries2:T<'T>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        if timeseries1.Interval <> timeseries2.Interval then invalidArg "timeseries2" "timeseries have different intervals";
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        for date in timeseries1.Indices do    
            timeseries1.[date] <- f.Invoke(timeseries1.[date],timeseries2.TryItem(date))
                  
    [<CompiledName("MergeManyInto")>]
    let mergeManyInto<'T> f (timeseries:T<'T>) (manyTimeseries:seq<T<'T>>) =
        checkNonNull "timeseries" timeseries
        for timeseries in manyTimeseries do
            if timeseries.Length <> timeseries.Length then invalidArg "manyTimeseries" "timeseries have different lengths";
            if timeseries.StartDate <> timeseries.StartDate then invalidArg "manyTimeseries" "timeseries have different start dates";
            if timeseries.Interval <> timeseries.Interval then invalidArg "manyTimeseries" "timeseries have different intervals";
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        for date in timeseries.Indices do    
            for otherTimeseries in manyTimeseries do
                timeseries.[date] <- f.Invoke(timeseries.[date],otherTimeseries.[date])
                  
    [<CompiledName("TryMergeManyInto")>]
    let tryMergeManyInto<'T> f (timeseries:T<'T>) (manyTimeseries:seq<T<'T>>) =
        checkNonNull "timeseries" timeseries
        for timeseries in manyTimeseries do
            if timeseries.Interval <> timeseries.Interval then invalidArg "manyTimeseries" "timeseries have different intervals";
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        for date in timeseries.Indices do    
            for otherTimeseries in manyTimeseries do
               timeseries.[date] <- f.Invoke(timeseries.[date],otherTimeseries.TryItem(date))
                 
    [<CompiledName("InsertAtDate")>]
    let insertAtDate<'T> (timeseries:T<'T>) (date:DateTimeOffset) (valuesInterval:TimeSpan) (values:seq<'T>) =
        checkNonNull "timeseries" timeseries
        values 
        |> convertToInterval timeseries.Interval valuesInterval
        |> Seq.iteri (fun i value -> timeseries.TryItem(date.AddMinutes(float i * timeseries.Interval.TotalMinutes)) <- value)

    [<CompiledName("Slice")>]
    let slice<'T> startDate size (timeseries:T<'T>) =
        let res = empty<'T>(startDate, timeseries.Interval, size)
        for date in res.Indices do
            res.[date] <- timeseries.[date]                 
        res
    
    [<CompiledName("forall2")>]
    let forall2 (pred : 'T -> 'U -> bool) (timeseries1:T<'T>) (timeseries2:T<'U>) =
        checkNonNull "timeseries1" timeseries1
        checkNonNull "timeseries2" timeseries2
        let mutable ok = true
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(pred)
        if timeseries1.Length <> timeseries2.Length then  ok <- false
        if timeseries1.StartDate <> timeseries2.StartDate then ok <- false
        if timeseries1.Interval <> timeseries2.Interval then ok <- false
        let enum = timeseries1.Indices.GetEnumerator()
        while ok && enum.MoveNext() do
            let curr = enum.Current :?> DateTimeOffset
            ok <- f.Invoke(timeseries1.[curr], timeseries2.[curr])
        ok


    let toDataPoints (timeseries:T<'T>) =
         Array.map2 (Point.create) timeseries.Indices (timeseries.ToArray())

    let toDataPointsAtDate (date:DateTimeOffset) (timeseries:T<'T>) =
        toDataPoints timeseries
        |> Array.filter (fun d -> d.X >= date)

    let toArray (timeseries:T<'T>) = timeseries.ToArray()
