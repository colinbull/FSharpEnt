namespace FSharp.Enterprise

open System
open OptionOperators

#if INTERACTIVE
open FSharp.Enterprise
#endif

module TimePoint =

    type T<'v> = { 
        Time:DateTimeOffset
        Value:Option<'v> 
    }

    let make (time,value) = 
        { Time = time; Value = value }

    let makeMany times values =
        Seq.zip times values |> Seq.map make
       
    let empty time : T<'v> = 
        make (time, None)
       
    let time point = 
        point.Time
       
    let value point = 
        point.Value
                      
    let inline map (f: DateTimeOffset * Option<'v> -> DateTimeOffset * Option<'u>) (point:T<'v>) =
        f (time point, value point) |> make
          
    let inline mapValue f (point:T<'v>) =
        make (time point, value point |> Option.map f)

    let inline mapTime f (point:T<'v>) =
        make (time point |> f, value point)

    let fold<'v,'State> (f : 'State -> DateTimeOffset * Option<'v> -> 'State) (acc: 'State) (point:T<'v>) =
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        let mutable state = acc 
        state <- f.Invoke(state,(time point, value point))
        state
