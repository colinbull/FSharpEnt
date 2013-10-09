
open System
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type Segment<'a, 'b> = 
    | Instant of 'a * 'b
    | Discrete of ('a * 'a) * 'b
    | Continuous of ('a * 'b) * ('a * 'b)

type Line<'a, 'b> =  Segment<'a,'b> array


let make (ctorF:'a -> Segment<'b,'c>) (args:seq<'a>) : Line<'b,'c> = args |> Seq.map ctorF |> Seq.toArray

let discrete = make Discrete [(1,2), 2; (2,3), 3]
let contLine = make Continuous [(1,1), (2,2); (2,2), (3,3)]
let instLine = make Instant [(1,2); (2,3)]
let emptyLine : Line<int,int> = make Discrete []


