namespace FSharp.Enterprise

#if INTERACTIVE
open FSharp.Enterprise
#endif

module TimePoint =

    open System

    type T<'v> = 
        | TimePoint of Point.T<DateTimeOffset,'v option>
        with
            member x.Time = match x with | TimePoint p -> p.X
            member x.Value = match x with | TimePoint p -> p.Y
    
    let make (x,y) = TimePoint (Point.make (x,y))
    let empty t = TimePoint (Point.make(t,None))    
    let time (TimePoint p) = p.X
    let value (TimePoint p) = p.Y
    let mapTime f (TimePoint p) = TimePoint (Point.mapX f p)
    let mapValue f (TimePoint p) = TimePoint (Point.mapY f p)
    let lift f (TimePoint p) = TimePoint (f p)
    let lift2 f (TimePoint p1) (TimePoint p2) = TimePoint (f p1 p2)
