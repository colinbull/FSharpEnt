namespace FSharp.Enterprise

#if INTERACTIVE
open FSharp.Enterprise
#endif

module Point =

    type T<'x,'y> = {
        X : 'x
        Y : 'y
    }

    let make (x,y) = { X = x; Y = y }
    let empty x = make (x, Unchecked.defaultof<'y>)
    let x p = p.X
    let y p = p.Y
    let mapX f p = make (f p.X,p.Y)
    let mapY f p = make (p.X,f p.Y)

    module Time =

        open System

        type T<'v> = T<DateTimeOffset,'v>
    
        let time (p:T<'v>) = p.X
        let value (p:T<'v>) = p.Y
