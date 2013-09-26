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
    let x p = p.X
    let y p = p.Y
    let mapX f p = make (f p.X,p.Y)
    let mapY f p = make (p.X,f p.Y)
