namespace FSharp.Enterprise

module Timeline =
    
    open FSharp.Enterprise.OptionOperators

    module Interval =
        
        type T<'n> = Option<'n> * Option<'n>
        let make (n1,n2) : T<'n> = (n1,n2)
        let empty () : T<'n> = None,None
        let first (interval:T<'n>) = fst interval
        let second (interval:T<'n>) = snd interval
        let isClosed (interval:T<'n>) = 
            (first interval).IsSome && (second interval).IsSome
        let order (interval:T<'n>) = 
            if isClosed interval && first interval ?>? second interval then
                make(second interval, first interval)
            else
                interval
        let isIn n interval =        
            let contains n interval = n ?>=? first interval && n ?<=? second interval 
            (order >> contains n) interval
        let inline delta interval = 
            second interval ?-? first interval
