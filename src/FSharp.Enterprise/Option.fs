namespace FSharp.Enterprise

module Option =
    
    /// Accepts an option of option and concatenates them.
    let inline concat x = 
        Option.bind id x

    let inline accumulate (acc:Option<_>) (value:Option<_>) =
        if acc.IsSome then
            if value.IsSome 
            then Some (acc.Value + value.Value)
            else acc
        else
            value

    let inline (<&>) f m = 
        Option.map (fun f' -> f' m) f

module OptionOperators =

    open System

    let (?>=) (x : Option<'T>) (y: 'T) = x.IsSome && x.Value >= y

    let (?>) (x : Option<'T>) (y: 'T) = x.IsSome && x.Value > y

    let (?<=) (x : Option<'T>) (y: 'T) = x.IsSome && x.Value <= y

    let (?<) (x : Option<'T>) (y: 'T) = x.IsSome && x.Value < y

    let (?=) (x : Option<'T>) (y: 'T) = x.IsSome && x.Value = y

    let (?<>) (x : Option<'T>) (y: 'T) = not (x ?= y)

    let (>=?) (x : 'T) (y: Option<'T>) = y.IsSome && x >= y.Value

    let (>?) (x : 'T) (y: Option<'T>) = y.IsSome && x > y.Value

    let (<=?) (x : 'T) (y: Option<'T>) = y.IsSome && x <= y.Value

    let (<?) (x : 'T) (y: Option<'T>) = y.IsSome && x < y.Value

    let (=?) (x : 'T) (y: Option<'T>) = y.IsSome && x = y.Value

    let (<>?) (x : 'T) (y: Option<'T>) = not (x =? y)

    let (?>=?) (x : Option<'T>) (y: Option<'T>) = (x.IsSome && y.IsSome && x.Value >= y.Value)

    let (?>?) (x : Option<'T>) (y: Option<'T>) = (x.IsSome && y.IsSome && x.Value > y.Value)

    let (?<=?) (x : Option<'T>) (y: Option<'T>) = (x.IsSome && y.IsSome && x.Value <= y.Value)

    let (?<?) (x : Option<'T>) (y: Option<'T>) = (x.IsSome && y.IsSome && x.Value < y.Value)

    let (?=?) (x : Option<'T>) (y: Option<'T>) = (not x.IsSome && not y.IsSome) || (x.IsSome && y.IsSome && x.Value = y.Value)

    let (?<>?) (x : Option<'T>) (y: Option<'T>) = not (x ?=? y)

    let inline (?+) (x : Option<_>) y = if x.IsSome then Some(x.Value + y) else None
    let inline (+?) x (y: Option<_>) = if y.IsSome then Some(x + y.Value) else None
    let inline (?+?) (x : Option<_>) (y: Option<_>) = if x.IsSome && y.IsSome then Some(x.Value + y.Value) else None

    let inline (?-) (x : Option<_>) y = if x.IsSome then Some(x.Value - y) else None
    let inline (-?) x (y: Option<_>) = if y.IsSome then Some(x - y.Value) else None
    let inline (?-?) (x : Option<_>) (y: Option<_>) = if x.IsSome && y.IsSome then Some(x.Value - y.Value) else None

    let inline ( ?*  ) (x : Option<_>) y = if x.IsSome then Some(x.Value * y) else None
    let inline ( *?  ) x (y: Option<_>) = if y.IsSome then Some(x * y.Value) else None
    let inline ( ?*? ) (x : Option<_>) (y: Option<_>) = if x.IsSome && y.IsSome then Some(x.Value * y.Value) else None

    let inline ( ?%  ) (x : Option<_>) y = if x.IsSome then Some(x.Value % y) else None
    let inline ( %?  ) x (y: Option<_>) = if y.IsSome then Some(x % y.Value) else None
    let inline ( ?%? ) (x : Option<_>) (y: Option<_>) = if x.IsSome && y.IsSome then Some(x.Value % y.Value) else None

    let inline ( ?/  ) (x : Option<_>) y = if x.IsSome then Some(x.Value / y) else None
    let inline ( /?  ) x (y: Option<_>) = if y.IsSome then Some(x / y.Value) else None
    let inline ( ?/? ) (x : Option<_>) (y: Option<_>) = if x.IsSome && y.IsSome then Some(x.Value / y.Value) else None

    let inline equalWithin tol (x : Option<_>) (y: Option<_>) =
        (not x.IsSome && not y.IsSome) || (x.IsSome && y.IsSome && abs(x.Value - y.Value) <= tol)
