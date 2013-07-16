module Seq

    let filteri pred = 
        Seq.mapi (fun i x -> i, x)
        >> Seq.choose (fun ((i,x) as a) -> if pred(a) then Some(x) else None) 
    
    let choosei pred = 
        Seq.mapi (fun i x -> i, x)
        >> Seq.choose pred 

    let picki pred = 
        Seq.mapi (fun i x -> i, x)
        >> Seq.pick pred 

    let contract n (source : seq<_>) =
        seq {
            for i in (n-1)..n..(Seq.length source - 1) do
                yield Seq.nth i source
        }
