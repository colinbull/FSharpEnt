namespace FSharp.Enterprise

module Regex = 
    
    open System.Text.RegularExpressions
    
    let hasMatches (pat:string) (inp:string) = 
        Regex.Match(inp, pat).Success
    
    let tryMatches (pat:string) (inp:string) = 
        if (hasMatches pat inp) then Some(inp) else None
    
    let rec (|Matches|_|) (pat:string) (inp:string) =
        let rec mtch (m : Match) s =
            if m.Success
            then mtch (m.NextMatch()) (Option.map (fun s' -> (m.Index, List.tail [ for g in m.Groups -> g.Value ]) :: s') s)
            else s
        mtch (Regex.Match(inp, pat)) (Some <| [])
        
    let rec (|MatchAll|_|) (pat:string) (inp:string) =
        let rec mtch (m : Match) s =
            if m.Success
            then mtch (m.NextMatch()) (Option.map (fun s' -> List.tail [ for g in m.Groups do 
                                                                            for c in g.Captures -> c.Value ] @ s') s)
            else s
        mtch (Regex.Match(inp, pat)) (Some <| [])
    
    let (|Match|_|) (pat:string) (inp:string) =
        let m = Regex.Match(inp, pat) in
            if m.Success
            then Some (List.tail [ for g in m.Groups -> g.Value ])
            else None
    
    let (|Match1|_|) (pat:string) (inp:string) =
        match (|Match|_|) pat inp with
        | Some (fst :: []) -> Some (fst)
        | _ -> None
    
    let (|Match2|_|) (pat:string) (inp:string) =
        match (|Match|_|) pat inp with
        | Some (fst :: snd :: []) -> Some (fst, snd)
        | _ -> None 
    
    let (|Match3|_|) (pat:string) (inp:string) =
        match (|Match|_|) pat inp with
        | Some (fst :: snd :: trd :: []) -> Some (fst, snd, trd)
        | _ -> None 
    
    let (|Match4|_|) (pat:string) (inp:string) =
        match (|Match|_|) pat inp with
        | Some (fst :: snd :: trd :: fth :: []) -> Some (fst, snd, trd, fth)
        | _ -> None 
    
    let (|Match5|_|) (pat:string) (inp:string) =
        match (|Match|_|) pat inp with
        | Some (fst :: snd :: trd :: fth :: fvth :: []) -> Some (fst, snd, trd, fth, fvth)
        | _ -> None
    
    let (|Match6|_|) (pat:string) (inp:string) =
        match (|Match|_|) pat inp with
        | Some (fst :: snd :: trd :: fth :: fvth :: sth :: []) -> Some (fst, snd, trd, fth, fvth, sth)
        | _ -> None 
    
    let (|Match7|_|) (pat:string) (inp:string) =
        match (|Match|_|) pat inp with
        | Some (fst :: snd :: trd :: fth :: fvth :: sth :: snth :: []) -> Some (fst, snd, trd, fth, fvth, sth, snth)
        | _ -> None
    
    let match_one regex inp =
        (|Match1|_|) regex inp
    
    let match_all regex inp =
        (|MatchAll|_|) regex inp 
    
    let ``match`` regex inp = 
        (|Match|_|) regex inp
    
    let matchThen regex inp f =
        match match_one regex inp with
        | Some(res) -> f(res)
        | None -> None

