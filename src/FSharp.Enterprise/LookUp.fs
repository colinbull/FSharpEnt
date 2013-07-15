namespace FSharp.Enterprise

module LookUp = 

    type Single<'key, 'payload when 'key : comparison> =  Map<'key, 'payload>
    type Double<'key, 'key1, 'payload when 'key : comparison and 'key1 : comparison> =  Map<'key, Map<'key1, 'payload>>

    let pairwise (s:Map<_,_>) =
        Map.toSeq s |> Seq.pairwise

    let tryFindBoundingValues key (s:Map<'a,'b>) =
        pairwise s 
        |> Seq.tryPick (fun ((x, y) as lowerBound,((x', y') as upperBound)) ->
            if key = x' then
                Some(None, (Some upperBound))
            elif key >= x && key < x' then
                Some((Some lowerBound), (Some upperBound))
            else 
                None)

    module Single =
        
        let tryFind key (s:Single<_,_>) = s.TryFind(key)

        let filter pred (s:Single<_,_>) : Single<_,_> = 
            Map.filter pred s

        let map f (s:Single<'a,'b>) : Single<'c,'d> = 
            Map.toSeq s
            |> Seq.map f
            |> Map.ofSeq
        
        let ofSeq s : Single<_,_> = Map.ofSeq s
        
        let empty : Single<_,_> = Map.empty

        let keys (s:Single<_,_>) =
            Map.toSeq s |> Seq.map fst
             
        let add key item (s:Single<_,_>) = s.Add(key, item)
                        
        let inline tryFindInterpolated x (m:Single<_,_>) =
            match tryFindBoundingValues x m with
            | Some(Some(x1', y1'),Some(x2', y2')) -> 
                Math.Interpolation.linear x x1' y1' x2' y2' |> Some
            | Some(None,Some(_, y2')) -> 
                Some y2'
            | Some(Some(_, y1'),None) -> 
                Some y1'
            | _ -> 
                None

    module Double = 

        let tryFind key key' (s:Double<_,_,_>) = 
            match s.TryFind(key) with
            | Some(s') -> s'.TryFind(key')
            | None -> None

        let filter pred (s:Double<_,_,_>) : Double<_,_,_> = 
            Map.filter pred s

        let map f (s:Double<'a,'b,'c>) : Double<'d,'e,'f> = 
            Map.toSeq s
            |> Seq.map f
            |> Map.ofSeq
        
        let ofSeq s : Double<_,_,_> = Seq.map (fun (k,v) -> k, Map.ofSeq v) s |> Map.ofSeq
        
        let empty : Double<_,_,_> = Map.empty

        let add key key' item (s:Double<_,_,_>) =
            match s.TryFind(key) with
            | Some(s') -> s.Add(key, s'.Add(key' , item))
            | None -> s.Add(key, Map([key', item]))

        let inline tryFindInterpolated (x:^A) (y:^B) (s:Double<_,_,_>) =
            let foo m = Single.tryFindInterpolated y m // WEIRD comment this out and it doesn't compile!
            match tryFindBoundingValues x s with
            | Some (Some (x1,m), Some (x2,m')) ->
                match tryFindBoundingValues y m, tryFindBoundingValues y m' with
                | Some(Some(y1, z1),Some(y2, z2)), Some(Some(y1', z1'),Some(y2', z2')) -> 
                    Math.Interpolation.bilinear x y z1 z1' z2 z2' x1 y1 x2 y2 |> Some
                | Some(None,Some(y2, z2)), Some(None,Some(y2', z2')) -> 
                    Math.Interpolation.linear x x1 z2 x2 z2' |> Some
                | Some(Some(y1, z1),None), Some(Some(y1', z1'),None) -> 
                    Math.Interpolation.linear x x1 z1 x2 z1' |> Some
                | _ -> 
                    None
            | Some (None, Some (_,m')) ->
                Single.tryFindInterpolated y m'
            | Some (Some (_,m),None) ->
                Single.tryFindInterpolated y m
            | _ ->
                None  
