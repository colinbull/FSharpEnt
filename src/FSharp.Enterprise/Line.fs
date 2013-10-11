namespace FSharp.Enterprise

#if INTERACTIVE
open FSharp.Enterprise
#endif

module Line =
    
    open System.Runtime.Serialization     
    open FSharpx
    open FSharpx.Option
    open FSharp.Enterprise.OptionOperators

    type T<'x,'y> = 
        | Line of Segment.T<'x,'y> array

    let make ctorF args = 
        Line(Seq.map ctorF args |> Seq.toArray)

    let empty = 
        Line(Array.empty)
    
    let isEmpty (Line(segments)) = 
        Array.isEmpty segments

    let ofSegments segments = 
        Line (Seq.toArray segments)

    let segments (Line(segments)) = segments         

    let startSegment (Line(segments)) =
        if segments.Length > 0 
        then Some segments.[0]
        else None
            
    let endSegment (Line(segments)) =
        if segments.Length > 0
        then Some segments.[segments.Length - 1]
        else None

    let startPoint line =
        startSegment line |> Option.map Segment.startPoint
            
    let endPoint line =
        endSegment line |> Option.map Segment.endPoint

    let startX line = 
        Option.map Point.x (startPoint line)

    let endX line = 
        Option.map Point.x (endPoint line)

    let startY line = 
        Option.map Point.y (startPoint line)

    let endY line = 
        Option.map Point.y (endPoint line)

    let range line =
        (fun t1 t2 -> Interval.make(t1,t2)) <!> startX line <*> endX line

    let inline choose f (Line(segments)) =
        Line(Array.choose f segments)   

    let inline map f (Line(segments)) =
        Line(Array.map f segments) 

    let inline map2 f (Line(s1s)) (Line(s2s)) =
        Line(Array.map2 f s1s s2s)

    let inline mapY f (Line(segments)) =
        Line(Array.map (Segment.mapY f) segments)

    let fold f state (Line(segments)) =
        Array.fold f state segments 

    let inline sumBy f line =
        fold (fun state segment -> f segment + state) LanguagePrimitives.GenericZero line

    let endXs line =
        fold (fun s segment -> Segment.endX segment :: s) [] line
        |> List.rev

    let startXs line =
        fold (fun state segment -> Segment.startX segment :: state) [] line
        |> List.rev

    let xs line =
        match startX line with
        | Some startX -> startX :: endXs line
        | _ -> []
        |> Seq.distinct

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let exists p (Line(segments)) =
        Array.exists p segments

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forall p (Line(segments)) =
        Array.forall p segments

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let existsPoint p line =
        exists (Segment.exists p) line 

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forallPoint p line =
        forall (Segment.forall p) line

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let existsY p line =
        exists (Segment.existsY p) line 

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forallY p line =
        forall (Segment.forallY p) line

    let tryPick chooser (Line(segments)) =
        Array.tryPick chooser segments

    let inline intersections (Line(s1s)) (Line(s2s)) =
        [|
            for s1 in s1s do
                for s2 in s2s do
                    match Segment.intersection s1 s2 with
                    | Some point -> yield point
                    | None -> ()
        |]

    let tryFindSegment x (Line(segments)) =        
        Array.tryFind (fun s -> Segment.isInRange (Segment.intervalType s) x s) segments

    let tryFindValue segmentInterpolateF x (Line(segments)) =            
        Array.tryPick (fun s -> Segment.tryFindValue segmentInterpolateF (Segment.intervalType s) x s) segments

    let tryFindValues segmentInterpolateF x (Line(segments)) =
        Array.choose (fun s -> Segment.tryFindValue segmentInterpolateF (Segment.intervalType s) x s) segments

    let toPoints (Line(segments) as line)  =
        match endPoint line with
        | Some endPoint ->
            let startPoints = Array.fold (fun points segment -> Segment.startPoint segment :: points) [] segments
            endPoint :: startPoints
        | None -> []
        |> List.rev
        |> Seq.distinct

    let slice segmentInterpolateF interval (Line(segments)) =
        segments
        |> Array.choose (fun s ->
            match s with
            | Segment.Instantaneous _ -> 
                if Interval.isIn IntervalType.T.Closed (Segment.startX s) interval then Some s else None
            | Segment.Discrete (t,_,_) -> 
                match (interval, s) with
                | Segment.Overlap (iStart, iEnd, s) ->
                    Some (Segment.makeDiscrete (t, interval, Segment.startY s))
                | Segment.Internal (_,_,s) -> 
                    Some(s)
                | Segment.External (_,_,s) -> 
                    None
                | Segment.OverlapStart (dt,_,s) ->
                    Some (Segment.map (fun (p1,p2) -> Point.mapX (fun _ -> dt) p1, p2) s)
                | Segment.OverlapEnd (_,dt,seg) ->
                    Some (Segment.map (fun (p1,p2) -> p1, Point.make(dt,p1.Y)) s)
            | Segment.Continuous _ ->
                let sif = Option.get segmentInterpolateF
                match (interval, s) with
                | Segment.Overlap (iStart, iEnd, seg) ->
                    Some (Segment.map (fun _ -> 
                        Point.make (iStart, sif iStart seg), 
                        Point.make (iEnd, sif iEnd seg)) seg)
                | Segment.Internal (_,_,seg) -> 
                    Some(seg)
                | Segment.External (_,_,seg) -> 
                    None
                | Segment.OverlapStart (dt,_,seg) ->
                    Some (Segment.map (fun (_,p2) -> Point.make (dt, sif dt seg), p2) seg)
                | Segment.OverlapEnd (_,dt,seg) ->
                    Some (Segment.map (fun (p1,p2) -> p1, Point.make (dt, sif dt seg)) seg) 
        ) |> ofSegments

    let append segmentInterpolateF (Line(s1s) as line1) (Line(s2s) as line2) =
        match startX line1, startX line2 with
        | Some startX, Some endX ->             
            let interval = Interval.make(startX, endX)
            let (Line(line1SliceS)) = slice segmentInterpolateF interval line1
            let segments = Array.append line1SliceS s2s
            Line(segments)
        | None, Some _ ->
            line2
        | _ ->
            line1

    let inline area unitF (Line(segments)) =
        Array.map (Segment.area unitF) segments
        |> Array.sum

    let inline tryArea unitF (Line(segments)) =
        Array.choose (Segment.tryArea unitF) segments
        |> Array.sum

    let inline divide interpolateF xs (Line(segments)) = 
        Seq.collect (Segment.divide interpolateF xs) segments
        |> ofSegments

    /// Returns the result of applying the predicate to the end X of the 
    /// previous segment and the start X of the next segment.
    let isContiguous pred (Line(segments)) =
        Seq.pairwise segments
        |> Seq.forall (fun (s1,s2) -> pred (Segment.endX s1) (Segment.startX s2))


    module Time =
    
        open System

        type T<'v> = T<DateTimeOffset,'v>

        let tryFindValue segmentInterpolateF time (Line(segments)) =            
            Array.tryPick (fun s -> Segment.Time.tryFindValue segmentInterpolateF (Segment.intervalType s) time s) segments

        let intersections (Line(s1s)) (Line(s2s)) =
            [|
                for s1 in s1s do
                    for s2 in s2s do
                        match Segment.Time.intersection s1 s2 with
                        | Some point -> yield point
                        | None -> ()
            |]

        /// Returns true if the predicate applied to the value at time t returns true, otherwise false.
        let isValueAtTime segmentInterpolateF t p (line:T<'v>) : bool =
            (tryFindValue segmentInterpolateF t >> p) line

        let toSeq intervalType segmentInterpolateF timeSpan line =
            range line
            |> Option.getOrElseWith Seq.empty (fun interval ->
                Interval.Time.toSeq intervalType timeSpan interval
                |> Seq.map (fun time -> tryFindValue segmentInterpolateF time line))
