namespace FSharp.Enterprise

#if INTERACTIVE
open FSharp.Enterprise
#endif

module Line =
    
    open System.Runtime.Serialization     
    open FSharpx
    open FSharpx.Option
    open FSharp.Enterprise.OptionOperators

    type LineType =
        | InstantaneousSegments
        | DiscreteSegments of IntervalType.T
        | ContinuousSegments

    type T<'x,'y> = {
        Type : LineType
        Segments : Segment.T<'x,'y> array
    }

    let inline checkLineType argName ``type`` line = 
        if line.Type <> ``type`` then
            let message = sprintf "invalid line type: expected %A but was %A" ``type`` line.Type
            invalidArg argName message

    let internal makeFromSegments lineType segments = { Type = lineType; Segments = segments }
    let makeDiscreteFromSegments intervalType segments = makeFromSegments (DiscreteSegments intervalType) segments
    let makeContinuousFromSegments segments = makeFromSegments ContinuousSegments segments
                                
    let make lineType points =             
        let segments =
            match lineType with
            | InstantaneousSegments ->
                points 
                |> Seq.map Segment.makeInstantaneous 
                |> Seq.toArray
            | DiscreteSegments _ ->
                if Seq.length points = 1 then
                    let p = Seq.head points
                    [| Segment.makeDiscrete (Interval.make(p.X,p.X), p.Y) |]
                else
                    Seq.pairwise points
                    |> Seq.map (fun (p1,p2) -> Segment.makeDiscrete (Interval.make(p1.X,p2.X), p1.Y))
                    |> Seq.toArray
            | ContinuousSegments _ ->
                if Seq.length points = 1 then
                    let p = Seq.head points
                    [| Segment.makeContinuous (p,p) |]
                else             
                    Seq.pairwise points
                    |> Seq.map Segment.makeContinuous 
                    |> Seq.toArray
        makeFromSegments lineType segments

    let makeInstantaneous points = make LineType.InstantaneousSegments points
    let makeDiscrete intervalType points = make (LineType.DiscreteSegments intervalType) points
    let makeContinuous points = make LineType.ContinuousSegments points

    let empty lineType = makeFromSegments lineType [||]
    let emptyInstantaneous () = empty LineType.InstantaneousSegments
    let emptyDiscrete intervalType = empty (LineType.DiscreteSegments intervalType)
    let emptyContinuous () = empty LineType.ContinuousSegments
    let isEmpty line = line.Segments.Length = 0

    let segmentIntervalType line = 
        match line.Type with
        | InstantaneousSegments 
        | ContinuousSegments -> IntervalType.T.Closed
        | DiscreteSegments intervalType -> intervalType

    let segments line =
        line.Segments

    let segmentCount line =
        line.Segments.Length

    let startSegment line =
        if segmentCount line > 0 
        then Some line.Segments.[0]
        else None
            
    let endSegment line =
        if segmentCount line > 0 
        then Some line.Segments.[(segmentCount line) - 1]
        else None

    let startPoint line =
        line |> startSegment |> Option.map Segment.startPoint
            
    let endPoint line =
        line |> endSegment |> Option.map Segment.endPoint

    let startX line = 
        Option.map Point.x (startPoint line)

    let endX line = 
        Option.map Point.x (endPoint line)

    let startY line = 
        Option.getOrElseWith None Point.y (startPoint line)

    let endY line = 
        Option.getOrElseWith None Point.y (endPoint line)

    let range line =
        (fun t1 t2 -> Interval.make(t1,t2)) <!> startX line <*> endX line

    let inline map f line =
        let segments = Array.map f line.Segments
        { Type = line.Type; Segments = segments }   

    let inline map2 f l1 l2 =
        if l1.Type <> l2.Type then (invalidArg "l2" "line types differ")
        let segments = Array.map2 f l1.Segments l2.Segments
        { Type = l1.Type; Segments = segments }   

    let inline mapY f line =
        let segments = Array.map (Segment.mapY f) line.Segments 
        { Type = line.Type; Segments = segments }   

    let fold f state line =
        Array.fold f state line.Segments 

    let inline sumBy f line =
        fold (fun state segment -> f segment |> Option.accumulate state) None line

    let endXs line =
        line 
        |> fold (fun s segment -> Segment.endX segment :: s) []
        |> List.rev

    let startXs line =
        line 
        |> fold (fun s segment -> Segment.startX segment :: s) []
        |> List.rev

    let xs line =
        match line.Type with
        | LineType.InstantaneousSegments -> startXs line
        | _ -> 
            match startX line with
            | Some startX -> startX :: endXs line
            | _ -> []

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let exists p line =
        Array.exists p line.Segments

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forall p line =
        Array.forall p line.Segments

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

    let tryPick chooser line =
        Array.tryPick chooser line.Segments

    let inline intersections l1 l2 =
        [|
            for s1 in l1.Segments do
                for s2 in l2.Segments do
                    match Segment.intersection s1 s2 with
                    | Some point -> yield point
                    | None -> ()
        |]

    let tryFindSegment x line =        
        Array.tryFind (Segment.isInRange (segmentIntervalType line) x) line.Segments

    let tryFindValue segmentInterpolateF x line =            
        Array.tryPick (Segment.tryFindValue segmentInterpolateF (segmentIntervalType line) x) line.Segments

    let tryFindValues segmentInterpolateF x line =
        Array.choose (Segment.tryFindValue segmentInterpolateF (segmentIntervalType line) x) line.Segments

    let toPoints line =
        match line.Type with
        | InstantaneousSegments -> 
            Array.fold (fun points segment -> Segment.startPoint segment :: points) [] line.Segments
        | DiscreteSegments _
        | ContinuousSegments _ ->
            match endPoint line with
            | Some endPoint ->
                let startPoints = Array.fold (fun points segment -> Segment.startPoint segment :: points) [] line.Segments
                endPoint :: startPoints
            | None -> []
        |> List.rev

    let slice segmentInterpolateF interval line =
        let segments = 
            match line.Type with
            | InstantaneousSegments ->
                Array.filter (fun segment -> Interval.isIn IntervalType.T.Closed (Segment.startX segment) interval) line.Segments
            | DiscreteSegments _ ->
                line.Segments
                |> Array.choose (fun seg -> 
                    match (interval, seg) with
                    | Segment.Overlap (iStart, iEnd, seg) ->
                        Some (Segment.makeDiscrete (interval, Segment.startY seg))
                    | Segment.Internal seg -> 
                        Some(seg)
                    | Segment.External seg -> 
                        None
                    | Segment.OverlapStart (dt,seg) ->
                        Some (Segment.map (fun (s,e) -> Point.mapX (fun _ -> dt) s, e) seg)
                    | Segment.OverlapEnd (dt,seg) ->
                        Some (Segment.map (fun (s,e) -> s, Point.make(dt,s.Y)) seg))
            | ContinuousSegments _ ->
                let sif = Option.get segmentInterpolateF
                line.Segments
                |> Array.choose (fun seg -> 
                    match (interval, seg) with
                    | Segment.Overlap (iStart, iEnd, seg) ->
                        Some (Segment.map (fun (s,e) -> 
                                    Point.make (iStart, sif iStart seg), 
                                    Point.make (iEnd, sif iEnd seg)) seg)
                    | Segment.Internal seg -> 
                        Some(seg)
                    | Segment.External seg -> 
                        None
                    | Segment.OverlapStart (dt,seg) ->
                        Some (Segment.map (fun (s,e) -> Point.make (dt, sif dt seg), e) seg)
                    | Segment.OverlapEnd (dt,seg) ->
                        Some (Segment.map (fun (s,e) -> s, Point.make (dt, sif dt seg)) seg))
        { Type = line.Type; Segments = segments}

    let append segmentInterpolateF line1 line2 =
        //checkLineType "line1" LineType.ContinuousSegments line1
        //checkLineType "line2" LineType.ContinuousSegments line2
        match startX line1, startX line2 with
        | Some startX, Some endX ->             
            let interval = Interval.make(startX, endX)
            let line1Slice = slice segmentInterpolateF interval line1
            let segments = Array.append line1Slice.Segments line2.Segments
            { Type = line1.Type; Segments = segments}
        | None, Some _ ->
            line2
        | _ ->
            line1

    let ofInterval lineType interval  =
        let segments =
            match lineType with
            | InstantaneousSegments -> [| Segment.emptyInstantaneous (Interval.left interval) |]
            | DiscreteSegments _ -> [| Segment.emptyDiscrete interval |]
            | ContinuousSegments _ -> [| Segment.emptyContinuous interval |]
        makeFromSegments lineType segments

    module Time =
    
        open System

        type T<'v> = T<DateTimeOffset,'v option>

        let intersections l1 l2 =
            [|
                for s1 in l1.Segments do
                    for s2 in l2.Segments do
                        match Segment.Time.intersection s1 s2 with
                        | Some point -> yield point
                        | None -> ()
            |]

        let tryFindValue segmentInterpolateF time l =            
            Array.tryPick (Segment.Time.tryFindValue segmentInterpolateF (segmentIntervalType l) time) l.Segments

        /// Returns true if the predicate applied to the value at time t returns true, otherwise false.
        let isValueAtTime segmentInterpolateF t p (line:T<'v>) : bool =
            (tryFindValue segmentInterpolateF t >> p) line

        let toSeq segmentInterpolateF timeSpan line =
            range line
            |> Option.getOrElseWith Seq.empty (fun interval ->
                Interval.Time.toSeq IntervalType.T.Closed timeSpan interval
                |> Seq.map (fun time -> tryFindValue segmentInterpolateF time line))
