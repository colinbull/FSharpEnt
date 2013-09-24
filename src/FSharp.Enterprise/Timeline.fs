namespace FSharp.Enterprise

module TimeLine =
    
    open System.Runtime.Serialization     
    open FSharpx
    open FSharpx.Option
    open FSharp.Enterprise.OptionOperators

    type LineType =
        | InstantaneousSegments
        | DiscreteSegments of IntervalType.T
        | ContinuousSegments

    type T<'v> = {
        Type : LineType
        Segments : TimeSegment.T<'v> array
    }

    let inline checkLineType argName ``type`` line = 
        if line.Type <> ``type`` then
            let message = sprintf "invalid line type: expected %A but was %A" ``type`` line.Type
            invalidArg argName message
                                
    let make lineType points =
        let segments =
            match lineType with
            | InstantaneousSegments ->
                points 
                |> Seq.map TimeSegment.makeInstantaneous 
                |> Seq.toArray
            | DiscreteSegments _ ->
                if Seq.length points = 1 then
                    let p = Seq.head points
                    [| TimeSegment.makeDiscrete (Interval.Time.make(p.Time,p.Time), p.Value) |]
                else
                    Seq.pairwise points
                    |> Seq.map (fun (p1,p2) -> TimeSegment.makeDiscrete (Interval.Time.make(p1.Time,p2.Time), p1.Value))
                    |> Seq.toArray
            | ContinuousSegments _ ->
                if Seq.length points = 1 then
                    let p = Seq.head points
                    [| TimeSegment.makeContinuous (p,p) |]
                else             
                    Seq.pairwise points
                    |> Seq.map TimeSegment.makeContinuous 
                    |> Seq.toArray
        { Type = lineType; Segments = segments }

    let empty ``type`` : T<'v> =
        let segments = [||]
        { Type = ``type``; Segments = segments }

    let makeInstantaneous points = make LineType.InstantaneousSegments points
    let emptyInstantaneous () = empty LineType.InstantaneousSegments

    let makeDiscrete intervalType points = make (LineType.DiscreteSegments intervalType) points
    let makeDiscreteFromSegments intervalType segments = 
        { Type = DiscreteSegments intervalType; Segments = segments }
    let emptyDiscrete intervalType = empty (LineType.DiscreteSegments intervalType)

    let makeContinuous points = make LineType.ContinuousSegments points
    let makeContinuousFromSegments intervalType segments = 
        { Type = ContinuousSegments; Segments = segments }
    let emptyContinuous () = empty LineType.InstantaneousSegments

    let isEmpty line =
        line.Segments.Length = 0

    let segmentIntervalType line = 
        match line.Type with
        | InstantaneousSegments 
        | ContinuousSegments -> IntervalType.T.Closed
        | DiscreteSegments intervalType -> intervalType

    let segments (line:T<_>) =
        line.Segments

    let segmentCount (line:T<'v>) =
        line.Segments.Length

    let startSegment line =
        if segmentCount line > 0 
        then Some line.Segments.[0]
        else None
            
    let startPoint line =
        line |> startSegment |> Option.map TimeSegment.startPoint

    let endSegment line =
        if segmentCount line > 0 
        then Some line.Segments.[(segmentCount line) - 1]
        else None
            
    let endPoint line =
        line |> endSegment |> Option.map TimeSegment.endPoint

    let startTime line = 
        line |> startPoint |> Option.map TimePoint.time

    let startValue line = 
        line |> startPoint |> Option.getOrElseWith None TimePoint.value

    let endTime line = 
        line |> endPoint |> Option.map TimePoint.time

    let endValue line = 
        line |> endPoint |> Option.getOrElseWith None TimePoint.value

    let ofInterval lineType interval  =
        let segments =
            match lineType with
            | InstantaneousSegments -> [| TimeSegment.emptyInstantaneous (Interval.left interval) |]
            | DiscreteSegments _ -> [| TimeSegment.emptyDiscrete interval |]
            | ContinuousSegments _ -> [| TimeSegment.emptyContinuous interval |]
        { Type = lineType; Segments = segments }

    let range line =
        (fun t1 t2 -> Interval.make(t1,t2)) <!> startTime line <*> endTime line

    let inline map (f: TimeSegment.T<'v> -> TimeSegment.T<'u>) (line:T<'v>) =
        let segments = Array.map f line.Segments
        { Type = line.Type; Segments = segments }   

    let inline map2 (f: TimeSegment.T<'a> -> TimeSegment.T<'v> -> TimeSegment.T<'u>) (l1:T<'a>) (l2:T<'v>) =
        if l1.Type <> l2.Type then (invalidArg "l2" "line types differ")
        let segments = Array.map2 f l1.Segments l2.Segments
        { Type = l1.Type; Segments = segments }   

    let inline mapValue f (line:T<'v>) : T<'w> =
        let segments = Array.map (TimeSegment.mapValue f) line.Segments 
        { Type = line.Type; Segments = segments }   

    let fold<'v,'State> f (acc:'State) (line:T<'v>) =
        Array.fold f acc line.Segments 

    let inline sumBy f line =
        fold (fun state segment -> f segment |> Option.accumulate state) None line

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let exists p line =
        Array.exists p line.Segments

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forall p line =
        Array.forall p line.Segments

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let existsPoint p line =
        exists (TimeSegment.exists p) line 

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forallPoint p line =
        forall (TimeSegment.forall p) line

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let existsValue p line =
        exists (TimeSegment.existsValue p) line 

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forallValue p line =
        forall (TimeSegment.forallValue p) line

    let tryPick (chooser: TimeSegment.T<'v> -> 'u option) (line:T<'v>) =
        Array.tryPick chooser line.Segments

    let endTimes line =
        line 
        |> fold (fun s segment -> TimeSegment.endTime segment :: s) []
        |> List.rev

    let startTimes line =
        line 
        |> fold (fun s segment -> TimeSegment.startTime segment :: s) []
        |> List.rev

    let times line =
        match line.Type with
        | LineType.InstantaneousSegments -> startTimes line
        | _ -> 
            match startTime line with
            | Some startTime -> startTime :: endTimes line
            | _ -> []

    let tryFindSegment time line =        
        Array.tryFind (TimeSegment.isTimeInRange (segmentIntervalType line) time) line.Segments

    let intersections line1 line2 =
        [|
            for line1Segment in line1.Segments do
                for line2Segment in line2.Segments do
                    match TimeSegment.intersection line1Segment line2Segment with
                    | Some point -> yield point
                    | None -> ()
        |]

    let volume timeUnitF line =
        Array.choose (TimeSegment.volume timeUnitF) line.Segments |> Array.sum

    let tryFindValue segmentInterpolateF time (line:T<'v>) =            
        Array.tryPick (TimeSegment.tryFindValue segmentInterpolateF (segmentIntervalType line) time) line.Segments

    let tryFindValues segmentInterpolateF time (line:T<'v>) =
        Array.choose (TimeSegment.tryFindValue segmentInterpolateF (segmentIntervalType line) time) line.Segments

    /// Returns true if the predicate applied to the value at time t returns true, otherwise false.
    let isValueAtTime segmentInterpolateF t p line : bool =
        (tryFindValue segmentInterpolateF t >> p) line

    let toSeq segmentInterpolateF timeSpan (line:T<float<'u>>) =
        range line
        |> Option.getOrElseWith Seq.empty (fun interval ->
            Interval.Time.toSeq IntervalType.T.Closed timeSpan interval
            |> Seq.map (fun time -> tryFindValue segmentInterpolateF time line))

    let toPoints line =
        match line.Type with
        | InstantaneousSegments -> 
            Array.fold (fun points segment -> TimeSegment.startPoint segment :: points) [] line.Segments
        | DiscreteSegments _
        | ContinuousSegments _ ->
            match endPoint line with
            | Some endPoint ->
                let startPoints = Array.fold (fun points segment -> TimeSegment.startPoint segment :: points) [] line.Segments
                endPoint :: startPoints
            | None -> []
        |> List.rev

    let slice segmentInterpolateF interval line =
        let segments = 
            match line.Type with
            | InstantaneousSegments ->
                Array.filter (fun segment -> Interval.Time.isIn IntervalType.T.Closed (TimeSegment.startTime segment) interval) line.Segments
            | DiscreteSegments _ ->
                line.Segments
                |> Array.choose (fun seg -> 
                    match (interval, seg) with
                    | TimeSegment.Overlap (iStart, iEnd, seg) ->
                        Some (TimeSegment.makeDiscrete (interval, TimeSegment.startValue seg))
                    | TimeSegment.Internal seg -> 
                        Some(seg)
                    | TimeSegment.External seg -> 
                        None
                    | TimeSegment.OverlapStart (dt,seg) ->
                        Some (TimeSegment.map (fun (s,e) -> TimePoint.mapTime (fun _ -> dt) s, e) seg)
                    | TimeSegment.OverlapEnd (dt,seg) ->
                        Some (TimeSegment.map (fun (s,e) -> s, TimePoint.map (fun (_,_) -> dt,s.Value) e) seg))
            | ContinuousSegments _ ->
                line.Segments
                |> Array.choose (fun seg -> 
                    match (interval, seg) with
                    | TimeSegment.Overlap (iStart, iEnd, seg) ->
                        Some (TimeSegment.map (fun (s,e) -> 
                                    TimePoint.map (fun _ -> iStart, (Option.get segmentInterpolateF) iStart seg) s, 
                                    TimePoint.map (fun _ -> iEnd, (Option.get segmentInterpolateF) iEnd seg) e) seg)
                    | TimeSegment.Internal seg -> 
                        Some(seg)
                    | TimeSegment.External seg -> 
                        None
                    | TimeSegment.OverlapStart (dt,seg) ->
                        Some (TimeSegment.map (fun (s,e) -> TimePoint.map (fun _ -> dt, (Option.get segmentInterpolateF) dt seg) s, e) seg)
                    | TimeSegment.OverlapEnd (dt,seg) ->
                        Some (TimeSegment.map (fun (s,e) -> s, TimePoint.map (fun _ -> dt, (Option.get segmentInterpolateF) dt seg) e) seg))
        { Type = line.Type; Segments = segments}

    let append (line1:T<float<'u>>) (line2:T<float<'u>>) =
        //checkLineType "line1" LineType.ContinuousSegments line1
        //checkLineType "line2" LineType.ContinuousSegments line2
        match startTime line1, startTime line2 with
        | Some startTime, Some endTime ->             
            let interval = Interval.Time.make(startTime, endTime)
            let line1Slice = slice (Some TimeSegment.interpolateValue) interval line1
            let segments = Array.append line1Slice.Segments line2.Segments
            { Type = line1.Type; Segments = segments}
        | None, Some _ ->
            line2
        | _ ->
            line1

    let isPointOnLine point line =
        //checkLineType "line" LineType.ContinuousSegments line
        match range line, TimePoint.value point with
        | Some interval, Some value ->
            match tryFindValue (Some TimeSegment.interpolateValue) (TimePoint.time point) line with
            | Some lineValue -> value = lineValue
            | None -> false                        
        | _ -> false
