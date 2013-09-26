namespace FSharp.Enterprise

module TimeLine =
    
    open FSharp.Enterprise.OptionOperators
    open FSharpx
    open FSharpx.Option
    open System    
    open System.Runtime.Serialization     

    type T<'v> = 
        | TimeLine of Line.T<DateTimeOffset,'v option>
        with
            member x.Type = match x with | TimeLine l -> l.Type
            member x.Segments = match x with | TimeLine l -> l.Segments

    let makeInstantaneous points = TimeLine (Line.makeInstantaneous (points |> Seq.map (fun (TimePoint.TimePoint p) -> p)))
    let makeDiscrete intervalType points = TimeLine (Line.makeDiscrete intervalType (points |> Seq.map (fun (TimePoint.TimePoint p) -> p)))
    let makeContinuous points = TimeLine (Line.makeContinuous (points |> Seq.map (fun (TimePoint.TimePoint p) -> p)))

    let makeFromSegments lineType segments = TimeLine (Line.makeFromSegments lineType (Array.map TimeSegment.toSegment segments))

    let emptyInstantaneous () = TimeLine (Line.emptyInstantaneous ())
    let emptyDiscrete intervalType = TimeLine (Line.emptyDiscrete intervalType)
    let emptyContinuous () = TimeLine (Line.emptyContinuous ())
    let isEmpty (TimeLine l) = Line.isEmpty l
    
    let startSegment (TimeLine l) = TimeSegment.TimeSegment <!> Line.startSegment l
    let startPoint (TimeLine l) = TimePoint.TimePoint <!> Line.startPoint l
    let startTime (TimeLine l) = Line.startX l
    let startTimes (TimeLine l) = Line.startXs l
    let startValue (TimeLine l) = Line.startY l

    let endSegment (TimeLine l) = TimeSegment.TimeSegment <!> Line.endSegment l            
    let endPoint (TimeLine l) = TimePoint.TimePoint <!> Line.endPoint l
    let endTime (TimeLine l) = Line.endX l
    let endTimes (TimeLine l) = Line.endXs l
    let endValue (TimeLine l) = Line.endY l

    let range (TimeLine l) = Line.range l
    let times (TimeLine l) = Line.xs l

    let map f (TimeLine l) = TimeLine (Line.map (TimeSegment.unlift f) l)
    let map2 f (TimeLine l1) (TimeLine l2) = TimeLine (Line.map2 f l1 l2)
    let mapValue f (TimeLine l) = TimeLine (Line.mapY f l)

    let fold f state (TimeLine l) = Line.fold (fun state s -> f state (TimeSegment.TimeSegment s)) state l
    let inline sumBy f (TimeLine l) = Line.sumBy (TimeSegment.TimeSegment >> f) l

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let exists pred (TimeLine l) = Line.exists pred l

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forall pred (TimeLine l) = Line.forall pred l

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let existsPoint pred (TimeLine l) = Line.existsPoint pred l

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forallPoint pred (TimeLine l) = Line.forallPoint pred l

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let existsValue pred (TimeLine l) = Line.existsY pred l

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forallValue pred (TimeLine l) = Line.forallY pred l

    let tryPick chooser (TimeLine l) = Line.tryPick chooser l

    let tryFindSegment time (TimeLine l) = Line.tryFindSegment time l        

    let intersections (TimeLine l1) (TimeLine l2) =
        [|
            for s1 in l1.Segments do
                for s2 in l2.Segments do
                    match TimeSegment.intersection (TimeSegment.TimeSegment s1) (TimeSegment.TimeSegment s2) with
                    | Some point -> yield point
                    | None -> ()
        |]

    let volume timeUnitF (TimeLine l) =
        Array.choose (TimeSegment.TimeSegment >> TimeSegment.volume timeUnitF) l.Segments 
        |> Array.sum

    let tryFindValue segmentInterpolateF time (TimeLine l) =            
        Array.tryPick (TimeSegment.TimeSegment >> TimeSegment.tryFindValue segmentInterpolateF (Line.segmentIntervalType l) time) l.Segments

    let tryFindValues segmentInterpolateF time (TimeLine l) =
        Array.choose (TimeSegment.TimeSegment >> TimeSegment.tryFindValue segmentInterpolateF (Line.segmentIntervalType l) time) l.Segments

    /// Returns true if the predicate applied to the value at time t returns true, otherwise false.
    let isValueAtTime segmentInterpolateF t p line : bool =
        (tryFindValue segmentInterpolateF t >> p) line

    let toSeq segmentInterpolateF timeSpan line =
        range line
        |> Option.getOrElseWith Seq.empty (fun interval ->
            Interval.Time.toSeq IntervalType.T.Closed timeSpan interval
            |> Seq.map (fun time -> tryFindValue segmentInterpolateF time line))

    let toPoints (TimeLine l) = Line.toPoints l |> List.map TimePoint.TimePoint

    let slice segmentInterpolateF interval (TimeLine l) =
        let f = Option.map (fun f -> (fun t s -> f t (TimeSegment.TimeSegment s))) segmentInterpolateF
        TimeLine (Line.slice f interval l)

    let append segmentInterpolateF (TimeLine l1) (TimeLine l2) = 
        let f = Option.map (fun f -> (fun t s -> f t (TimeSegment.TimeSegment s))) segmentInterpolateF
        TimeLine (Line.append f l1 l2)

    let isPointOnLine point line =
        //checkLineType "line" LineType.ContinuousSegments line
        match range line, TimePoint.value point with
        | Some interval, Some value ->
            match tryFindValue (Some TimeSegment.interpolateValue) (TimePoint.time point) line with
            | Some lineValue -> value = lineValue
            | None -> false                        
        | _ -> false

    let ofInterval lineType interval  =
        let segments =
            match lineType with
            | Line.InstantaneousSegments -> [| TimeSegment.emptyInstantaneous (Interval.left interval) |]
            | Line.DiscreteSegments _ -> [| TimeSegment.emptyDiscrete interval |]
            | Line.ContinuousSegments _ -> [| TimeSegment.emptyContinuous interval |]
        makeFromSegments lineType segments
