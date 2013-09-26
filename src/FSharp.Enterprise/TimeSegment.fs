namespace FSharp.Enterprise

#if INTERACTIVE 
open FSharp.Enterprise
#endif

open System

module TimeSegment =

    open FSharpx
    open System

    type T<'v> = | TimeSegment of Segment.T<DateTimeOffset,'v option>
    
    let makeInstantaneous (TimePoint.TimePoint p) = TimeSegment (Segment.makeInstantaneous p)
    let makeDiscrete (i,v) = TimeSegment (Segment.makeDiscrete (i,v))
    let makeContinuous (TimePoint.TimePoint p1,TimePoint.TimePoint p2) = TimeSegment (Segment.makeContinuous (p1,p2))

    let emptyInstantaneous t = makeInstantaneous (TimePoint.empty t)
    let emptyDiscrete interval = makeDiscrete (interval,None)
    let emptyContinuous interval = makeContinuous (TimePoint.empty (Interval.left interval), TimePoint.empty (Interval.right interval))

    let startPoint (TimeSegment s) = Segment.startPoint s
    let startTime (TimeSegment s) = Segment.startX s
    let startValue (TimeSegment s) = Segment.startY s

    let endPoint (TimeSegment s) = Segment.endPoint s
    let endTime (TimeSegment s) = Segment.endX s
    let endValue (TimeSegment s) = Segment.endY s

    let range (TimeSegment s) = Segment.range s
    let isInRange intervalType t s = Interval.Time.isIn intervalType t (range s) 
    let domain (TimeSegment s) = Segment.domain s
    let isInDomain intervalType v s = Interval.Value.isIn intervalType (domain s) v
    let isFlatDomain (TimeSegment s) = Segment.isFlat s

    let deltaTime s = Interval.Time.delta (range s) 
    let duration timeUnitF s = deltaTime s |> timeUnitF

    let lift f (TimeSegment s) = TimeSegment (f s)
    let lift2 f (TimeSegment s1) (TimeSegment s2) = TimeSegment (f s1 s2)
    let toSegment (TimeSegment s) = s
    let unlift f = (TimeSegment >> f >> toSegment)

    let mapTime f (TimeSegment s) = TimeSegment (Segment.mapX f s)
    let mapValue f (TimeSegment s) = TimeSegment (Segment.mapY f s)
    let mapValue2 f (TimeSegment s1) (TimeSegment s2) = Segment.mapY2 f s1 s2
   
    let interpolateValue<[<Measure>]'u> (time:DateTimeOffset) (s:T<float<'u>>) =
        match startValue s, endValue s with
        | Some y0, Some y1 ->
            let startX = float (startTime s).Ticks
            let endX = float (endTime s).Ticks
            let x = float time.Ticks
            let y = Math.Interpolation.linear x startX y0 endX y1
            Some y
        | _ -> None 

    let interpolateTime value (s:T<float<'u>>) = 
        if isInDomain IntervalType.T.Closed (Some value) s then
            if isFlatDomain s then
                Some (startTime s)
            else
                let startX = float (startTime s).Ticks
                let endX = float (endTime s).Ticks
                let y0 = Option.get (startValue s)
                let y1 = Option.get (endValue s) 
                let x = Math.Interpolation.linear value y0 startX y1 endX
                Some (DateTimeOffset(int64 x, (startTime s).Offset))
        else
            None                     

    let intersection<[<Measure>]'u> (s1:T<float<'u>>) (s2:T<float<'u>>) : TimePoint.T<float<'u>> option=
        match startValue s1, endValue s1, startValue s2, endValue s2 with
        | Some y0, Some y1, Some y2, Some y3 ->
            let x0 = float (startTime s1).Ticks
            let x1 = float (endTime s1).Ticks
            let x2 = float (startTime s2).Ticks
            let x3 = float (endTime s2).Ticks
            Math.intersection x0 (float y0) x1 (float y1) x2 (float y2) x3 (float y3)
            |> Option.getOrElseWith None (fun (t,v) ->
                let time = DateTimeOffset(int64 t, (startTime s1).Offset)
                let value = LanguagePrimitives.FloatWithMeasure<'u> v 
                Some(TimePoint.make(time, Some value)))
        | _ -> None
         
    let tryFindValue segmentInterpolateF intervalType (t:DateTimeOffset) ((TimeSegment s) as segment) =   
        let interpolate = function
            | Segment.Instantaneous p -> p.Y
            | Segment.Discrete (_,value) -> value 
            | Segment.Continuous (p1,p2) -> 
                match segmentInterpolateF with
                | Some f -> f t segment
                | None -> startValue segment
        if isInRange intervalType t segment 
        then interpolate s
        else None

    /// Returns true if the predicate applied to either the start point or the
    /// end point returns true, otherwise false
    let exists pred (TimeSegment s) = Segment.exists pred s

    /// Returns true if the predicate applied to both the start point and the 
    /// end point returns true, otherwise false
    let forall pred (TimeSegment s) = Segment.forall pred s

    /// Returns true if the predicate applied to either the start value or the
    /// end value returns true, otherwise false
    let existsValue pred (TimeSegment s) = Segment.existsY pred s 

    /// Returns true if the predicate applied to both the start value and the
    /// end value returns true, otherwise false
    let forallValue pred s = Segment.forallY pred s

    let volume timeUnitF (s:T<float<'u>>) =
        Option.maybe {
            let! startValue = startValue s
            let! endValue = endValue s
            let duration = duration timeUnitF s 
            return (startValue + endValue) / 2.0 * duration
        }
