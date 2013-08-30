namespace FSharp.Enterprise

#if INTERACTIVE 
open FSharp.Enterprise
#endif

open System

module TimeSegment =
    
    open FSharpx
    open OptionOperators

    type T<'v> =
        | Instantaneous of TimePoint.T<'v>
        | Discrete of Interval.T<DateTimeOffset> * 'v option
        | Continuous of TimePoint.T<'v> * TimePoint.T<'v>

    let makeInstantaneous p =
        Instantaneous p

    let emptyInstantaneous t =
        Instantaneous (TimePoint.empty t)

    let makeDiscrete (interval,value) =
        Discrete (interval,value)

    let emptyDiscrete interval =
        Discrete (interval,None)

    let makeContinuous (p1,p2) =
        Continuous (p1,p2)

    let emptyContinuous interval =
        Continuous (Interval.left interval |> TimePoint.empty, Interval.right interval |> TimePoint.empty)

    let startPoint = function
        | Instantaneous p -> p
        | Discrete (interval,value) -> TimePoint.make(Interval.left interval,value) 
        | Continuous (p,_) -> p

    let endPoint = function
        | Instantaneous p -> p
        | Discrete (interval,value) -> TimePoint.make(Interval.right interval,value) 
        | Continuous (_,p) -> p      

    let startTime = function
        | Instantaneous p -> p.Time
        | Discrete (interval,_) -> Interval.left interval 
        | Continuous (p,_) -> p.Time

    let startValue = function
        | Instantaneous p -> p.Value
        | Discrete (_,value) -> value 
        | Continuous (p,_) -> p.Value

    let endTime = function
        | Instantaneous p -> p.Time
        | Discrete (interval,_) -> Interval.right interval 
        | Continuous (_,p) -> p.Time

    let endValue = function
        | Instantaneous p -> p.Value
        | Discrete (_,value) -> value 
        | Continuous (_,p) -> p.Value

    let range = function
        | Instantaneous p -> Interval.Time.make(p.Time,p.Time)
        | Discrete (inverval,_) -> inverval 
        | Continuous (p1,p2) -> Interval.Time.make(p1.Time,p2.Time)

    let domain = function
        | Instantaneous p -> Interval.Value.make(p.Value,p.Value)
        | Discrete (_,value) -> Interval.Value.make(value,value) 
        | Continuous (p1,p2) -> Interval.Value.make(p1.Value,p2.Value)

    let isTimeInRange intervalType t s =
        Interval.Time.isIn intervalType t (range s) 

    let isValueInDomain intervalType value s = 
        Interval.Value.isIn intervalType (domain s) value

    let isFlat s =
        startValue s = endValue s

    let deltaTime s = 
        Interval.Time.delta (range s) 

    let duration timeUnitF s = 
        deltaTime s |> timeUnitF

    let apply op (s1:T<'a>) (s2:T<'b>) = 
        match s1, s2 with
        | Instantaneous p1, Instantaneous p2 when p1.Time = p2.Time -> 
                makeInstantaneous <| TimePoint.make(p1.Time, op p1.Value p2.Value)
        | Discrete (interval, v1), Discrete(interval2, v2) when interval = interval2 ->
                makeDiscrete(interval, op v1 v2)
        | Continuous (p1, p1'), Continuous (p2, p2') when p1.Time = p2.Time && p1'.Time = p2'.Time -> 
                makeContinuous (TimePoint.make(p1.Time, op p1.Value p2.Value), TimePoint.make(p1'.Time, op p1'.Value p2'.Value))
        | _ -> invalidArg "s2" "Time segments must be consistent with respect to time"
           
    let inline map (f: TimePoint.T<'v> * TimePoint.T<'v> -> TimePoint.T<'u> * TimePoint.T<'u>) (s:T<'v>) =
        let p1,p2 = f (startPoint s, endPoint s)
        match s with
        | Instantaneous _ -> makeInstantaneous p1
        | Discrete _ -> makeDiscrete (Interval.Time.make(p1.Time,p2.Time), p1.Value) 
        | Continuous _ -> makeContinuous (p1,p2)

    let inline mapValue f = function
        | Instantaneous p -> makeInstantaneous (TimePoint.mapValue f p)
        | Discrete (interval, value) as s -> makeDiscrete (interval, (TimePoint.mapValue f (startPoint s)).Value) 
        | Continuous _ as s-> makeContinuous (TimePoint.mapValue f (startPoint s), TimePoint.mapValue f (endPoint s))

    let fold<'v,'State> (f : 'State -> TimePoint.T<'v> * TimePoint.T<'v> -> 'State) (state: 'State) (s:T<'v>) =
        let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
        f.Invoke(state,(startPoint s, endPoint s))

    let interpolateValue<[<Measure>]'u> (time:DateTimeOffset) (s:T<float<'u>>) =
        match startValue s, endValue s with
        | Some y0, Some y1 ->
            let startX = float (startTime s).UtcTicks
            let endX = float (endTime s).UtcTicks
            let x = float time.UtcTicks
            let y = Math.Interpolation.linear x startX y0 endX y1
            Some y
        | _ -> None 

    let interpolateTime value (s:T<float<'u>>) = 
        if isValueInDomain IntervalType.T.Closed (Some value) s then
            if isFlat s then
                Some (startTime s)
            else
                let startX = float (startTime s).UtcTicks
                let endX = float (endTime s).UtcTicks
                let y0 = Option.get (startValue s)
                let y1 = Option.get (endValue s) 
                let x = Math.Interpolation.linear value y0 startX y1 endX
                Some (DateTimeOffset(int64 x, TimeSpan.Zero))
        else
            None                     

    let intersection<[<Measure>]'u> (s1:T<float<'u>>) (s2:T<float<'u>>) : TimePoint.T<float<'u>> option=
        match startValue s1, endValue s1, startValue s2, endValue s2 with
        | Some y0, Some y1, Some y2, Some y3 ->
            let x0 = float (startTime s1).UtcTicks
            let x1 = float (endTime s1).UtcTicks
            let x2 = float (startTime s2).UtcTicks
            let x3 = float (endTime s2).UtcTicks
            Math.intersection x0 (float y0) x1 (float y1) x2 (float y2) x3 (float y3)
            |> Option.getOrElseWith None (fun (t,v) ->
                let time = DateTimeOffset(int64 t, (startTime s1).Offset)
                let value = LanguagePrimitives.FloatWithMeasure<'u> v 
                Some(TimePoint.make(time, Some value)))
        | _ -> None
         
    let tryFindValue interpolateF intervalType (t:DateTimeOffset) s =    
        let interpolate = function
            | Instantaneous p -> p.Value
            | Discrete (_,value) -> value 
            | Continuous (p1,p2) -> (Option.get interpolateF) t s
        if isTimeInRange intervalType t s 
        then interpolate s
        else None

    let (|OverlapStart|OverlapEnd|Overlap|Internal|External|) (interval,segment) =
         let iStart, iEnd = Interval.left interval, Interval.right interval
         let segStart, segEnd = startTime segment, endTime segment
         if segEnd <= iStart || segStart >= iEnd
         then External segment
         elif segStart >= iStart && segEnd <= iEnd
         then Internal segment
         elif segStart < iStart && segEnd <= iEnd
         then OverlapStart (iStart,segment)
         elif segStart >= iStart && segEnd > iEnd
         then OverlapEnd (iEnd, segment)
         else Overlap (iStart, iEnd, segment)

    /// Returns true if both the start and end value of the segment is zero.    
    let inline isZero s =
        startValue s ?<= LanguagePrimitives.GenericZero && endValue s ?<= LanguagePrimitives.GenericZero

    /// Returns true if either the start or end value of the segment is non zero.    
    let inline isNonZero s =
        startValue s ?> LanguagePrimitives.GenericZero || endValue s ?> LanguagePrimitives.GenericZero

    /// Returns true if either the start or end value of the segment is none.    
    let isNone s =
        startValue s = None || endValue s = None

    /// Returns true if either the start or end value of the segment is below the given value.
    let isBelow value s =
        startValue s ?< value || endValue s ?< value

    /// Returns true if either the start or end value of the segment is above the given value.
    let isAbove value s =
        startValue s ?> value || endValue s ?> value

    let volume timeUnitF (s:T<float<'u>>) =
        Option.maybe {
            let! startValue = startValue s
            let! endValue = endValue s
            let duration = duration timeUnitF s 
            return (startValue + endValue) / 2.0 * duration
        }

