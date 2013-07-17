namespace FSharp.Enterprise

module Geometry =

    open System

    let inline checkNonNull argName arg = 
        match box arg with 
        | null -> nullArg argName 
        | _ -> ()

    module IntervalType =

        type T =
            | LeftClosedRightOpen = 0b0010
            | LeftOpenRightClosed = 0b0001
            | Closed = 0b0011
            | Open = 0b0000
    
        let isLeftClosed t = 
            t &&& T.LeftClosedRightOpen = T.LeftClosedRightOpen
        
        let isRightClosed t = 
            t &&& T.LeftOpenRightClosed = T.LeftOpenRightClosed
        
        let isClosed t = 
            t = T.Closed
        
        let isLeftOpen t = 
            not (isLeftClosed t)
        
        let isRightOpen t = 
            not (isRightClosed t)
        
        let isOpen t = 
            t = T.Open


    module Interval =

        type T<'n> = 'n * 'n

        let left interval = 
            fst interval

        let right interval = 
            snd interval

        let make (left,right) = 
            left,right

        let flip interval = 
            right interval, left interval

        let private contains geF gF leF lF n t interval =
            if IntervalType.isLeftClosed t 
            then geF n (left interval)
            else gF n (left interval)
            &&
            if IntervalType.isRightClosed t  
            then leF n (right interval)
            else lF n (right interval)

        let isOrdered leF interval = 
            leF (left interval) (right interval) 

        let order leF interval = 
            if isOrdered leF interval 
            then interval 
            else flip interval

        let map f (interval:T<_>) : T<_> = 
            make <| f (left interval, right interval) 

        let merge i1 i2 =
            make(min (left i1) (left i2), max (right i1) (right i2))

        /// Represents an interval between two options of float.
        module Value =

            open OptionOperators

            let rightUnboundedValue = Double.PositiveInfinity

            let leftUnboundedValue = Double.NegativeInfinity

            let make (n1:Option<float<'u>>,n2:Option<float<'u>>) = 
                make(n1,n2)

            let makeLeftUnbounded<[<Measure>]'u> rightValue = 
                make(Some (LanguagePrimitives.FloatWithMeasure<'u> leftUnboundedValue), rightValue)

            let makeRightUnbounded<[<Measure>]'u> leftValue = 
                make(leftValue, Some (LanguagePrimitives.FloatWithMeasure<'u> rightUnboundedValue))

            let makeUnbounded<[<Measure>]'u> = 
                make(Some (LanguagePrimitives.FloatWithMeasure<'u> leftUnboundedValue), 
                     Some (LanguagePrimitives.FloatWithMeasure<'u> rightUnboundedValue))

            let empty<[<Measure>]'u> = 
                make(Some (LanguagePrimitives.FloatWithMeasure<'u> 0.),
                     Some (LanguagePrimitives.FloatWithMeasure<'u> 0.))

            let isLeftUnbounded interval = 
                left interval = Some(LanguagePrimitives.FloatWithMeasure<'u> leftUnboundedValue)

            let isRightUnbounded interval = 
                right interval = Some(LanguagePrimitives.FloatWithMeasure<'u> rightUnboundedValue)

            let isUnbounded interval = 
                isLeftUnbounded interval && isRightUnbounded interval       

            let isLeftBounded interval = 
                not (isLeftUnbounded interval)

            let isRightBounded interval = 
                not (isRightUnbounded interval)

            let isBounded interval = 
                isLeftBounded interval && isRightBounded interval

            let order (interval:T<Option<'a>>) = 
                if (left interval).IsNone || (right interval).IsNone 
                then interval
                else order (?<=?) interval

            let isIn t (interval:T<Option<'a>>) n =
                if Option.isNone n then
                    if IntervalType.isClosed t then
                        (left interval).IsNone || (right interval).IsNone
                    elif IntervalType.isLeftClosed t then
                        (left interval).IsNone
                    elif IntervalType.isRightClosed t then
                        (right interval).IsNone
                    else
                        false
                else
                    (order >> contains (?>=?) (?>?) (?<=?) (?<?) n t) interval

            let delta (interval:T<Option<float<'u>>>) = 
                right interval ?-? left interval


        /// Represents an interval between two DateTimeOffsets.
        module Time =
            
            type T = T<DateTimeOffset>
            
            let rightUnboundedTime = DateTimeOffset.MaxValue

            let leftUnboundedTime = DateTimeOffset.MinValue
            
            let make (n1,n2) : T = 
                make(n1,n2)
            
            let makeLeftUnbounded rightTime = 
                make(leftUnboundedTime, rightTime)
            
            let makeRightUnbounded leftTime = 
                make(leftTime, rightUnboundedTime)
            
            let makeUnbounded = 
                make(leftUnboundedTime, rightUnboundedTime)
            
            let empty : T = 
                let d = DateTimeOffset.UtcNow
                make(d, d)
            
            let isLeftUnbounded interval = 
                left interval = leftUnboundedTime
            
            let isRightUnbounded interval = 
                right interval = rightUnboundedTime
            
            let isUnbounded interval = 
                isLeftUnbounded interval && isRightUnbounded interval       
            
            let isLeftBounded interval = 
                not (isLeftUnbounded interval)
            
            let isRightBounded interval = 
                not (isRightUnbounded interval)
            
            let isBounded interval = 
                isLeftBounded interval && isRightBounded interval
            
            let order interval = 
                order (<=) interval
            
            let isIn t n (interval:T) = 
                (order >> contains (>=) (>) (<=) (<) n t) interval 
            
            let delta interval = 
                right interval - left interval

            let toSeq intervalType step (interval:T<DateTimeOffset>) =
                seq {
                    let startTime = left interval
                    let endTime = right interval
                    if IntervalType.isLeftClosed intervalType then yield startTime
                    let time = ref (startTime.Add(step))                    
                    while !time < endTime do
                        yield !time
                        time := (!time).Add(step)
                    if IntervalType.isRightClosed intervalType then yield endTime
                }

            let getTimes intervalType ceilF floorF step (interval:T<DateTimeOffset>) =
                let startTime = interval |> left |> ceilF
                let endTime = interval |> right |> floorF
                if startTime > right interval || endTime < left interval then
                    Seq.empty
                else
                    make(startTime,endTime) |> toSeq intervalType step
                                    
            /// Returns the times of the halfhours that fall within the interval.
            let getHalfhourTimes intervalType (interval:T<DateTimeOffset>) =
                interval
                |> getTimes intervalType DateTimeOffset.ceilHalfhour DateTimeOffset.floorHalfhour (TimeSpan.FromMinutes(30.0))

            /// Returns the times of the halfhours that fall within the interval.
            let getMinuteTimes intervalType (interval:T<DateTimeOffset>) =
                interval
                |> getTimes intervalType DateTimeOffset.ceilMinute DateTimeOffset.floorMinute (TimeSpan.FromMinutes(1.0))
            
            let incr (span:TimeSpan) (interval:T) : T = 
                interval |> map (fun (s,e) -> s.Add(span), e.Add(span)) 

            /// Returns an interval with the left floored to a halfhour value and right ceiled to a halfhour value.
            let toHalfhour (interval:T) =
                make(left interval |> DateTimeOffset.floorHalfhour, right interval |> DateTimeOffset.ceilHalfhour)

            let toHalfhourIntervals interval = 
                interval 
                |> getHalfhourTimes IntervalType.T.Closed
                |> Seq.pairwise
                |> Seq.map make

    /// Represents a value at a point in time. 
    module TimePoint =

        type T<'v> = { 
            Time:DateTimeOffset
            Value:Option<'v> 
        }

        let make (time,value) = { 
            Time = time
            Value = value 
        }
        
        let empty time : T<'v> = 
            make (time, None)
        
        let time point = 
            point.Time
        
        let value point = 
            point.Value
           
        let inline map (f: DateTimeOffset * Option<'v> -> DateTimeOffset * Option<'u>) (point:T<'v>) =
            checkNonNull "point" point
            f (time point, value point) |> make
           
        let inline mapValue f (point:T<'v>) =
            checkNonNull "point" point
            make (time point, value point |> Option.map f)

        let inline mapTime f (point:T<'v>) =
            checkNonNull "point" point
            make (time point |> f, value point)

        let fold<'v,'State> (f : 'State -> DateTimeOffset * Option<'v> -> 'State) (acc: 'State) (point:T<'v>) =
            checkNonNull "point" point
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable state = acc 
            state <- f.Invoke(state,(time point, value point))
            state


    /// Represents a part of a line bounded by two points in time.
    module TimeSegment =
   
        open FSharpx
        open OptionOperators
   
        type T<'u> = { 
            Start:TimePoint.T<'u>
            End:TimePoint.T<'u> 
        }
  
        let make (p1,p2) = { Start = p1; End = p2 }

        let empty interval = 
            make(
                interval |> Interval.left |> TimePoint.empty, 
                interval |> Interval.right |> TimePoint.empty)
   
        let makeFlat(interval,value) =
            make(
                TimePoint.make(Interval.left interval, value), 
                TimePoint.make(Interval.right interval, value))

        let startPoint segment = 
            segment.Start
   
        let endPoint segment = 
            segment.End         
   
        let startTime segment = 
            (startPoint >> TimePoint.time) segment
   
        let startValue segment = 
            (startPoint >> TimePoint.value) segment
   
        let endTime segment = 
            (endPoint >> TimePoint.time) segment
   
        let endValue segment = 
            (endPoint >> TimePoint.value) segment
   
        let range segment = 
            Interval.make(startTime segment,endTime segment)
   
        let domain segment = 
            Interval.make(startValue segment,endValue segment)
   
        let isTimeInRange intervalType time segment =
            range segment
            |> Interval.Time.isIn intervalType time
   
        let isValueInDomain intervalType value segment = 
            Interval.Value.isIn intervalType (domain segment) value

        let isVanishinglySmall segment = 
            (endTime segment) = (startTime segment)
            
        let deltaTime segment = (range >> Interval.Time.delta) segment

        let duration timeUnitF segment = (range >> Interval.Time.delta >> timeUnitF) segment
               
        let inline map (f: TimePoint.T<'v> * TimePoint.T<'v> -> TimePoint.T<'u> * TimePoint.T<'u>) (segment:T<'v>) =
            checkNonNull "segment" segment
            f (startPoint segment, endPoint segment) |> make

        let inline mapValue (f) (segment:T<'v>) =
            checkNonNull "segment" segment
            make(
                segment |> startPoint |> TimePoint.mapValue f,
                segment |> endPoint |> TimePoint.mapValue f)

        let fold<'v,'State> (f : 'State -> TimePoint.T<'v> * TimePoint.T<'v> -> 'State) (acc: 'State) (segment:T<'v>) =
            checkNonNull "segment" segment
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable state = acc 
            state <- f.Invoke(state,(startPoint segment, endPoint segment))
            state

        let interpolate<[<Measure>]'u> (time:DateTimeOffset) (s:T<float<'u>>) : float<'u> option=
            match s.Start.Value, s.End.Value with
            | Some y0, Some y1 ->
                let startX = float s.Start.Time.UtcTicks
                let endX = float s.End.Time.UtcTicks
                let x = float time.UtcTicks
                let y = Math.Interpolation.linear x startX y0 endX y1
                Some y
            | _ -> None 

        let intersection<[<Measure>]'u> (s1:T<float<'u>>) (s2:T<float<'u>>) : TimePoint.T<float<'u>> option=
            match s1.Start.Value, s1.End.Value, s2.Start.Value, s2.End.Value with
            | Some y0, Some y1, Some y2, Some y3 ->
                let x0 = float s1.Start.Time.UtcTicks
                let x1 = float s1.End.Time.UtcTicks
                let x2 = float s2.Start.Time.UtcTicks
                let x3 = float s2.End.Time.UtcTicks
                Math.intersection x0 (float y0) x1 (float y1) x2 (float y2) x3 (float y3)
                |> Option.getOrElseWith None (fun (t,v) ->
                    let time = DateTimeOffset(int64 t, s1.Start.Time.Offset)
                    let value = LanguagePrimitives.FloatWithMeasure<'u> v 
                    Some(TimePoint.make(time, Some value)))
            | _ -> None

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
        let inline isZero segment =
            startValue segment ?<= LanguagePrimitives.GenericZero && endValue segment ?<= LanguagePrimitives.GenericZero

        /// Returns true if either the start or end value of the segment is non zero.    
        let inline isNonZero segment =
            startValue segment ?> LanguagePrimitives.GenericZero || endValue segment ?> LanguagePrimitives.GenericZero

        /// Returns true if either the start or end value of the segment is none.    
        let isNone segment =
            startValue segment = None || endValue segment = None

        /// Returns true if either the start or end value of the segment is below the given value.
        let isBelow value segment =
            startValue segment ?< value || endValue segment ?< value

        /// Returns true if either the start or end value of the segment is above the given value.
        let isAbove value segment =
            startValue segment ?> value || endValue segment ?> value

        let volume timeUnitF (segment:T<float<'u>>) =
            Option.maybe {
                let! startValue = startValue segment
                let! endValue = endValue segment
                let duration = duration timeUnitF segment 
                return (startValue + endValue) / 2.0 * duration
            }

    /// Represents a line of line segments.
    module TimeLine =
        
        open FSharpx

        type LineType =
            | InstantaneousSegments
            | DiscreteSegments
            | ContinuousSegments

        type T<'v> = {
            Type : LineType
            Segments : TimeSegment.T<'v> array
        }

        let inline checkLineType argName ``type`` line = 
            if line.Type <> ``type`` then
                let message = sprintf "invalid line type: expected %A but was %A" ``type`` line.Type
                invalidArg argName message
                                
        let make ``type`` points =
            let segments =
                match ``type`` with
                | InstantaneousSegments ->
                    points 
                    |> Seq.map (fun point -> TimeSegment.make(point,point)) 
                    |> Seq.toArray
                | DiscreteSegments ->
                    Seq.pairwise points
                    |> Seq.map (fun (startPoint,endPoint) -> TimeSegment.make(startPoint,TimePoint.make (TimePoint.time endPoint, TimePoint.value startPoint)))
                    |> Seq.toArray
                | ContinuousSegments -> 
                    Seq.pairwise points
                    |> Seq.map TimeSegment.make 
                    |> Seq.toArray
            { Type = ``type``; Segments = segments }       

        let makeInstantaneous points = make LineType.InstantaneousSegments points
        let makeDiscrete points = make LineType.DiscreteSegments points
        let makeContinuous points = make LineType.ContinuousSegments points

        let empty ``type`` : T<'v> =
            { Type = ``type``; Segments = [||] }

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
            { Type = lineType; Segments = [| TimeSegment.empty interval |] }

        let toInterval line =
            if segmentCount line > 0
            then Some (Interval.make((line |> startPoint |> Option.get).Time, (line |> endPoint |> Option.get).Time))
            else None

        let inline map (f: TimeSegment.T<'v> -> TimeSegment.T<'u>) (line:T<'v>) =
            let segments = Array.map f line.Segments 
            { Type = line.Type; Segments = segments }

        let inline mapValue f (line:T<'v>) : T<'w> =
            let segments = Array.map (TimeSegment.mapValue f) line.Segments 
            { Type = line.Type; Segments = segments }

        let fold<'v,'State> f (acc:'State) (line:T<'v>) =
            Array.fold f acc line.Segments 

        let exists (f: TimeSegment.T<'v> -> bool) (line:T<'v>) =
            Array.exists f line.Segments

        let forall (f: TimeSegment.T<'v> -> bool) (line:T<'v>) =
            Array.forall f line.Segments

        let unzip4 (line:T<_>) = 
            checkNonNull "line" line
            let res1 = Array.zeroCreate line.Segments.Length
            let res2 = Array.zeroCreate line.Segments.Length
            let res3 = Array.zeroCreate line.Segments.Length
            let res4 = Array.zeroCreate line.Segments.Length
            for i in [0 .. line.Segments.Length - 1] do
                let segment = line.Segments.[i]
                let unpack = function
                    | Some(a,b,c,d) ->  a, b, c, d
                    | _ -> None,None,None,None                
                let sy1,sy2,sy3,sy4 = unpack segment.Start.Value
                let ey1,ey2,ey3,ey4 = unpack segment.End.Value
                res1.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy1), TimePoint.make (segment.End.Time,ey1))
                res2.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy2), TimePoint.make (segment.End.Time,ey2))
                res3.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy3), TimePoint.make (segment.End.Time,ey3))
                res4.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy4), TimePoint.make (segment.End.Time,ey4))
            { Type = line.Type; Segments = res1 },
            { Type = line.Type; Segments = res2 },
            { Type = line.Type; Segments = res3 },
            { Type = line.Type; Segments = res4 }

        let unzip5 (line:T<_>) = 
            checkNonNull "line" line
            let res1 = Array.zeroCreate line.Segments.Length
            let res2 = Array.zeroCreate line.Segments.Length
            let res3 = Array.zeroCreate line.Segments.Length
            let res4 = Array.zeroCreate line.Segments.Length
            let res5 = Array.zeroCreate line.Segments.Length
            for i in [0 .. line.Segments.Length - 1] do
                let segment = line.Segments.[i]
                let unpack = function
                    | Some(a,b,c,d,e) ->  a, b, c, d, e
                    | _ -> None,None,None,None,None                
                let sy1,sy2,sy3,sy4,sy5 = unpack segment.Start.Value
                let ey1,ey2,ey3,ey4,ey5 = unpack segment.End.Value
                res1.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy1), TimePoint.make (segment.End.Time,ey1))
                res2.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy2), TimePoint.make (segment.End.Time,ey2))
                res3.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy3), TimePoint.make (segment.End.Time,ey3))
                res4.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy4), TimePoint.make (segment.End.Time,ey4))
                res5.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy5), TimePoint.make (segment.End.Time,ey5))
            { Type = line.Type; Segments = res1 },
            { Type = line.Type; Segments = res2 },
            { Type = line.Type; Segments = res3 },
            { Type = line.Type; Segments = res4 },
            { Type = line.Type; Segments = res5 }

        let endTimes line =
            line 
            |> fold (fun s segment -> TimeSegment.endTime segment :: s) []
            |> List.rev

        let startTimes line =
            line 
            |> fold (fun s segment -> TimeSegment.startTime segment :: s) []
            |> List.rev

        let allDistinctTimes line =
            let endTimes = line |> endTimes
            line 
            |> startPoint 
            |> Option.getOrElseWith endTimes (fun startPoint -> TimePoint.time startPoint :: endTimes) 

        let tryFindSegment intervalType time line =
            Array.tryFind (TimeSegment.isTimeInRange intervalType time) line.Segments

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

        module Instantaneous =
            
            open FSharpx.Option

            let tryFindValue time (line:T<'v>) =
                checkLineType "line" LineType.InstantaneousSegments line
                tryFindSegment IntervalType.T.Closed time line
                |> Option.map TimeSegment.startValue
                |> Option.concat

            let toSeq timeSpan line =
                checkLineType "line" LineType.InstantaneousSegments line
                toInterval line
                |> Option.getOrElseWith Seq.empty (fun interval ->
                    Interval.Time.toSeq IntervalType.T.Closed timeSpan interval
                    |> Seq.map (fun time -> tryFindValue time line))

            let slice interval line = 
                checkLineType "line" LineType.InstantaneousSegments line                
                let segments = Array.filter (fun segment -> Interval.Time.isIn IntervalType.T.Closed (TimeSegment.startTime segment) interval) line.Segments
                { Type = line.Type; Segments = segments }

        module Discrete =
            
            let tryFindValue segmentIntervalType time (line:T<'v>) =
                checkLineType "line" LineType.DiscreteSegments line
                tryFindSegment segmentIntervalType time line
                |> Option.map TimeSegment.startValue
                |> Option.concat

            let toSeq segmentIntervalType timeSpan (line:T<'v>) =
                checkLineType "line" LineType.DiscreteSegments line
                toInterval line
                |> Option.getOrElseWith Seq.empty (fun interval ->
                    Interval.Time.toSeq IntervalType.T.Closed timeSpan interval
                    |> Seq.map (fun time -> tryFindValue segmentIntervalType time line))

            let slice interval line = 
                checkLineType "line" LineType.DiscreteSegments line
                let segments =
                    line.Segments
                    |> Array.choose (fun seg -> 
                                       match (interval, seg) with
                                       | TimeSegment.Overlap (iStart, iEnd, seg) ->
                                            Some <| TimeSegment.makeFlat(interval,seg.Start.Value)
                                       | TimeSegment.Internal seg -> 
                                            Some(seg)
                                       | TimeSegment.External seg -> 
                                            None
                                       | TimeSegment.OverlapStart (dt,seg) ->
                                            Some <| TimeSegment.map (fun (s,e) -> TimePoint.mapTime (fun _ -> dt) s, e) seg
                                       | TimeSegment.OverlapEnd (dt,seg) ->
                                            Some <| TimeSegment.map (fun (s,e) -> s, TimePoint.map (fun (_,_) -> dt,s.Value) e) seg
                                    )
                { Type = line.Type; Segments = segments }

        module Continuous =

            let tryFindValue segmentIntervalType time (line:T<float<'u>>) =                
                checkLineType "line" LineType.ContinuousSegments line
                tryFindSegment segmentIntervalType time line
                |> Option.map (TimeSegment.interpolate time)
                |> Option.concat

            let toSeq segmentIntervalType timeSpan (line:T<float<'u>>) =
                checkLineType "line" LineType.ContinuousSegments line
                toInterval line
                |> Option.getOrElseWith Seq.empty (fun interval ->
                    Interval.Time.toSeq IntervalType.T.Closed timeSpan interval
                    |> Seq.map (fun time -> tryFindValue segmentIntervalType time line))

            let slice interval line = 
                checkLineType "line" LineType.ContinuousSegments line
                let segments =
                    line.Segments
                    |> Array.choose (fun seg -> 
                                       match (interval, seg) with
                                       | TimeSegment.Overlap (iStart, iEnd, seg) ->
                                            Some <| TimeSegment.map (fun (s,e) -> 
                                                        TimePoint.map (fun _ -> iStart, TimeSegment.interpolate iStart seg) s, 
                                                        TimePoint.map (fun _ -> iEnd, TimeSegment.interpolate iEnd seg) e) seg
                                       | TimeSegment.Internal seg -> 
                                            Some(seg)
                                       | TimeSegment.External seg -> 
                                            None
                                       | TimeSegment.OverlapStart (dt,seg) ->
                                            Some <| TimeSegment.map (fun (s,e) -> TimePoint.map (fun _ -> dt, TimeSegment.interpolate dt seg) s, e) seg
                                       | TimeSegment.OverlapEnd (dt,seg) ->
                                            Some <| TimeSegment.map (fun (s,e) -> s, TimePoint.map (fun _ -> dt, TimeSegment.interpolate dt seg) e) seg
                                    )
                { Type = line.Type; Segments = segments }
