namespace FSharp.Enterprise

module Geometry =

    open System
    open FSharp.Enterprise
    open FSharp.Enterprise.DateTimeOffset

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

        /// Represents an interval between two options of float.
        module Value =

            open FSharp.Enterprise.OptionOperators

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
   
        type Any<'u> = { Start:TimePoint.T<'u>; End:TimePoint.T<'u> }
   
        type Float<[<Measure>]'u> = Any<float<'u>>

        let make (p1,p2) = { Start = p1; End = p2 }

        let empty interval = 
            make(
                interval |> Interval.left |> TimePoint.empty, 
                interval |> Interval.right |> TimePoint.empty)
   
        let makeFlat(interval,value) =
            make(
                TimePoint.make(Interval.left interval, value), 
                TimePoint.make(Interval.right interval, value))

        let startPoint lineSegment = 
            lineSegment.Start
   
        let endPoint lineSegment = 
            lineSegment.End         
   
        let startTime lineSegment = 
            (startPoint >> TimePoint.time) lineSegment
   
        let startValue lineSegment = 
            (startPoint >> TimePoint.value) lineSegment
   
        let endTime lineSegment = 
            (endPoint >> TimePoint.time) lineSegment
   
        let endValue lineSegment = 
            (endPoint >> TimePoint.value) lineSegment
   
        let range lineSegment = 
            Interval.make(startTime lineSegment,endTime lineSegment)
   
        let domain lineSegment = 
            Interval.make(startValue lineSegment,endValue lineSegment)
   
        let isTimeInRange intervalType time lineSegment =
            range lineSegment
            |> Interval.Time.isIn intervalType time
   
        let isValueInDomain intervalType value lineSegment = 
            Interval.Value.isIn intervalType (domain lineSegment) value

        let isVanishinglySmall lineSegment = 
            (endTime lineSegment) = (startTime lineSegment)
            
        let deltaTime lineSegment = (range >> Interval.Time.delta) lineSegment
           
        let inline map (f: TimePoint.T<'v> * TimePoint.T<'v> -> TimePoint.T<'u> * TimePoint.T<'u>) (lineSegment:Any<'v>) =
            checkNonNull "lineSegment" lineSegment
            f (startPoint lineSegment, endPoint lineSegment) |> make

        let inline mapValue (f) (lineSegment:Any<'v>) =
            checkNonNull "lineSegment" lineSegment
            make(
                lineSegment |> startPoint |> TimePoint.mapValue f,
                lineSegment |> endPoint |> TimePoint.mapValue f)

        let fold<'v,'State> (f : 'State -> TimePoint.T<'v> * TimePoint.T<'v> -> 'State) (acc: 'State) (lineSegment:Any<'v>) =
            checkNonNull "lineSegment" lineSegment
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable state = acc 
            state <- f.Invoke(state,(startPoint lineSegment, endPoint lineSegment))
            state

        let interpolate<[<Measure>]'u> (time:DateTimeOffset) (s:Float<'u>) : float<'u> option=
            match s.Start.Value, s.End.Value with
            | Some y0, Some y1 ->
                let startX = float s.Start.Time.UtcTicks
                let endX = float s.End.Time.UtcTicks
                let x = float time.UtcTicks
                let y = Math.Interpolation.linear x startX y0 endX y1
                Some y
            | _ -> None 

        let intersection<[<Measure>]'u> (s1:Float<'u>) (s2:Float<'u>) : TimePoint.T<float<'u>> option=
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

        let (|OverlapStart|OverlapEnd|Overlap|Internal|External|) (interval,lineSegment) =
             let iStart, iEnd = Interval.left interval, Interval.right interval
             let segStart, segEnd = startTime lineSegment, endTime lineSegment
             if segEnd <= iStart || segStart >= iEnd
             then External lineSegment
             elif segStart >= iStart && segEnd <= iEnd
             then Internal lineSegment
             elif segStart < iStart && segEnd <= iEnd
             then OverlapStart (iStart,lineSegment)
             elif segStart >= iStart && segEnd > iEnd
             then OverlapEnd (iEnd, lineSegment)
             else Overlap (iStart, iEnd, lineSegment)

    /// Represents a line of line segments.
    module TimeLine =
        
        open FSharpx

        type Any<'v> = 
            TimeSegment.Any<'v> array

        type Float<[<Measure>]'u> = 
            Any<float<'u>>

        let empty : Any<'v> =
            [||] 
    
        let segmentCount (line:Any<'v>) =
            line.Length

        let startSegment line =
            if segmentCount line > 0 
            then Some line.[0]
            else None
            
        let startPoint line =
            line |> startSegment |> Option.map TimeSegment.startPoint

        let endSegment line =
            if segmentCount line > 0 
            then Some line.[(segmentCount line) - 1]
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

        let ofInterval interval =
            [| interval |> TimeSegment.empty |] 

        let toInterval line =
            if segmentCount line > 0
            then Some (Interval.make((line |> startPoint |> Option.get).Time, (line |> endPoint |> Option.get).Time))
            else None
        
        let ofLineSegment lineSegment =
            [| lineSegment |]

        let ofLineSegments lineSegments : Any<_> = 
            lineSegments |> Seq.toArray

        let inline map (f: TimeSegment.Any<'v> -> TimeSegment.Any<'u>) (line:Any<'v>) =
            checkNonNull "line" line
            match toInterval line with
            | Some interval ->
                let res = Array.zeroCreate line.Length
                for i in [0 .. line.Length - 1] do 
                    res.[i] <- f line.[i]
                res
            | None ->
                [||]

        let inline mapValue (f) (line:Any<'v>) : Any<'w> =
            checkNonNull "line" line
            match toInterval line with
            | Some interval ->
                line |> Array.map (TimeSegment.mapValue f)
            | None ->
                [||]

        let fold<'v,'State> (f : 'State -> TimeSegment.Any<'v> -> 'State) (acc: 'State) (line:Any<'v>) =
            checkNonNull "line" line
            let f = OptimizedClosures.FSharpFunc<_,_,_>.Adapt(f)
            let mutable state = acc 
            for segment in line do 
                state <- f.Invoke(state,segment)
            state

        let unzip4 (line:Any<_>) = 
            checkNonNull "line" line
            let res1 = Array.zeroCreate line.Length
            let res2 = Array.zeroCreate line.Length
            let res3 = Array.zeroCreate line.Length
            let res4 = Array.zeroCreate line.Length
            for i in [0 .. line.Length - 1] do
                let segment = line.[i]
                let unpack = function
                    | Some(a,b,c,d) ->  a, b, c, d
                    | _ -> None,None,None,None                
                let sy1,sy2,sy3,sy4 = unpack segment.Start.Value
                let ey1,ey2,ey3,ey4 = unpack segment.End.Value
                res1.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy1), TimePoint.make (segment.End.Time,ey1))
                res2.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy2), TimePoint.make (segment.End.Time,ey2))
                res3.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy3), TimePoint.make (segment.End.Time,ey3))
                res4.[i] <- TimeSegment.make (TimePoint.make (segment.Start.Time,sy4), TimePoint.make (segment.End.Time,ey4))
            res1,res2,res3,res4

        let unzip5 (line:Any<_>) = 
            checkNonNull "line" line
            let res1 = Array.zeroCreate line.Length
            let res2 = Array.zeroCreate line.Length
            let res3 = Array.zeroCreate line.Length
            let res4 = Array.zeroCreate line.Length
            let res5 = Array.zeroCreate line.Length
            for i in [0 .. line.Length - 1] do
                let segment = line.[i]
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
            res1,res2,res3,res4,res5

        let endTimes line =
            line 
            |> fold (fun s lineSegment -> TimeSegment.endTime lineSegment :: s) []
            |> List.rev

        let startTimes line =
            line 
            |> fold (fun s lineSegment -> TimeSegment.startTime lineSegment :: s) []
            |> List.rev

        let allDistinctTimes line =
            let endTimes = line |> endTimes
            line 
            |> startPoint 
            |> Option.getOrElseWith endTimes (fun startPoint -> TimePoint.time startPoint :: endTimes) 

        let tryFindSegment intervalType time line =
            Array.tryFind (TimeSegment.isTimeInRange intervalType time) line

        let intersections line1 line2 =
            [|
                for line1Segment in line1 do
                    for line2Segment in line2 do
                        match TimeSegment.intersection line1Segment line2Segment with
                        | Some point -> yield point
                        | None -> ()
            |]

        module Instantaneous =
            
            open FSharpx.Option

            let make (points:TimePoint.T<'a> seq) =
                points
                |> Seq.map (fun startPoint ->
                    TimeSegment.make(startPoint,startPoint))
                |> Seq.toArray

            let tryFindValue time (line:Any<'a>) =
                tryFindSegment IntervalType.T.Closed time line
                |> Option.map TimeSegment.startValue
                |> Option.concat

            let toSeq timeSpan line =
                toInterval line
                |> Option.getOrElseWith Seq.empty (fun interval ->
                    Interval.Time.toSeq IntervalType.T.Closed timeSpan interval
                    |> Seq.map (fun time -> tryFindValue time line))

            let slice interval line = 
                line
                |> Array.filter (fun segment -> Interval.Time.isIn IntervalType.T.Closed (TimeSegment.startTime segment) interval)


        module Discrete =
            
            let make (points:TimePoint.T<_> seq) =
                points
                |> Seq.pairwise
                |> Seq.map (fun (startPoint,endPoint) ->
                    TimeSegment.make(startPoint,TimePoint.make (TimePoint.time endPoint, TimePoint.value startPoint)))
                |> Seq.toArray

            let tryFindValue segmentIntervalType time (line:Any<'a>) =
                tryFindSegment segmentIntervalType time line
                |> Option.map TimeSegment.startValue
                |> Option.concat

            let toSeq segmentIntervalType timeSpan (line:Any<'a>) =
                toInterval line
                |> Option.getOrElseWith Seq.empty (fun interval ->
                    Interval.Time.toSeq IntervalType.T.Closed timeSpan interval
                    |> Seq.map (fun time -> tryFindValue segmentIntervalType time line))

            let slice interval line = 
                line
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

        module Continuous =

            let make (points:seq<TimePoint.T<_>>) =
                points 
                |> Seq.pairwise 
                |> Seq.map TimeSegment.make 
                |> Seq.toArray

            let tryFindValue segmentIntervalType time (line:Float<'a>) =
                tryFindSegment segmentIntervalType time line
                |> Option.map (TimeSegment.interpolate time)
                |> Option.concat

            let toSeq segmentIntervalType timeSpan (line:Float<'a>) =
                toInterval line
                |> Option.getOrElseWith Seq.empty (fun interval ->
                    Interval.Time.toSeq IntervalType.T.Closed timeSpan interval
                    |> Seq.map (fun time -> tryFindValue segmentIntervalType time line))

            let slice interval line = 
                line
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
