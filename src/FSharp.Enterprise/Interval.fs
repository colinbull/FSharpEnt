namespace FSharp.Enterprise

open System

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

    let isIn t n (interval) = 
        (order (<=) >> contains (>=) (>) (<=) (<) n t) interval 

    let map f (interval:T<_>) : T<_> = 
        make <| f (left interval, right interval) 

    let merge i1 i2 =
        make(min (left i1) (left i2), max (right i1) (right i2))

    /// Returns true if the intervals intersect, otherwise false.
    let inline intersects i1 i2 =
        let a,b = left i1, right i1
        let c,d = left i2, right i2 
        (max a b > min c d) && (min a b < max c d)

    /// Returns <0 if there is no overlap, 0 if touching and >0 if intersecting.
    let inline overlaps i1 i2 =
        let a,b = left i1, right i1
        let c,d = left i2, right i2 
        min (max a b) (max c d) - max (min c d) (min a b)
   
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
        
        let delta (interval:T) = 
            (right interval) - (left interval)

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

        let getTimes intervalType leftF rightF step (interval:T<DateTimeOffset>) =
            let startTime = interval |> left |> leftF
            let endTime = interval |> right |> rightF
            if startTime > right interval || endTime < left interval then
                Seq.empty
            else
                make(startTime,endTime) |> toSeq intervalType step
                
        /// Returns the times of the days that fall within the interval.
        let getDayTimes intervalType (interval:T<DateTimeOffset>) =
            getTimes intervalType DateTimeOffset.ceilDay DateTimeOffset.floorDay (TimeSpan.FromDays(1.0)) interval        
                                
        /// Returns the times of the halfhours that fall within the interval.
        let getHalfhourTimes intervalType (interval:T<DateTimeOffset>) =
            getTimes intervalType DateTimeOffset.ceilHalfhour DateTimeOffset.floorHalfhour (TimeSpan.FromMinutes(30.0)) interval

        /// Returns the times of the minutes that fall within the interval.
        let getMinuteTimes intervalType (interval:T<DateTimeOffset>) = 
            getTimes intervalType DateTimeOffset.ceilMinute DateTimeOffset.floorMinute (TimeSpan.FromMinutes(1.0)) interval
        
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

