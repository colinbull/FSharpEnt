﻿namespace FSharp.Enterprise.Tests

open NUnit.Framework
open FsUnit
open System
open Microsoft.FSharp.Data.UnitSystems.SI
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.Enterprise.OptionOperators
open FSharp.Enterprise.Geometry

module Helper =

    let d0 = DateTimeOffset(2013,5,9,0,0,0,TimeSpan.FromHours(0.0))

    let getPoints (startTime:DateTimeOffset) (timeStep:float<s>) (values:Option<'a> []) =
        [|
            for i in 0 .. values.Length - 1 do
                yield TimePoint.make(startTime.AddMinutes(float i * float timeStep), values.[i])
        |]

              
[<TestFixture; Category("Unit")>]
type ``Given an interval type`` () =

    [<Test>]
    member x.``I can check if it is left closed`` () =
        IntervalType.isLeftClosed IntervalType.T.LeftClosedRightOpen |> should be True
        IntervalType.isLeftClosed IntervalType.T.LeftOpenRightClosed |> should be False
        IntervalType.isLeftClosed IntervalType.T.Closed |> should be True
        IntervalType.isLeftClosed IntervalType.T.Open |> should be False

    [<Test>]
    member x.``I can check if it is right closed`` () =
        IntervalType.isRightClosed IntervalType.T.LeftClosedRightOpen |> should be False
        IntervalType.isRightClosed IntervalType.T.LeftOpenRightClosed |> should be True
        IntervalType.isRightClosed IntervalType.T.Closed |> should be True
        IntervalType.isRightClosed IntervalType.T.Open |> should be False

    [<Test>]
    member x.``I can check if it is closed`` () =
        IntervalType.isClosed IntervalType.T.LeftClosedRightOpen |> should be False
        IntervalType.isClosed IntervalType.T.LeftOpenRightClosed |> should be False
        IntervalType.isClosed IntervalType.T.Closed |> should be True
        IntervalType.isClosed IntervalType.T.Open |> should be False

    [<Test>]
    member x.``I can check if it is left open`` () =
        IntervalType.isLeftOpen IntervalType.T.LeftClosedRightOpen |> should be False
        IntervalType.isLeftOpen IntervalType.T.LeftOpenRightClosed |> should be True
        IntervalType.isLeftOpen IntervalType.T.Closed |> should be False
        IntervalType.isLeftOpen IntervalType.T.Open |> should be True

    [<Test>]
    member x.``I can check if it is right open`` () =
        IntervalType.isRightOpen IntervalType.T.LeftClosedRightOpen |> should be True
        IntervalType.isRightOpen IntervalType.T.LeftOpenRightClosed |> should be False
        IntervalType.isRightOpen IntervalType.T.Closed |> should be False
        IntervalType.isRightOpen IntervalType.T.Open |> should be True

    [<Test>]
    member x.``I can check if it is open`` () =
        IntervalType.isOpen IntervalType.T.LeftClosedRightOpen |> should be False
        IntervalType.isOpen IntervalType.T.LeftOpenRightClosed |> should be False
        IntervalType.isOpen IntervalType.T.Closed |> should be False
        IntervalType.isOpen IntervalType.T.Open |> should be True
        
[<TestFixture; Category("Unit")>]
type ``Given the Interval Value module`` () =

    [<Test>]
    member x.``I can make an empty value interval with unit of measure`` () =
        let interval = Interval.Value.empty<J>
        Interval.left interval = Some 0.0<J> |> should be True
        Interval.right interval = Some 0.0<J> |> should be True

    [<Test>]
    member x.``I can make an empty value interval without unit of measure`` () =
        let interval = Interval.Value.empty
        Interval.left interval = Some 0.0 |> should be True
        Interval.right interval = Some 0.0 |> should be True

    [<Test>]
    member x.``I can make a bounded value interval with unit of measure`` () =
        let interval = Interval.Value.make(Some 3.0<J>, Some 7.0<J>) 
        Interval.left interval = Some 3.0<J> |> should be True
        Interval.right interval = Some 7.0<J> |> should be True
        Interval.Value.isBounded interval |> should be True

    [<Test>]
    member x.``I can make a bounded value interval without unit of measure`` () =
        let interval = Interval.Value.make(Some 3.0, Some 7.0) 
        Interval.left interval = Some 3.0 |> should be True
        Interval.right interval = Some 7.0 |> should be True
        Interval.Value.isBounded interval |> should be True

    [<Test>]
    member x.``I can make a left unbounded value interval with unit of measure`` () =
        let interval = Interval.Value.makeLeftUnbounded(Some 7.0<J>) 
        Interval.left interval = Some(LanguagePrimitives.FloatWithMeasure Interval.Value.leftUnboundedValue) |> should be True
        Interval.right interval = Some 7.0<J> |> should be True
        Interval.Value.isLeftUnbounded interval |> should be True
        Interval.Value.isLeftBounded interval |> should be False
        Interval.Value.isRightUnbounded interval |> should be False
        Interval.Value.isRightBounded interval |> should be True
        Interval.Value.isUnbounded interval |> should be False
        Interval.Value.isBounded interval |> should be False
    
    [<Test>]
    member x.``I can make a left unbounded value interval without unit of measure`` () =
        let interval = Interval.Value.makeLeftUnbounded(Some 7.0) 
        Interval.left interval = Some Interval.Value.leftUnboundedValue |> should be True
        Interval.right interval = Some 7.0 |> should be True
        Interval.Value.isLeftUnbounded interval |> should be True
        Interval.Value.isLeftBounded interval |> should be False
        Interval.Value.isRightUnbounded interval |> should be False
        Interval.Value.isRightBounded interval |> should be True
        Interval.Value.isUnbounded interval |> should be False
        Interval.Value.isBounded interval |> should be False

    [<Test>]
    member x.``I can make a right unbounded value interval with unit of measure`` () =
        let interval = Interval.Value.makeRightUnbounded(Some 3.0<J>) 
        Interval.left interval = Some 3.0<J> |> should be True
        Interval.right interval = Some(LanguagePrimitives.FloatWithMeasure Interval.Value.rightUnboundedValue) |> should be True
        Interval.Value.isLeftUnbounded interval |> should be False
        Interval.Value.isLeftBounded interval |> should be True
        Interval.Value.isRightUnbounded interval |> should be True
        Interval.Value.isRightBounded interval |> should be False
        Interval.Value.isUnbounded interval |> should be False
        Interval.Value.isBounded interval |> should be False

    [<Test>]
    member x.``I can make a right unbounded value interval without unit of measure`` () =
        let interval = Interval.Value.makeRightUnbounded(Some 3.0) 
        Interval.left interval = Some 3.0 |> should be True
        Interval.right interval = Some Interval.Value.rightUnboundedValue |> should be True
        Interval.Value.isLeftUnbounded interval |> should be False
        Interval.Value.isLeftBounded interval |> should be True
        Interval.Value.isRightUnbounded interval |> should be True
        Interval.Value.isRightBounded interval |> should be False
        Interval.Value.isUnbounded interval |> should be False
        Interval.Value.isBounded interval |> should be False

    [<Test>]
    member x.``I can make an unbounded value interval with unit of measure`` () =
        let interval = Interval.Value.makeUnbounded<J> 
        Interval.left interval = Some(LanguagePrimitives.FloatWithMeasure Interval.Value.leftUnboundedValue) |> should be True
        Interval.right interval = Some(LanguagePrimitives.FloatWithMeasure Interval.Value.rightUnboundedValue) |> should be True
        Interval.Value.isLeftUnbounded interval |> should be True
        Interval.Value.isLeftBounded interval |> should be False
        Interval.Value.isRightUnbounded interval |> should be True
        Interval.Value.isRightBounded interval |> should be False
        Interval.Value.isUnbounded interval |> should be True
        Interval.Value.isBounded interval |> should be False

    [<Test>]
    member x.``I can make an unbounded value interval without unit of measure`` () =
        let interval = Interval.Value.makeUnbounded 
        Interval.left interval = Some(Interval.Value.leftUnboundedValue) |> should be True
        Interval.right interval = Some(Interval.Value.rightUnboundedValue) |> should be True
        Interval.Value.isLeftUnbounded interval |> should be True
        Interval.Value.isLeftBounded interval |> should be False
        Interval.Value.isRightUnbounded interval |> should be True
        Interval.Value.isRightBounded interval |> should be False
        Interval.Value.isUnbounded interval |> should be True
        Interval.Value.isBounded interval |> should be False

[<TestFixture; Category("Unit")>]
type ``Given a value interval`` () =

    [<Test>]
    member x.``I can order a value interval with unit of measure`` () =
        let interval = Interval.Value.make(Some 7.0<J>,Some 3.0<J>) 
        Interval.Value.make(Some 7.0<J>,Some 3.0<J>) |> Interval.Value.order |> should equal (Interval.Value.make(Some 3.0<J>,Some 7.0<J>))
        Interval.Value.make(Some 3.0<J>,Some 7.0<J>) |> Interval.Value.order |> should equal (Interval.Value.make(Some 3.0<J>,Some 7.0<J>))

    [<Test>]
    member x.``I can order a value interval without unit of measure`` () =
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.make(Some 7.0,Some 3.0) |> Interval.Value.order |> should equal (Interval.Value.make(Some 3.0,Some 7.0))
        Interval.Value.make(Some 3.0,Some 7.0) |> Interval.Value.order |> should equal (Interval.Value.make(Some 3.0,Some 7.0))

    [<Test>]
    member x.``I can order a value interval with a none and a some value`` () =
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.make(None,Some 3.0) |> Interval.Value.order |> should equal (Interval.Value.make(None, Some 3.0))
        Interval.Value.make(Some 3.0,None) |> Interval.Value.order |> should equal (Interval.Value.make(Some 3.0,None))

    [<Test>]
    member x.``I can determine if a value falls within a closed interval`` () =
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.isIn IntervalType.T.Closed interval (Some 0.0) |> should be False
        Interval.Value.isIn IntervalType.T.Closed interval (Some 3.0) |> should be True
        Interval.Value.isIn IntervalType.T.Closed interval (Some 5.0) |> should be True
        Interval.Value.isIn IntervalType.T.Closed interval (Some 7.0) |> should be True
        Interval.Value.isIn IntervalType.T.Closed interval (Some 9.0) |> should be False

    [<Test>]
    member x.``I can determine if a value falls within an open interval`` () =
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.isIn IntervalType.T.Open interval (Some 0.0) |> should be False
        Interval.Value.isIn IntervalType.T.Open interval (Some 3.0) |> should be False
        Interval.Value.isIn IntervalType.T.Open interval (Some 5.0) |> should be True
        Interval.Value.isIn IntervalType.T.Open interval (Some 7.0) |> should be False
        Interval.Value.isIn IntervalType.T.Open interval (Some 9.0) |> should be False

    [<Test>]
    member x.``I can determine if a value falls within an left closed right open interval`` () =
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.isIn IntervalType.T.LeftClosedRightOpen interval (Some 0.0) |> should be False
        Interval.Value.isIn IntervalType.T.LeftClosedRightOpen interval (Some 3.0) |> should be True
        Interval.Value.isIn IntervalType.T.LeftClosedRightOpen interval (Some 5.0) |> should be True
        Interval.Value.isIn IntervalType.T.LeftClosedRightOpen interval (Some 7.0) |> should be False
        Interval.Value.isIn IntervalType.T.LeftClosedRightOpen interval (Some 9.0) |> should be False

    [<Test>]
    member x.``I can determine if a value falls within an left open right closed interval`` () =
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.isIn IntervalType.T.LeftOpenRightClosed interval (Some 0.0) |> should be False
        Interval.Value.isIn IntervalType.T.LeftOpenRightClosed interval (Some 3.0) |> should be False
        Interval.Value.isIn IntervalType.T.LeftOpenRightClosed interval (Some 5.0) |> should be True
        Interval.Value.isIn IntervalType.T.LeftOpenRightClosed interval (Some 7.0) |> should be True
        Interval.Value.isIn IntervalType.T.LeftOpenRightClosed interval (Some 9.0) |> should be False

    [<Test>]
    member x.``I can determine if a none value falls within an interval with some ends`` () =
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.isIn IntervalType.T.LeftClosedRightOpen interval None |> should be False
        Interval.Value.isIn IntervalType.T.LeftOpenRightClosed interval None |> should be False
        Interval.Value.isIn IntervalType.T.Closed interval None |> should be False
        Interval.Value.isIn IntervalType.T.Open interval None |> should be False

    [<Test>]
    member x.``I can determine if a none value falls within a closed interval with some and none ends`` () =
        let interval = Interval.Value.make(Some 7.0,None) 
        Interval.Value.isIn IntervalType.T.LeftClosedRightOpen interval None |> should be False
        Interval.Value.isIn IntervalType.T.LeftOpenRightClosed interval None |> should be True
        Interval.Value.isIn IntervalType.T.Closed interval None |> should be True
        Interval.Value.isIn IntervalType.T.Open interval None |> should be False

    [<Test>]
    member x.``I can determine if a none value falls within a closed interval with none and some ends`` () =
        let interval = Interval.Value.make(None, Some 7.0) 
        Interval.Value.isIn IntervalType.T.LeftClosedRightOpen interval None |> should be True
        Interval.Value.isIn IntervalType.T.LeftOpenRightClosed interval None |> should be False
        Interval.Value.isIn IntervalType.T.Closed interval None |> should be True
        Interval.Value.isIn IntervalType.T.Open interval None |> should be False

    [<Test>]
    member x.``I can determine if a none value falls within a closed interval with none ends`` () =
        let interval = Interval.Value.make(None,None) 
        Interval.Value.isIn IntervalType.T.LeftClosedRightOpen interval None |> should be True
        Interval.Value.isIn IntervalType.T.LeftOpenRightClosed interval None |> should be True
        Interval.Value.isIn IntervalType.T.Closed interval None |> should be True
        Interval.Value.isIn IntervalType.T.Open interval None |> should be False

    [<Test>]
    member x.``I can calculate the interval delta with some ends`` () =
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.delta interval |> should equal (Some -4.0)

    [<Test>]
    member x.``I can calculate the interval delta with some ends and unit of measure`` () =
        let interval = Interval.Value.make(Some 7.0<J>,Some 3.0<J>) 
        Interval.Value.delta interval |> should equal (Some -4.0<J>)

    [<Test>]
    member x.``I can calculate the interval delta with some and none ends`` () =
        let interval = Interval.Value.make(Some 7.0,None) 
        Interval.Value.delta interval |> should equal None

    [<Test>]
    member x.``I can calculate the interval delta with none and some ends`` () =
        let interval = Interval.Value.make(None, Some 7.0) 
        Interval.Value.delta interval |> should equal None

    [<Test>]
    member x.``I can calculate the interval delta with none ends`` () =
        let interval = Interval.Value.make(None, None) 
        Interval.Value.delta interval |> should equal None

    [<Test>]
    member x.``I can map over the interval start and end points``() =
        let interval = Interval.make(2,3)
        let actual = Interval.map (fun (x,y) -> x+1, y+1) interval
        let expected = Interval.make(3,4)
        actual |> should equal expected

    
[<TestFixture; Category("Unit")>]
type ``Given the Interval Time module`` () =

    let d1 = DateTimeOffset(2013,2,5,0,0,0,TimeSpan.FromHours(0.))
    let d2 = DateTimeOffset(2013,2,5,12,0,0,TimeSpan.FromHours(0.))

    [<Test>]
    member x.``I can make an empty time interval`` () =
        let interval = Interval.Time.empty
        Interval.left interval |> should equal (Interval.right interval)

    [<Test>]
    member x.``I can increment a interval by a time span`` () =
        let interval = Interval.Time.make (d1,d2)
        let actual = Interval.Time.incr (TimeSpan.FromHours(1.)) interval
        let expected = Interval.Time.make (d1.AddHours(1.), d2.AddHours(1.))
        actual |> should equal expected

    [<Test>]
    member x.``I can make a bounded time interval`` () =
        let interval = Interval.Time.make(d1, d2) 
        Interval.left interval |> should equal d1
        Interval.right interval |> should equal d2
        Interval.Time.isLeftUnbounded interval |> should be False
        Interval.Time.isRightUnbounded interval |> should be False
        Interval.Time.isUnbounded interval |> should be False
        Interval.Time.isLeftBounded interval |> should be True
        Interval.Time.isRightBounded interval |> should be True
        Interval.Time.isBounded interval |> should be True

    [<Test>]
    member x.``I can make a left unbounded time interval`` () =
        let interval = Interval.Time.makeLeftUnbounded(d2) 
        Interval.left interval |> should equal Interval.Time.leftUnboundedTime
        Interval.right interval |> should equal d2
        Interval.Time.isLeftUnbounded interval |> should be True
        Interval.Time.isLeftBounded interval |> should be False
        Interval.Time.isRightUnbounded interval |> should be False
        Interval.Time.isRightBounded interval |> should be True
        Interval.Time.isUnbounded interval |> should be False
        Interval.Time.isBounded interval |> should be False

    [<Test>]
    member x.``I can make a right unbounded time interval`` () =
        let interval = Interval.Time.makeRightUnbounded(d1) 
        Interval.left interval |> should equal d1
        Interval.right interval |> should equal Interval.Time.rightUnboundedTime
        Interval.Time.isLeftUnbounded interval |> should be False
        Interval.Time.isLeftBounded interval |> should be True
        Interval.Time.isRightUnbounded interval |> should be True
        Interval.Time.isRightBounded interval |> should be False
        Interval.Time.isUnbounded interval |> should be False
        Interval.Time.isBounded interval |> should be False

    [<Test>]
    member x.``I can make an unbounded time interval`` () =
        let interval = Interval.Time.makeUnbounded 
        Interval.left interval |> should equal Interval.Time.leftUnboundedTime
        Interval.right interval |> should equal Interval.Time.rightUnboundedTime
        Interval.Time.isLeftUnbounded interval |> should be True
        Interval.Time.isLeftBounded interval |> should be False
        Interval.Time.isRightUnbounded interval |> should be True
        Interval.Time.isRightBounded interval |> should be False
        Interval.Time.isUnbounded interval |> should be True
        Interval.Time.isBounded interval |> should be False

[<TestFixture; Category("Unit")>]
type ``Given a time interval`` () =

    let d1 = DateTimeOffset(2013,2,5,0,0,0,TimeSpan.FromHours(0.))
    let d2 = DateTimeOffset(2013,2,5,12,0,0,TimeSpan.FromHours(0.))
    let closedHalfhours = [|
            d1
            d1.AddMinutes(1.0 * 30.0)
            d1.AddMinutes(2.0 * 30.0)
            d1.AddMinutes(3.0 * 30.0)
            d1.AddMinutes(4.0 * 30.0)
            d1.AddMinutes(5.0 * 30.0)
            d1.AddMinutes(6.0 * 30.0)
            d1.AddMinutes(7.0 * 30.0)
            d1.AddMinutes(8.0 * 30.0)
            d1.AddMinutes(9.0 * 30.0)
            d1.AddMinutes(10.0 * 30.0)
            d1.AddMinutes(11.0 * 30.0)
            d1.AddMinutes(12.0 * 30.0) 
            d1.AddMinutes(13.0 * 30.0)
            d1.AddMinutes(14.0 * 30.0)
            d1.AddMinutes(15.0 * 30.0)
            d1.AddMinutes(16.0 * 30.0)
            d1.AddMinutes(17.0 * 30.0)
            d1.AddMinutes(18.0 * 30.0)
            d1.AddMinutes(19.0 * 30.0)
            d1.AddMinutes(20.0 * 30.0)
            d1.AddMinutes(21.0 * 30.0)
            d1.AddMinutes(22.0 * 30.0)
            d1.AddMinutes(23.0 * 30.0)
            d2
        |]
    let closedMinutes = [|
            d1
            d1.AddMinutes(1.0)
            d1.AddMinutes(2.0)
            d1.AddMinutes(3.0)
            d1.AddMinutes(4.0)
            d1.AddMinutes(5.0)
            d1.AddMinutes(6.0)
            d1.AddMinutes(7.0)
            d1.AddMinutes(8.0)
            d1.AddMinutes(9.0)
            d1.AddMinutes(10.0)
            d1.AddMinutes(11.0)
            d1.AddMinutes(12.0) 
            d1.AddMinutes(13.0)
            d1.AddMinutes(14.0)
            d1.AddMinutes(15.0)
        |]

    [<Test>]
    member x.``I can order a time interval`` () =
        let interval = Interval.Value.make(Some 7.0<J>,Some 3.0<J>) 
        Interval.Time.make(d2,d1) |> Interval.Time.order |> should equal (Interval.Time.make(d1,d2))
        Interval.Time.make(d1,d2) |> Interval.Time.order |> should equal (Interval.Time.make(d1,d2))

    [<Test>]
    member x.``I can determine if a time falls within a closed interval`` () =
        let interval = Interval.Time.make(d1,d2) 
        Interval.Time.isIn IntervalType.T.Closed (d1.AddHours(-1.0)) interval |> should be False
        Interval.Time.isIn IntervalType.T.Closed (d1.AddHours(0.0))  interval |> should be True
        Interval.Time.isIn IntervalType.T.Closed (d1.AddHours(1.0))  interval |> should be True
        Interval.Time.isIn IntervalType.T.Closed (d2.AddHours(0.0))  interval |> should be True
        Interval.Time.isIn IntervalType.T.Closed (d2.AddHours(1.0))  interval |> should be False

    [<Test>]
    member x.``I can determine if a value falls within an open interval`` () =
        let interval = Interval.Time.make(d1,d2) 
        Interval.Time.isIn IntervalType.T.Open (d1.AddHours(-1.0)) interval |> should be False
        Interval.Time.isIn IntervalType.T.Open (d1.AddHours(0.0))  interval |> should be False
        Interval.Time.isIn IntervalType.T.Open (d1.AddHours(1.0))  interval |> should be True
        Interval.Time.isIn IntervalType.T.Open (d2.AddHours(0.0))  interval |> should be False
        Interval.Time.isIn IntervalType.T.Open (d2.AddHours(1.0))  interval |> should be False

    [<Test>]
    member x.``I can determine if a value falls within an left closed right open interval`` () =
        let interval = Interval.Time.make(d1,d2) 
        Interval.Time.isIn IntervalType.T.LeftClosedRightOpen (d1.AddHours(-1.0)) interval |> should be False
        Interval.Time.isIn IntervalType.T.LeftClosedRightOpen (d1.AddHours(0.0))  interval |> should be True
        Interval.Time.isIn IntervalType.T.LeftClosedRightOpen (d1.AddHours(1.0))  interval |> should be True
        Interval.Time.isIn IntervalType.T.LeftClosedRightOpen (d2.AddHours(0.0))  interval |> should be False
        Interval.Time.isIn IntervalType.T.LeftClosedRightOpen (d2.AddHours(1.0))  interval |> should be False

    [<Test>]
    member x.``I can determine if a value falls within an left open right closed interval`` () =
        let interval = Interval.Time.make(d1,d2) 
        Interval.Time.isIn IntervalType.T.LeftOpenRightClosed (d1.AddHours(-1.0)) interval |> should be False
        Interval.Time.isIn IntervalType.T.LeftOpenRightClosed (d1.AddHours(0.0))  interval |> should be False
        Interval.Time.isIn IntervalType.T.LeftOpenRightClosed (d1.AddHours(1.0))  interval |> should be True
        Interval.Time.isIn IntervalType.T.LeftOpenRightClosed (d2.AddHours(0.0))  interval |> should be True
        Interval.Time.isIn IntervalType.T.LeftOpenRightClosed (d2.AddHours(1.0))  interval |> should be False

    [<Test>]
    member x.``I can calculate the interval delta`` () =
        let interval = Interval.Time.make(d1,d2) 
        Interval.Time.delta interval |> should equal (TimeSpan.FromHours(12.))

    [<Test>]
    member x.``I can calculate the negative interval delta`` () =
        let interval = Interval.Time.make(d2,d1) 
        Interval.Time.delta interval |> should equal (- TimeSpan.FromHours(12.))

    [<Test>]
    member x.``I can get the halfhours within a closed interval starting and stopping on the halfhour`` () =
        let interval = Interval.Time.make(d1,d2)
        let actual = Interval.Time.getHalfhourTimes IntervalType.T.Closed interval
        let expected = closedHalfhours
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhours within a closed interval not starting and ending on the hour`` () =
        let interval = Interval.Time.make(d1.AddSeconds(1.0),d2.AddSeconds(-1.0))
        let actual = Interval.Time.getHalfhourTimes IntervalType.T.Closed interval
        let expected = closedHalfhours.[1..23]
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhours within an open interval`` () =
        let interval = Interval.Time.make(d1,d2)
        let actual = Interval.Time.getHalfhourTimes IntervalType.T.Open interval
        let expected = closedHalfhours.[1..23]
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhours within a closed-open interval`` () =
        let interval = Interval.Time.make(d1,d2)
        let actual = Interval.Time.getHalfhourTimes IntervalType.T.LeftClosedRightOpen interval
        let expected = closedHalfhours.[0..23]
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhours within a open-closed interval`` () =
        let interval = Interval.Time.make(d1,d2)
        let actual = Interval.Time.getHalfhourTimes IntervalType.T.LeftOpenRightClosed interval
        let expected = closedHalfhours.[1..24]
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhours within an interval without any halfhours`` () =
        let interval = Interval.Time.make(d1.AddMinutes(1.0),d1.AddMinutes(15.0))
        let actual = Interval.Time.getHalfhourTimes IntervalType.T.LeftOpenRightClosed interval
        let expected = Seq.empty
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhours within an interval with a single halfhour`` () =
        let interval = Interval.Time.make(d1,d1.AddMinutes(15.0))
        let actual = Interval.Time.getHalfhourTimes IntervalType.T.LeftOpenRightClosed interval
        let expected = closedHalfhours.[0..0]
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minutes within a closed interval starting and stopping on the minute`` () =
        let interval = Interval.Time.make(d1,d1.AddMinutes(15.0))
        let actual = Interval.Time.getMinuteTimes IntervalType.T.Closed interval
        let expected = closedMinutes
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minutes within a closed interval not starting and ending on the minute`` () =
        let interval = Interval.Time.make(d1.AddSeconds(1.0),d1.AddMinutes(15.0).AddSeconds(-1.0))
        let actual = Interval.Time.getMinuteTimes IntervalType.T.Closed interval |> Seq.toArray
        let expected = closedMinutes.[1..14]
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minutes within an open interval`` () =
        let interval = Interval.Time.make(d1,d1.AddMinutes(15.0))
        let actual = Interval.Time.getMinuteTimes IntervalType.T.Open interval
        let expected = closedMinutes.[1..14]
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minutes within a closed-open interval`` () =
        let interval = Interval.Time.make(d1,d1.AddMinutes(15.0))
        let actual = Interval.Time.getMinuteTimes IntervalType.T.LeftClosedRightOpen interval
        let expected = closedMinutes.[0..14]
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minutes within a open-closed interval`` () =
        let interval = Interval.Time.make(d1,d1.AddMinutes(15.0))
        let actual = Interval.Time.getMinuteTimes IntervalType.T.LeftOpenRightClosed interval
        let expected = closedMinutes.[1..15]
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minutes within an interval without any minutes`` () =
        let interval = Interval.Time.make(d1.AddSeconds(1.0),d1.AddSeconds(15.0))
        let actual = Interval.Time.getMinuteTimes IntervalType.T.LeftOpenRightClosed interval
        let expected = Seq.empty
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minutes within an interval with a single minute`` () =
        let interval = Interval.Time.make(d1,d1.AddSeconds(15.0))
        let actual = Interval.Time.getMinuteTimes IntervalType.T.LeftOpenRightClosed interval
        let expected = closedMinutes.[0..0]
        actual |> should equal expected
    

[<TestFixture; Category("Unit")>]
type ``Given the TimePoint module`` () =

    let d1 = DateTimeOffset(2013,2,5,0,0,0,TimeSpan.FromHours(0.))

    [<Test>]
    member x.``I can make an empty time point`` () =
        let time = DateTimeOffset.UtcNow
        let timePoint:TimePoint.T<float<J>> = TimePoint.empty time
        TimePoint.time timePoint |> should equal time
        TimePoint.value timePoint |> should equal None

    [<Test>]
    member x.``I can make a time point`` () =
        let time = DateTimeOffset.UtcNow
        let timePoint = TimePoint.make(d1, Some 11.0)
        (TimePoint.time timePoint) |> should equal d1
        TimePoint.value timePoint |> should equal (Some 11.0)

    [<Test>]
    member x.``I can make a time point with unit of measure`` () =
        let time = DateTimeOffset.UtcNow
        let timePoint = TimePoint.make(d1, Some 11.0<J>)
        (TimePoint.time timePoint) |> should equal d1
        TimePoint.value timePoint |> should equal (Some 11.0<J>)

type ``Given the TimeSegment module`` () =

    [<Test>]
    member x.``the intersection calculation for two intersecting line segments should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d1
        let lineSegment1 = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d2,Some 100.0<J>), TimePoint.make(d3,Some 0.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection.Value.Time |> should equal (d0.AddHours(12.0))
        intersection.Value.Value.Value |> should equal 50.0<J>
  
    [<Test>]
    member x.``the intersection calculation for two parallel line segments should return None`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0.AddHours(1.0)
        let d3 = d2.AddDays(1.0)
        let lineSegment1 = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d2,Some 0.0<J>), TimePoint.make(d3,Some 100.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection |> should equal None  

    [<Test>]
    member x.``the intersection calculation for two non-intersecting non-parallel line segments should return None`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d2,Some 100.0<J>), TimePoint.make(d3,Some 100.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection |> should equal None

    [<Test>]
    member x.``the intersection calculation for two line segments that touch at the end point of the first segment should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = TimeSegment.make(TimePoint.make(d2,Some 50.0<J>), TimePoint.make(d3,Some 50.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection.Value.Time |> should equal d3
        intersection.Value.Value.Value |> should equal 50.0<J>

    [<Test>]
    member x.``the intersection calculation for two line segments that touch at the start point of the first segment should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0.AddHours(12.0)
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = TimeSegment.make(TimePoint.make(d2,Some 50.0<J>), TimePoint.make(d3,Some 50.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection.Value.Time |> should equal d2
        intersection.Value.Value.Value |> should equal 50.0<J>

    [<Test>]
    member x.``the intersection calculation for two line segments that touch at the end point of the second segment should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d2,Some 50.0<J>), TimePoint.make(d3,Some 50.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection.Value.Time |> should equal d3
        intersection.Value.Value.Value |> should equal 50.0<J>

    [<Test>]
    member x.``the intersection calculation for two line segments that touch at the start point of the second segment should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0.AddHours(12.0)
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d2,Some 50.0<J>), TimePoint.make(d3,Some 50.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection.Value.Time |> should equal d2
        intersection.Value.Value.Value |> should equal 50.0<J>

    [<Test>]
    member x.``the intersection calculation for two collinear line segments should return None`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0.AddHours(12.0)
        let d3 = d2.AddDays(1.0)
        let lineSegment1 = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d2,Some 50.0<J>), TimePoint.make(d3,Some 150.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection |> should equal None

    [<Test>]
    member x.``the intersection calculation for two line segments with coincident start points should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d1
        let lineSegment1 = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d2,Some 0.0<J>), TimePoint.make(d3,Some 150.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection.Value.Time |> should equal d0
        intersection.Value.Value.Value |> should equal 0.0<J>

    [<Test>]
    member x.``the intersection calculation for two line segments with coincident end points should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d1
        let lineSegment1 = TimeSegment.make(TimePoint.make(d0,Some 100.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d2,Some 0.0<J>), TimePoint.make(d3,Some 100.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection.Value.Time |> should equal d1
        intersection.Value.Value.Value |> should equal 100.0<J>

    [<Test>]
    member x.``the intersection calculation for two line segments with None values should return None`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d1
        let lineSegment1 = TimeSegment.make(TimePoint.make(d0,None), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2 = TimeSegment.make(TimePoint.make(d2,Some 100.0<J>), TimePoint.make(d3,Some 0.0<J>))
        let intersection = TimeSegment.intersection lineSegment1 lineSegment2
        intersection |> should equal None
        let lineSegment1' = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,None))
        let lineSegment2' = TimeSegment.make(TimePoint.make(d2,Some 100.0<J>), TimePoint.make(d3,Some 0.0<J>))
        let intersection' = TimeSegment.intersection lineSegment1' lineSegment2'
        intersection' |> should equal None
        let lineSegment1'' = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2'' = TimeSegment.make(TimePoint.make(d2,None), TimePoint.make(d3,Some 0.0<J>))
        let intersection'' = TimeSegment.intersection lineSegment1'' lineSegment2''
        intersection'' |> should equal None
        let lineSegment1''' = TimeSegment.make(TimePoint.make(d0,Some 0.0<J>), TimePoint.make(d1,Some 100.0<J>))
        let lineSegment2''' = TimeSegment.make(TimePoint.make(d2,Some 100.0<J>), TimePoint.make(d3,None))
        let intersection''' = TimeSegment.intersection lineSegment1''' lineSegment2'''
        intersection''' |> should equal None


type ``Given the TimeLine module`` () =

    [<Test>]
    member x.``I can make an empty TimeLine without unit of measure`` () =
        let actual : TimeLine.Any<int> = TimeLine.empty
        actual |> should equal [||]

    [<Test>]
    member x.``I can make an empty TimeLine with unit of measure`` () =
        let actual : TimeLine.Float<J> = TimeLine.empty
        actual |> should equal [||]

    [<Test>]
    member x.``I can make a TimeLine from an empty sequence of points`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [||]        
        let actual = TimeLine.Continuous.make points
        actual |> should equal [||]

    [<Test>]
    member x.``I can make a TimeLine from a sequence of points containing a single point`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>|]        
        let actual = TimeLine.Continuous.make points
        actual |> should equal [||]

    [<Test>]
    member x.``I can make a TimeLine from a sequence of two points`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>|]        
        let actual = TimeLine.Continuous.make points
        let expected = [|
            TimeSegment.make(TimePoint.make(Helper.d0,Some 0.0<J>),TimePoint.make(Helper.d0.AddMinutes(30.0),Some 100.0<J>))
        |]
        actual |> should equal expected

    [<Test>]
    member x.``I can make a TimeLine from a sequence of three points`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let actual = TimeLine.Continuous.make points
        let expected = [|
            TimeSegment.make(TimePoint.make(Helper.d0,Some 0.0<J>),TimePoint.make(Helper.d0.AddMinutes(30.0),Some 100.0<J>))
            TimeSegment.make(TimePoint.make(Helper.d0.AddMinutes(30.0),Some 100.0<J>),TimePoint.make(Helper.d0.AddMinutes(60.0),Some 200.0<J>))
        |]
        actual |> should equal expected

    [<Test>]
    member x.``I can make a TimeLine from a time interval`` () =
        let interval = Interval.make(Helper.d0, Helper.d0.AddMinutes(30.0))
        let actual = TimeLine.ofInterval interval
        let expected = [|
            TimeSegment.make(TimePoint.make(Helper.d0,None),TimePoint.make(Helper.d0.AddMinutes(30.0),None))
        |]
        actual |> should equal expected

    [<Test>]
    member x.``I can make a time interval from a line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.toInterval line
        let expected = Some(Interval.make(Helper.d0,Helper.d0.AddMinutes(60.0)))
        actual |> should equal expected

    [<Test>]
    member x.``I can get the start point of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.startPoint line
        let expected = Some(TimePoint.make(Helper.d0, Some 0.0<J>))
        actual |> should equal expected

    [<Test>]
    member x.``I can get the end point of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.endPoint line
        let expected = Some(TimePoint.make(Helper.d0.AddMinutes(60.0), Some 200.0<J>))
        actual |> should equal expected

    [<Test>]
    member x.``I can get the start point of the empty line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [||]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.startPoint line
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can get the end point of the empty line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [||]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.endPoint line
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can get all of the insection points with another line`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d1.AddDays(1.0)
        let d3 = d2.AddDays(1.0)
        let line1 = 
            TimeLine.Continuous.make(
                [
                    TimePoint.make(d0,Some 0.0<J>)
                    TimePoint.make(d1,Some 100.0<J>)
                    TimePoint.make(d2,Some 100.0<J>)
                    TimePoint.make(d3,Some 0.0<J>)
                ])
        let line2 =
            TimeLine.Continuous.make(
                [
                    TimePoint.make(d0,Some 50.0<J>)
                    TimePoint.make(d3,Some 50.0<J>)
                ])
        let intersections = TimeLine.intersections line1 line2
        Array.length intersections |> should equal 2
        intersections.[0].Time |> should equal (d0.AddHours(12.0))
        intersections.[0].Value.Value |> should equal 50.0<J>
        intersections.[1].Time |> should equal (d2.AddHours(12.0))
        intersections.[1].Value.Value |> should equal 50.0<J>

    [<Test>]
    member x.``I can map a function over the segments of a line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 50.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = 
            TimeLine.map (
                TimeSegment.map (fun (startPoint,endPoint) -> 
                    TimePoint.map (fun (d,v) -> 
                        (d, Option.map (fun x -> (*) x 1000.<s>) v)) startPoint, 
                    TimePoint.map (fun (d,v) -> 
                        (d, Option.map (fun x -> (*) x 1000.<s>) v)) endPoint)) line
        let expected = [|
            TimeSegment.make(TimePoint.make(Helper.d0,Some 50000.0<J s>),TimePoint.make(Helper.d0.AddMinutes(30.0),Some 100000.0<J s>))
            TimeSegment.make(TimePoint.make(Helper.d0.AddMinutes(30.0),Some 100000.0<J s>),TimePoint.make(Helper.d0.AddMinutes(60.0),Some 200000.0<J s>))
        |]
        actual |> should equal expected

    [<Test>]
    member x.``I can fold a function over the segments of a line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 50.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let energy (lineSegment:TimeSegment.Float<J>) =
            let power = (TimeSegment.startValue lineSegment ?+? TimeSegment.endValue lineSegment) ?/ 2.0 
            let time = lineSegment |> TimeSegment.deltaTime |> (fun x -> x.TotalSeconds * 1.<s>)
            power ?* time 
        let actual = TimeLine.fold (fun s lineSegment -> s ?+? energy lineSegment) (Some 0.0<J s>) line
        let expected = Some 405000.0<J s>
        actual |> should equal expected
  
    [<Test>]
    member x.``I can get the start time of each segment of an empty line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [||]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.startTimes line
        let expected = []        
        actual |> should equal expected
 
    [<Test>]
    member x.``I can get the end time of each segment of an empty line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [||]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.endTimes line
        let expected = []        
        actual |> should equal expected
 
    [<Test>]
    member x.``I can get all of the distinct times of each point of an empty line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [||]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.allDistinctTimes line
        let expected = []        
        actual |> should equal expected

    [<Test>]
    member x.``I can get the start time of each segment of a line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 50.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.startTimes line
        let expected = [Helper.d0.AddMinutes(0.0);Helper.d0.AddMinutes(30.0)]        
        actual |> should equal expected
 
    [<Test>]
    member x.``I can get the end time of each segment of a line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 50.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.endTimes line
        let expected = [Helper.d0.AddMinutes(30.0);Helper.d0.AddMinutes(60.0)]        
        actual |> should equal expected
 
    [<Test>]
    member x.``I can get all of the distinct times of each point of a line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 50.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.allDistinctTimes line
        let expected = [Helper.d0.AddMinutes(0.0);Helper.d0.AddMinutes(30.0);Helper.d0.AddMinutes(60.0)]        
        actual |> should equal expected

type ``Given the TimeLine Continuous module`` () =

    [<Test>]
    member x.``I can take a slice of the timeline with a single element`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = 
            TimeLine.Continuous.slice
                        (Helper.d0,Helper.d0.AddMinutes(30.)) 
                        line
        actual |> should equal [|
            TimeSegment.make(TimePoint.make (Helper.d0, Some 0.0<J>), TimePoint.make(Helper.d0.AddMinutes(30.), Some 100.<J>))
        |]


    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval ends in the middle of a segement but spanning two segments``() =
       let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>; Some 200.<J>; Some 250.<J>|] 
       let line = TimeLine.Continuous.make points
       let interval = Interval.make (Helper.d0.AddMinutes(15.),Helper.d0.AddMinutes(45.))
       let actual = 
             TimeLine.Continuous.slice 
                        interval
                        line
       let expected = 
            [|
                TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(15.), Some 50.0<J>), TimePoint.make(Helper.d0.AddMinutes(30.), Some 100.<J>))
                TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(30.), Some 100.0<J>), TimePoint.make(Helper.d0.AddMinutes(45.), Some 150.<J>))
            |]
        
       actual |> should equal expected


    [<Test>]
    member x.``I can take a slice of a TimeLine when a single segment wraps the interal completely``() =
       let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>;|] 
       let line = TimeLine.Continuous.make points
       let interval = Interval.make (Helper.d0.AddMinutes(10.),Helper.d0.AddMinutes(15.))
       let actual = 
             TimeLine.Continuous.slice 
                        interval
                        line

       actual.[0].Start.Time |> should equal (Helper.d0.AddMinutes(10.))
       actual.[0].End.Time |> should equal (Helper.d0.AddMinutes(15.))
       Assert.That(actual.[0].Start.Value.Value, Is.EqualTo(33.3).Within(0.1))
       Assert.That(actual.[0].End.Value.Value, Is.EqualTo(50.0))

    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval is fully within the timeline``() =
       let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>; Some 200.<J>; Some 250.<J>|] 
       let line = TimeLine.Continuous.make points
       let interval = Interval.make (Helper.d0.AddMinutes(30.),Helper.d0.AddHours(1.))
       let actual = 
             TimeLine.Continuous.slice 
                        interval
                        line
       let expected = 
            [|TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(30.), Some 100.0<J>), TimePoint.make(Helper.d0.AddMinutes(60.), Some 200.<J>))|]
        
       actual |> should equal expected


    [<Test>]
    member x.``I can try find a value before the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.Continuous.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(-1.0)) line
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.Continuous.tryFindValue IntervalType.T.Closed Helper.d0 line
        let expected = Some 0.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.Continuous.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(60.0)) line
        let expected = Some 200.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value after the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.Continuous.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(61.0)) line
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the start of a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.Continuous.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(30.0)) line
        let expected = Some 100.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the within a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.Continuous.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(45.0)) line
        let expected = Some 150.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can convert a line to a seq`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 30.0<J>; Some 60.0<J>|]        
        let line = TimeLine.Continuous.make points
        let actual = TimeLine.Continuous.toSeq IntervalType.T.LeftOpenRightClosed (TimeSpan.FromMinutes(1.0)) line
        let expected = [0 .. 60] |> Seq.map (fun i -> if i = 0 then None else float i |> (fun x -> x * 1.<J>) |> Some)
        actual |> should equal expected

    [<Test>]
    member x.``I can convert an empty line to a seq`` () =
        let line : TimeLine.Float<J> = TimeLine.empty
        let actual = TimeLine.Continuous.toSeq IntervalType.T.LeftOpenRightClosed (TimeSpan.FromMinutes(1.0)) line
        let expected = Seq.empty
        actual |> should equal expected


type ``Given the TimeLine Instantaneous module`` () =

    [<Test>]
    member x.``I can take a slice of the timeline with a single element`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>|]        
        let line = TimeLine.Instantaneous.make points
        let actual = 
            TimeLine.Instantaneous.slice
                        (Helper.d0,Helper.d0.AddMinutes(30.)) 
                        line
        actual |> should equal [|
            TimeSegment.make(TimePoint.make (Helper.d0, Some 0.0<J>), TimePoint.make(Helper.d0, Some 0.0<J>))
            TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(30.), Some 100.<J>), TimePoint.make(Helper.d0.AddMinutes(30.), Some 100.<J>))
        |]


    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval ends in the middle of a segement but spanning two segments``() =
       let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>; Some 200.<J>; Some 250.<J>|] 
       let line = TimeLine.Instantaneous.make points
       let interval = Interval.make (Helper.d0.AddMinutes(15.),Helper.d0.AddMinutes(45.))
       let actual = 
             TimeLine.Instantaneous.slice 
                        interval
                        line
       let expected : TimeSegment.Any<float<J>> [] = 
            [|
                TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(30.), Some 100.0<J>), TimePoint.make(Helper.d0.AddMinutes(30.), Some 100.<J>))
            |]
        
       actual |> should equal expected


    [<Test>]
    member x.``I can take a slice of a TimeLine when a single segment wraps the interal completely``() =
       let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>;|] 
       let line = TimeLine.Instantaneous.make points
       let interval = Interval.make (Helper.d0.AddMinutes(10.),Helper.d0.AddMinutes(15.))
       let actual = 
             TimeLine.Instantaneous.slice 
                        interval
                        line
       let expected : TimeSegment.Any<float<J>> [] = 
            [|
            |]
        
       actual |> should equal expected

    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval is fully within the timeline``() =
       let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>; Some 200.<J>; Some 250.<J>|] 
       let line = TimeLine.Instantaneous.make points
       let interval = Interval.make (Helper.d0.AddMinutes(30.),Helper.d0.AddHours(1.))
       let actual = 
             TimeLine.Instantaneous.slice 
                        interval
                        line
       let expected = 
            [|
                TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(30.), Some 100.<J>), TimePoint.make (Helper.d0.AddMinutes(30.), Some 100.<J>))
                TimeSegment.make(TimePoint.make(Helper.d0.AddMinutes(60.), Some 200.<J>), TimePoint.make(Helper.d0.AddMinutes(60.), Some 200.<J>))
            |]
        
       actual |> should equal expected

    [<Test>]
    member x.``I can try find a value before the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Instantaneous.make points
        let actual = TimeLine.Instantaneous.tryFindValue (Helper.d0.AddMinutes(-1.0)) line
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Instantaneous.make points
        let actual = TimeLine.Instantaneous.tryFindValue Helper.d0 line
        let expected = Some 0.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Instantaneous.make points
        let actual = TimeLine.Instantaneous.tryFindValue (Helper.d0.AddMinutes(60.0)) line
        let expected = Some 200.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value after the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Instantaneous.make points
        let actual = TimeLine.Instantaneous.tryFindValue (Helper.d0.AddMinutes(61.0)) line
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the start of a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Instantaneous.make points
        let actual = TimeLine.Instantaneous.tryFindValue (Helper.d0.AddMinutes(30.0)) line
        let expected = Some 100.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value within a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Instantaneous.make points
        let actual = TimeLine.Instantaneous.tryFindValue (Helper.d0.AddMinutes(45.0)) line
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can convert a line to a seq`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 30.0<J>; Some 60.0<J>|]        
        let line = TimeLine.Instantaneous.make points
        let actual = TimeLine.Instantaneous.toSeq (TimeSpan.FromMinutes(1.0)) line
        let a = Seq.toArray actual
        let expected = seq { 
                yield Some 0.0<J>
                yield! Seq.init 29 (fun i -> None)
                yield Some 30.0<J> 
                yield! Seq.init 29 (fun i -> None) 
                yield Some 60.0<J> 
            }
        actual |> should equal expected

    [<Test>]
    member x.``I can convert an empty line to a seq`` () =
        let line : TimeLine.Float<J> = TimeLine.empty
        let actual = TimeLine.Instantaneous.toSeq (TimeSpan.FromMinutes(1.0)) line
        let expected = Seq.empty
        actual |> should equal expected


type ``Given the TimeLine Discrete module`` () =

    [<Test>]
    member x.``I can take a slice of the timeline with a single element`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>|]        
        let line = TimeLine.Discrete.make points
        let actual = 
            TimeLine.Discrete.slice
                        (Helper.d0,Helper.d0.AddMinutes(30.)) 
                        line
        actual |> should equal [|
            TimeSegment.make(TimePoint.make (Helper.d0, Some 0.0<J>), TimePoint.make(Helper.d0.AddMinutes(30.), Some 0.<J>))
        |]


    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval ends in the middle of a segement but spanning two segments``() =
       let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>; Some 200.<J>; Some 250.<J>|] 
       let line = TimeLine.Discrete.make points
       let interval = Interval.make (Helper.d0.AddMinutes(15.),Helper.d0.AddMinutes(45.))
       let actual = 
             TimeLine.Discrete.slice 
                        interval
                        line
       let expected = 
            [|
                TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(15.), Some 0.0<J>), TimePoint.make(Helper.d0.AddMinutes(30.), Some 0.<J>))
                TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(30.), Some 100.0<J>), TimePoint.make(Helper.d0.AddMinutes(45.), Some 100.<J>))
            |]
        
       actual |> should equal expected


    [<Test>]
    member x.``I can take a slice of a TimeLine when a single segment wraps the interal completely``() =
       let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>;|] 
       let line = TimeLine.Discrete.make points
       let interval = Interval.make (Helper.d0.AddMinutes(10.),Helper.d0.AddMinutes(15.))
       let actual = 
             TimeLine.Discrete.slice 
                        interval
                        line
       let expected = 
            [|
                TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(10.), Some 0.0<J>), TimePoint.make(Helper.d0.AddMinutes(15.), Some 0.<J>))
            |]
        
       actual |> should equal expected

    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval is fully within the timeline``() =
       let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.<J>; Some 200.<J>; Some 250.<J>|] 
       let line = TimeLine.Discrete.make points
       let interval = Interval.make (Helper.d0.AddMinutes(30.),Helper.d0.AddHours(1.))
       let actual = 
             TimeLine.Discrete.slice 
                        interval
                        line
       let expected = 
            [|TimeSegment.make(TimePoint.make (Helper.d0.AddMinutes(30.), Some 100.0<J>), TimePoint.make(Helper.d0.AddMinutes(60.), Some 100.<J>))|]
        
       actual |> should equal expected

    [<Test>]
    member x.``I can try find a value before the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Discrete.make points
        let actual = TimeLine.Discrete.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(-1.0)) line
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Discrete.make points
        let actual = TimeLine.Discrete.tryFindValue IntervalType.T.Closed (Helper.d0) line
        let expected = Some 0.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Discrete.make points
        let actual = TimeLine.Discrete.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(60.0)) line
        let expected = Some 100.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value after the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Discrete.make points
        let actual = TimeLine.Discrete.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(61.0)) line
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value at the start of a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Discrete.make points
        let actual = TimeLine.Discrete.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(30.0)) line
        let expected = Some 0.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can try find a value within a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 100.0<J>; Some 200.0<J>|]        
        let line = TimeLine.Discrete.make points
        let actual = TimeLine.Discrete.tryFindValue IntervalType.T.Closed (Helper.d0.AddMinutes(45.0))line
        let expected = Some 100.0<J>
        actual |> should equal expected

    [<Test>]
    member x.``I can convert a line to a seq`` () =
        let points = Helper.getPoints Helper.d0 30.0<s> [|Some 0.0<J>; Some 30.0<J>; Some 60.0<J>|]        
        let line = TimeLine.Discrete.make points
        let actual = TimeLine.Discrete.toSeq IntervalType.T.LeftOpenRightClosed (TimeSpan.FromMinutes(1.0)) line
        let expected = seq {
                yield None
                yield! Seq.init 30 (fun i -> Some 0.0<J>); 
                yield! Seq.init 30 (fun i -> Some 30.0<J>);  
            }
        actual |> should equal expected

    [<Test>]
    member x.``I can convert an empty line to a seq`` () =
        let line : TimeLine.Float<J> = TimeLine.empty
        let actual = TimeLine.Discrete.toSeq IntervalType.T.LeftClosedRightOpen (TimeSpan.FromMinutes(1.0)) line
        let expected = Seq.empty
        actual |> should equal expected

        