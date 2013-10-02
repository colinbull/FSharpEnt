namespace FSharp.Enterprise.Tests

open NUnit.Framework
open FsUnit
open System
open FSharp.Enterprise.OptionOperators
open FSharp.Enterprise

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
        let interval = Interval.Value.empty
        Interval.left interval = Some 0.0 |> should be True
        Interval.right interval = Some 0.0 |> should be True

    [<Test>]
    member x.``I can make an empty value interval without unit of measure`` () =
        let interval = Interval.Value.empty
        Interval.left interval = Some 0.0 |> should be True
        Interval.right interval = Some 0.0 |> should be True

    [<Test>]
    member x.``I can make a bounded value interval with unit of measure`` () =
        let interval = Interval.Value.make(Some 3.0, Some 7.0) 
        Interval.left interval = Some 3.0 |> should be True
        Interval.right interval = Some 7.0 |> should be True
        Interval.Value.isBounded interval |> should be True

    [<Test>]
    member x.``I can make a bounded value interval without unit of measure`` () =
        let interval = Interval.Value.make(Some 3.0, Some 7.0) 
        Interval.left interval = Some 3.0 |> should be True
        Interval.right interval = Some 7.0 |> should be True
        Interval.Value.isBounded interval |> should be True

    [<Test>]
    member x.``I can make a left unbounded value interval with unit of measure`` () =
        let interval = Interval.Value.makeLeftUnbounded(Some 7.0) 
        Interval.left interval = Some(Interval.Value.leftUnboundedValue) |> should be True
        Interval.right interval = Some 7.0 |> should be True
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
        let interval = Interval.Value.makeRightUnbounded(Some 3.0) 
        Interval.left interval = Some 3.0 |> should be True
        Interval.right interval = Some(Interval.Value.rightUnboundedValue) |> should be True
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
        let interval = Interval.Value.makeUnbounded 
        Interval.left interval = Some(Interval.Value.leftUnboundedValue) |> should be True
        Interval.right interval = Some(Interval.Value.rightUnboundedValue) |> should be True
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
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.make(Some 7.0,Some 3.0) |> Interval.Value.order |> should equal (Interval.Value.make(Some 3.0 ,Some 7.0))
        Interval.Value.make(Some 3.0,Some 7.0) |> Interval.Value.order |> should equal (Interval.Value.make(Some 3.0 ,Some 7.0))

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
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
        Interval.Value.delta interval |> should equal (Some -4.0)

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

    [<Test>]
    member x.``I can determine if two intervals intersect`` () =
        Interval.intersects (Interval.make(2.0,2.5)) (Interval.make(3.0,4.0)) |> should be False
        Interval.intersects (Interval.make(2.0,3.0)) (Interval.make(3.0,4.0)) |> should be False
        Interval.intersects (Interval.make(2.0,3.5)) (Interval.make(3.0,4.0)) |> should be True
        Interval.intersects (Interval.make(2.0,4.0)) (Interval.make(3.0,4.0)) |> should be True
        Interval.intersects (Interval.make(2.0,5.0)) (Interval.make(3.0,4.0)) |> should be True
        Interval.intersects (Interval.make(3.0,5.0)) (Interval.make(3.0,4.0)) |> should be True
        Interval.intersects (Interval.make(3.5,5.0)) (Interval.make(3.0,4.0)) |> should be True
        Interval.intersects (Interval.make(4.0,5.0)) (Interval.make(3.0,4.0)) |> should be False
        Interval.intersects (Interval.make(4.5,5.0)) (Interval.make(3.0,4.0)) |> should be False
        Interval.intersects (Interval.make(3.2,3.7)) (Interval.make(3.0,4.0)) |> should be True

    [<Test>]
    member x.``I can determine if two intervals overlap`` () =
        Interval.overlaps (Interval.make(2.0,2.5)) (Interval.make(3.0,4.0)) |> should be (lessThan 0.0)
        Interval.overlaps (Interval.make(2.0,3.0)) (Interval.make(3.0,4.0)) |> should equal 0.0
        Interval.overlaps (Interval.make(2.0,3.5)) (Interval.make(3.0,4.0)) |> should be (greaterThan 0.0)
        Interval.overlaps (Interval.make(2.0,4.0)) (Interval.make(3.0,4.0)) |> should be (greaterThan 0.0)
        Interval.overlaps (Interval.make(2.0,5.0)) (Interval.make(3.0,4.0)) |> should be (greaterThan 0.0)
        Interval.overlaps (Interval.make(3.0,5.0)) (Interval.make(3.0,4.0)) |> should be (greaterThan 0.0)
        Interval.overlaps (Interval.make(3.5,5.0)) (Interval.make(3.0,4.0)) |> should be (greaterThan 0.0)
        Interval.overlaps (Interval.make(4.0,5.0)) (Interval.make(3.0,4.0)) |> should equal 0.0
        Interval.overlaps (Interval.make(4.5,5.0)) (Interval.make(3.0,4.0)) |> should be (lessThan 0.0)
        Interval.overlaps (Interval.make(3.2,3.7)) (Interval.make(3.0,4.0)) |> should be (greaterThan 0.0)

    
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
        let interval = Interval.Value.make(Some 7.0,Some 3.0) 
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
    
    [<Test>]
    member x.``I can determine if two time intervals intersect`` () =
        Interval.intersects (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(2.5))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be False
        Interval.intersects (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(3.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be False
        Interval.intersects (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(3.5))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be True
        Interval.intersects (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(4.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be True
        Interval.intersects (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be True
        Interval.intersects (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be True
        Interval.intersects (Interval.Time.make(d1.AddMinutes(3.5),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be True
        Interval.intersects (Interval.Time.make(d1.AddMinutes(4.0),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be False
        Interval.intersects (Interval.Time.make(d1.AddMinutes(4.5),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be False
        Interval.intersects (Interval.Time.make(d1.AddMinutes(3.2),d1.AddMinutes(3.7))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be True

    [<Test>]
    member x.``I can determine if two time intervals overlap`` () =
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(2.5))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be (lessThan TimeSpan.Zero)
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(3.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should equal TimeSpan.Zero
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(3.5))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be (greaterThan TimeSpan.Zero)
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(4.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be (greaterThan TimeSpan.Zero)
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(2.0),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be (greaterThan TimeSpan.Zero)
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be (greaterThan TimeSpan.Zero)
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(3.5),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be (greaterThan TimeSpan.Zero)
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(4.0),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should equal TimeSpan.Zero
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(4.5),d1.AddMinutes(5.0))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be (lessThan TimeSpan.Zero)
        Interval.overlaps (Interval.Time.make(d1.AddMinutes(3.2),d1.AddMinutes(3.7))) (Interval.Time.make(d1.AddMinutes(3.0),d1.AddMinutes(4.0))) |> should be (greaterThan TimeSpan.Zero)
        