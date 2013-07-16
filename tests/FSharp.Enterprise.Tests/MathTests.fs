namespace FSharp.Enterprise.Tests

open NUnit.Framework
open FsUnit
open System
open FSharp.Enterprise

[<TestFixture; Category("Unit")>]
type ``Given the Math module linear interpolation function`` () =

    [<Test>]
    member x.``I can perform linear interpolation within range`` () =
        let actual = Math.Interpolation.linear 17.0 0.0 0.0 100.0 100.0
        let expected = 17.0
        actual |> should (equalWithin 0.001) expected

    [<Test>]
    member x.``I can perform linear interpolation after range`` () =
        let actual = Math.Interpolation.linear 150.0 0.0 0.0 100.0 100.0
        let expected = 150.0
        actual |> should (equalWithin 0.001) expected

    [<Test>]
    member x.``I can perform linear interpolation before range`` () =
        let actual = Math.Interpolation.linear -150.0 0.0 0.0 100.0 100.0
        let expected = -150.0
        actual |> should (equalWithin 0.001) expected

    [<Test>]
    member x.``I can perform linear interpolation at range start`` () =
        let actual = Math.Interpolation.linear 0.0 0.0 0.0 100.0 100.0
        let expected = 0.0
        actual |> should (equalWithin 0.001) expected

    [<Test>]
    member x.``I can perform linear interpolation at range end`` () =
        let actual = Math.Interpolation.linear 100.0 0.0 0.0 100.0 100.0
        let expected = 100.0
        actual |> should (equalWithin 0.001) expected

    [<Test>]
    member x.``I can perform linear interpolation on a horizontal`` () =
        let actual = Math.Interpolation.linear 100.0 0.0 0.0 100.0 0.0
        let expected = 0.0
        actual |> should (equalWithin 0.001) expected

    [<Test>]
    member x.``I can perform linear interpolation on a vertical`` () =
        let actual = Math.Interpolation.linear 100.0 0.0 0.0 0.0 100.0
        let expected = Double.PositiveInfinity
        actual |> should (equalWithin 0.001) expected

    [<Test>]
    member x.``I can perform linear interpolation on a negative slope`` () =
        let actual = Math.Interpolation.linear 17.0 0.0 100.0 100.0 0.0
        let expected = 83.0
        actual |> should (equalWithin 0.001) expected

    [<Test>]
    member x.``I can perform linear interpolation on reversed points`` () =
        let actual = Math.Interpolation.linear 17.0 100.0 0.0 0.0 100.0
        let expected = 83.0
        actual |> should (equalWithin 0.001) expected


[<TestFixture; Category("Unit")>]
type ``Given the Math module bilinear interpolation function`` () =

    [<Test>]
    member x.``I can perform bilinear interpolation within range`` () =
        let actual = Math.Interpolation.bilinear 14.5 20.2 162. 95. 91. 210. 14. 21. 15. 20.
        let expected = 146.1
        actual |> should (equalWithin 0.001) expected             
    


[<TestFixture; Category("Unit")>]
type ``Given the Math module intersection`` () =

    [<Test>]
    member x.``I can determine the intersection point of two lines that cross within their range`` () =
        let actual = Math.intersection 0. 0. 100. 100. 0. 100. 100. 0.
        let expected = Some(50.,50.)
        actual |> should equal expected

    [<Test>]
    member x.``I can determine the intersection point of two lines that cross outside their range`` () =
        let actual = Math.intersection 0. 0. 100. 40. 0. 100. 100. 60.
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can determine the intersection point of vertical and horizontal lines that cross`` () =
        let actual = Math.intersection 50. 0. 50. 100. 0. 50. 100. 50.
        let expected = Some(50.,50.)
        actual |> should equal expected

    [<Test>]
    member x.``I can determine that two horizontal lines that overlap do not intersect`` () =
        let actual = Math.intersection 0. 50. 60. 50. 40. 50. 100. 50.
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can determine that two vertical lines that overlap do not intersect`` () =
        let actual = Math.intersection 0. 0. 0. 60. 0. 40. 0. 100.
        let expected = None
        actual |> should equal expected

    [<Test>]
    member x.``I can determine that lines that meet at their start points intersect`` () =
        let actual = Math.intersection 0. 0. 100. 100. 0. 0. 100. 50.
        let expected = Some(0.,0.)
        actual |> should equal expected

    [<Test>]
    member x.``I can determine that lines that meet at their end points intersect`` () =
        let actual = Math.intersection 0. 0. 100. 100. 50. 0. 100. 100.
        let expected = Some(100.,100.)
        actual |> should equal expected
