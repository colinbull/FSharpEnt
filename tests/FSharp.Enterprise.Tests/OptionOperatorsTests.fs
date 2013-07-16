namespace FSharp.Enterprise.Tests

open NUnit.Framework
open FsUnit
open System
open FSharp.Enterprise.OptionOperators
      
[<TestFixture; Category("Unit")>]
type ``Given the ?<=? operator`` () =

    [<Test>]
    member x.``I can compare two some values`` () =
        Some 0 ?<=? Some 0 |> should be True 
        Some 0 ?<=? Some 1 |> should be True
        Some 1 ?<=? Some 0 |> should be False

    [<Test>]
    member x.``I can compare some value with a none value`` () =
        None ?<=? Some 0 |> should be False
        Some 0 ?<=? None |> should be False

    [<Test>]
    member x.``I can compare two none values`` () =    
        None ?<=? None |> should be False

[<TestFixture; Category("Unit")>]
type ``Given the ?>=? operator`` () =

    [<Test>]
    member x.``I can compare two some values`` () =
        Some 0 ?>=? Some 0 |> should be True 
        Some 0 ?>=? Some 1 |> should be False
        Some 1 ?>=? Some 0 |> should be True

    [<Test>]
    member x.``I can compare some value with a none value`` () =
        None ?>=? Some 0 |> should be False
        Some 0 ?>=? None |> should be False

    [<Test>]
    member x.``I can compare two none values`` () =    
        None ?>=? None |> should be False

[<TestFixture; Category("Unit")>]
type ``Given the ?>? operator`` () =

    [<Test>]
    member x.``I can compare two some values`` () =
        Some 0 ?>? Some 0 |> should be False 
        Some 0 ?>? Some 1 |> should be False
        Some 1 ?>? Some 0 |> should be True

    [<Test>]
    member x.``I can compare some value with a none value`` () =
        None ?>? Some 1 |> should be False
        Some 1 ?>? None |> should be False

    [<Test>]
    member x.``I can compare two none values`` () =    
        None ?>? None |> should be False

[<TestFixture; Category("Unit")>]
type ``Given the ?<? operator`` () =

    [<Test>]
    member x.``I can compare two some values`` () =
        Some 0 ?<? Some 0 |> should be False 
        Some 0 ?<? Some 1 |> should be True
        Some 1 ?<? Some 0 |> should be False

    [<Test>]
    member x.``I can compare some value with a none value`` () =
        None ?<? Some 1 |> should be False
        Some 1 ?<? None |> should be False

    [<Test>]
    member x.``I can compare two none values`` () =    
        None ?<? None |> should be False

[<TestFixture; Category("Unit")>]
type ``Given the ?-? operator`` () =

    [<Test>]
    member x.``I can subtract two some values`` () =
        Some 10 ?-? Some 10 |> should equal (Some 0) 
        Some 7 ?-? Some 10 |> should equal (Some -3) 
        Some 10 ?-? Some 7 |> should equal (Some 3)

    [<Test>]
    member x.``I can substract some value with a none value`` () =
        None ?-? Some 10 |> should equal None
        Some 10 ?-? None |> should equal None

    [<Test>]
    member x.``I can subtract two none values`` () =    
        None ?-? None |> should equal None

[<TestFixture; Category("Unit")>]
type ``Given the equalWithin function`` () =

    [<Test>]
    member x.``I can test equality of two some values within a tolerance`` () =
        equalWithin 0.001 (Some 10.0) (Some 10.0) |> should be True 
        equalWithin 0.001 (Some 10.0) (Some 10.0009) |> should be True 
        equalWithin 0.001 (Some 10.0) (Some 10.001) |> should be True 
        equalWithin 0.001 (Some 10.0) (Some 10.0011) |> should be False 

    [<Test>]
    member x.``I can test equality of a some value with a none value within a tolerance`` () =
        equalWithin 0.001 (Some 10.0) None |> should be False 
        equalWithin 0.001 None (Some 10.0) |> should be False 

    [<Test>]
    member x.``I can test equality of two none values within a tolerance`` () =
        equalWithin 0.001 None None |> should be True 
    