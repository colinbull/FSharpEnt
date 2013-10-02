namespace FSharp.Enterprise.Tests

open NUnit.Framework
open FsUnit
open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharp.Enterprise

[<TestFixture; Category("Unit")>]
type ``Given a single lookup`` () =
    
    let source = LookUp.Single.ofSeq [(1.,0.);(2.,1.);(3.,4.)]

    let keys = [|  0.0<J>;100.0<J>;200.0<J>;300.0<J>;400.0<J>;500.0<J>|]
    let values = [|  0.0<J>; 10.0<J>; 20.0<J>; 30.0<J>; 40.0<J>; 50.0<J>|]
    let table = Array.zip keys values |> LookUp.Single.ofSeq

    [<Test>]
    member x.``I can find a value for a key``() =
        // Internal keys
        table |> LookUp.Single.tryFindInterpolated   0.0<J> |> should equal (Some  0.0<J>)        
        table |> LookUp.Single.tryFindInterpolated 100.0<J> |> should equal (Some 10.0<J>)        
        table |> LookUp.Single.tryFindInterpolated 200.0<J> |> should equal (Some 20.0<J>)        
        table |> LookUp.Single.tryFindInterpolated 300.0<J> |> should equal (Some 30.0<J>)        
        table |> LookUp.Single.tryFindInterpolated 400.0<J> |> should equal (Some 40.0<J>)        
        table |> LookUp.Single.tryFindInterpolated 500.0<J> |> should equal (Some 50.0<J>)
        // Internal interpolated
        table |> LookUp.Single.tryFindInterpolated  50.0<J> |> should equal (Some  5.0<J>)        
        table |> LookUp.Single.tryFindInterpolated 150.0<J> |> should equal (Some 15.0<J>)        
        table |> LookUp.Single.tryFindInterpolated 250.0<J> |> should equal (Some 25.0<J>)        
        table |> LookUp.Single.tryFindInterpolated 350.0<J> |> should equal (Some 35.0<J>)        
        table |> LookUp.Single.tryFindInterpolated 450.0<J> |> should equal (Some 45.0<J>)        
        // External
        table |> LookUp.Single.tryFindInterpolated  -1.0<J> |> should equal None        
        table |> LookUp.Single.tryFindInterpolated 501.0<J> |> should equal None        

    [<Test>]
    member x.``I can find a value from a given key``() =
        LookUp.Single.tryFind 2. source |> should equal (Some 1.)

    [<Test>]
    member x.``I can find an interpolated value from a key``() = 
        LookUp.Single.tryFindInterpolated 1.5 source |> should equal (Some 0.5)

    [<Test>]
    member x.``I can filter a given lookup``() = 
        let expected = 
            LookUp.Single.ofSeq [(3.,4.)]

        let actual = 
            LookUp.Single.filter (fun k _ -> k > 2.) source

        actual |> should equal expected

    [<Test>]
    member x.``I can map over a given lookup``() =
        let expected = 
            LookUp.Single.ofSeq [(2.,0.);(4.,2.);(6.,8.)]

        let actual = 
            LookUp.Single.map (fun (k,v) -> k * 2., v * 2.) source

        actual |> should equal expected

type ``Given a double lookup`` () =

    let pdtPositive = 
        [
            0.1<Hz>, [  
                      0.0<J>, 10.0<J s>;
                    100.0<J>, 20.0<J s>;
                    200.0<J>, 30.0<J s>;
                    300.0<J>, 40.0<J s>;
                    400.0<J>, 50.0<J s>;
                    500.0<J>, 60.0<J s>
                ];
            0.2<Hz>, [  
                      0.0<J>, 11.0<J s>;
                    100.0<J>, 21.0<J s>;
                    200.0<J>, 31.0<J s>;
                    300.0<J>, 41.0<J s>;
                    400.0<J>, 51.0<J s>;
                    500.0<J>, 61.0<J s>
                ];
            0.3<Hz>, [  
                      0.0<J>, 12.0<J s>;
                    100.0<J>, 22.0<J s>;
                    200.0<J>, 32.0<J s>;
                    300.0<J>, 42.0<J s>;
                    400.0<J>, 52.0<J s>;
                    500.0<J>, 62.0<J s>
                ];
            0.4<Hz>, [  
                      0.0<J>, 13.0<J s>;
                    100.0<J>, 23.0<J s>;
                    200.0<J>, 33.0<J s>;
                    300.0<J>, 43.0<J s>;
                    400.0<J>, 53.0<J s>;
                    500.0<J>, 63.0<J s>
                ];
            0.5<Hz>, [  
                      0.0<J>, 14.0<J s>;
                    100.0<J>, 24.0<J s>;
                    200.0<J>, 34.0<J s>;
                    300.0<J>, 44.0<J s>;
                    400.0<J>, 54.0<J s>;
                    500.0<J>, 64.0<J s>
                ]
        ] |> LookUp.Double.ofSeq

    let pdtNegative = 
        [
            -0.1<Hz>, [  
                      0.0<J>, 10.0<J s>;
                    100.0<J>, 20.0<J s>;
                    200.0<J>, 30.0<J s>;
                    300.0<J>, 40.0<J s>;
                    400.0<J>, 50.0<J s>;
                    500.0<J>, 60.0<J s>
                ];
            -0.2<Hz>, [  
                      0.0<J>, 11.0<J s>;
                    100.0<J>, 21.0<J s>;
                    200.0<J>, 31.0<J s>;
                    300.0<J>, 41.0<J s>;
                    400.0<J>, 51.0<J s>;
                    500.0<J>, 61.0<J s>
                ];
            -0.3<Hz>, [  
                      0.0<J>, 12.0<J s>;
                    100.0<J>, 22.0<J s>;
                    200.0<J>, 32.0<J s>;
                    300.0<J>, 42.0<J s>;
                    400.0<J>, 52.0<J s>;
                    500.0<J>, 62.0<J s>
                ];
            -0.4<Hz>, [  
                      0.0<J>, 13.0<J s>;
                    100.0<J>, 23.0<J s>;
                    200.0<J>, 33.0<J s>;
                    300.0<J>, 43.0<J s>;
                    400.0<J>, 53.0<J s>;
                    500.0<J>, 63.0<J s>
                ];
            -0.5<Hz>, [  
                      0.0<J>, 14.0<J s>;
                    100.0<J>, 24.0<J s>;
                    200.0<J>, 34.0<J s>;
                    300.0<J>, 44.0<J s>;
                    400.0<J>, 54.0<J s>;
                    500.0<J>, 64.0<J s>
                ]

        ]
        |> LookUp.Double.ofSeq

    [<Test>]
    member x.``I can find a value from the given keys - top corners``() =
        // Top corners
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.1<Hz>   0.0<J> |> should equal (Some 10.0<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.2<Hz>   0.0<J> |> should equal (Some 11.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.3<Hz>   0.0<J> |> should equal (Some 12.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.4<Hz>   0.0<J> |> should equal (Some 13.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.5<Hz>   0.0<J> |> should equal (Some 14.0<J s>)

    [<Test>]
    member x.``I can find a value from the given keys - bottom corners``() =
        // Bottom corners
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.1<Hz> 500.0<J> |> should equal (Some 60.0<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.2<Hz> 500.0<J> |> should equal (Some 61.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.3<Hz> 500.0<J> |> should equal (Some 62.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.4<Hz> 500.0<J> |> should equal (Some 63.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.5<Hz> 500.0<J> |> should equal (Some 64.0<J s>)

    [<Test>]
    member x.``I can find a value from the given keys - inner internal``() =
        // Inner internal cases
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.1<Hz> 300.0<J> |> should equal (Some 40.0<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.2<Hz> 300.0<J> |> should equal (Some 41.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.3<Hz> 300.0<J> |> should equal (Some 42.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.4<Hz> 300.0<J> |> should equal (Some 43.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.5<Hz> 300.0<J> |> should equal (Some 44.0<J s>)

    [<Test>]
    member x.``I can find a value from the given keys - inner external``() =
        // Inner external cases
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.1<Hz>  -1.0<J> |> should equal None 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.2<Hz>  -1.0<J> |> should equal None
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.3<Hz>  -1.0<J> |> should equal None
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.4<Hz>  -1.0<J> |> should equal None
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.5<Hz>  -1.0<J> |> should equal None
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.1<Hz> 501.0<J> |> should equal None 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.2<Hz> 501.0<J> |> should equal None
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.3<Hz> 501.0<J> |> should equal None
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.4<Hz> 501.0<J> |> should equal None
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.5<Hz> 501.0<J> |> should equal None

    [<Test>]
    member x.``I can find a value from the given keys - outer external``() =
        // Outer external cases
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.0<Hz>   0.0<J> |> should equal None 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.6<Hz>   0.0<J> |> should equal None
        // Both external cases
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.0<Hz> 501.0<J> |> should equal None 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.6<Hz> 501.0<J> |> should equal None

    [<Test>]
    member x.``I can find a value from the given keys - interpolate outer``() =
        // Interpolate outer
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.15<Hz>   0.0<J> |> should equal (Some 10.5<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.25<Hz>   0.0<J> |> should equal (Some 11.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.35<Hz>   0.0<J> |> should equal (Some 12.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.45<Hz>   0.0<J> |> should equal (Some 13.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.15<Hz> 300.0<J> |> should equal (Some 40.5<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.25<Hz> 300.0<J> |> should equal (Some 41.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.35<Hz> 300.0<J> |> should equal (Some 42.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.45<Hz> 300.0<J> |> should equal (Some 43.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.15<Hz> 500.0<J> |> should equal (Some 60.5<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.25<Hz> 500.0<J> |> should equal (Some 61.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.35<Hz> 500.0<J> |> should equal (Some 62.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.45<Hz> 500.0<J> |> should equal (Some 63.5<J s>)

    [<Test>]
    member x.``I can find a value from the given keys - interpolate inner``() =
        // Interpolate inner
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.1<Hz>  50.0<J> |> should equal (Some 15.0<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.2<Hz>  50.0<J> |> should equal (Some 16.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.3<Hz>  50.0<J> |> should equal (Some 17.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.4<Hz>  50.0<J> |> should equal (Some 18.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.5<Hz>  50.0<J> |> should equal (Some 19.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.1<Hz> 250.0<J> |> should equal (Some 35.0<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.2<Hz> 250.0<J> |> should equal (Some 36.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.3<Hz> 250.0<J> |> should equal (Some 37.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.4<Hz> 250.0<J> |> should equal (Some 38.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.5<Hz> 250.0<J> |> should equal (Some 39.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.1<Hz> 450.0<J> |> should equal (Some 55.0<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.2<Hz> 450.0<J> |> should equal (Some 56.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.3<Hz> 450.0<J> |> should equal (Some 57.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.4<Hz> 450.0<J> |> should equal (Some 58.0<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.5<Hz> 450.0<J> |> should equal (Some 59.0<J s>)

    [<Test>]
    member x.``I can find a value from the given keys - interpolate both``() =
        // Interpolate both
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.15<Hz>  50.0<J> |> should equal (Some 15.5<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.25<Hz>  50.0<J> |> should equal (Some 16.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.35<Hz>  50.0<J> |> should equal (Some 17.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.45<Hz>  50.0<J> |> should equal (Some 18.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.15<Hz> 250.0<J> |> should equal (Some 35.5<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.25<Hz> 250.0<J> |> should equal (Some 36.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.35<Hz> 250.0<J> |> should equal (Some 37.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.45<Hz> 250.0<J> |> should equal (Some 38.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.15<Hz> 450.0<J> |> should equal (Some 55.5<J s>) 
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.25<Hz> 450.0<J> |> should equal (Some 56.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.35<Hz> 450.0<J> |> should equal (Some 57.5<J s>)
        pdtPositive |> LookUp.Double.tryFindInterpolated 0.45<Hz> 450.0<J> |> should equal (Some 58.5<J s>)

    [<Test>]
    member x.``I can find a value from the given negative keys - top corners``() =
        // Top corners
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.1<Hz>   0.0<J> |> should equal (Some 10.0<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.2<Hz>   0.0<J> |> should equal (Some 11.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.3<Hz>   0.0<J> |> should equal (Some 12.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.4<Hz>   0.0<J> |> should equal (Some 13.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.5<Hz>   0.0<J> |> should equal (Some 14.0<J s>)

    [<Test>]
    member x.``I can find a value from the given negative keys - bottom corners``() =
        // Bottom corners
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.1<Hz> 500.0<J> |> should equal (Some 60.0<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.2<Hz> 500.0<J> |> should equal (Some 61.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.3<Hz> 500.0<J> |> should equal (Some 62.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.4<Hz> 500.0<J> |> should equal (Some 63.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.5<Hz> 500.0<J> |> should equal (Some 64.0<J s>)

    [<Test>]
    member x.``I can find a value from the given negative keys - inner internal``() =
        // Inner internal cases
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.1<Hz> 300.0<J> |> should equal (Some 40.0<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.2<Hz> 300.0<J> |> should equal (Some 41.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.3<Hz> 300.0<J> |> should equal (Some 42.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.4<Hz> 300.0<J> |> should equal (Some 43.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.5<Hz> 300.0<J> |> should equal (Some 44.0<J s>)

    [<Test>]
    member x.``I can find a value from the given negative keys - inner external``() =
        // Inner external cases
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.1<Hz>  -1.0<J> |> should equal None 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.2<Hz>  -1.0<J> |> should equal None
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.3<Hz>  -1.0<J> |> should equal None
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.4<Hz>  -1.0<J> |> should equal None
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.5<Hz>  -1.0<J> |> should equal None
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.1<Hz> 501.0<J> |> should equal None 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.2<Hz> 501.0<J> |> should equal None
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.3<Hz> 501.0<J> |> should equal None
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.4<Hz> 501.0<J> |> should equal None
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.5<Hz> 501.0<J> |> should equal None

    [<Test>]
    member x.``I can find a value from the given negative keys - outer external``() =
        // Outer external cases
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.0<Hz>   0.0<J> |> should equal None 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.6<Hz>   0.0<J> |> should equal None
        // Both external cases
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.0<Hz> 501.0<J> |> should equal None 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.6<Hz> 501.0<J> |> should equal None

    [<Test>]
    member x.``I can find a value from the given negative keys - interpolate outer``() =
        // Interpolate outer
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.15<Hz>   0.0<J> |> should equal (Some 10.5<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.25<Hz>   0.0<J> |> should equal (Some 11.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.35<Hz>   0.0<J> |> should equal (Some 12.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.45<Hz>   0.0<J> |> should equal (Some 13.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.15<Hz> 300.0<J> |> should equal (Some 40.5<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.25<Hz> 300.0<J> |> should equal (Some 41.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.35<Hz> 300.0<J> |> should equal (Some 42.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.45<Hz> 300.0<J> |> should equal (Some 43.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.15<Hz> 500.0<J> |> should equal (Some 60.5<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.25<Hz> 500.0<J> |> should equal (Some 61.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.35<Hz> 500.0<J> |> should equal (Some 62.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.45<Hz> 500.0<J> |> should equal (Some 63.5<J s>)

    [<Test>]
    member x.``I can find a value from the given negative keys - interpolate inner``() =
        // Interpolate inner
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.1<Hz>  50.0<J> |> should equal (Some 15.0<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.2<Hz>  50.0<J> |> should equal (Some 16.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.3<Hz>  50.0<J> |> should equal (Some 17.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.4<Hz>  50.0<J> |> should equal (Some 18.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.5<Hz>  50.0<J> |> should equal (Some 19.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.1<Hz> 250.0<J> |> should equal (Some 35.0<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.2<Hz> 250.0<J> |> should equal (Some 36.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.3<Hz> 250.0<J> |> should equal (Some 37.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.4<Hz> 250.0<J> |> should equal (Some 38.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.5<Hz> 250.0<J> |> should equal (Some 39.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.1<Hz> 450.0<J> |> should equal (Some 55.0<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.2<Hz> 450.0<J> |> should equal (Some 56.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.3<Hz> 450.0<J> |> should equal (Some 57.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.4<Hz> 450.0<J> |> should equal (Some 58.0<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.5<Hz> 450.0<J> |> should equal (Some 59.0<J s>)

    [<Test>]
    member x.``I can find a value from the given negative keys - interpolate both``() =
        // Interpolate both
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.15<Hz>  50.0<J> |> should equal (Some 15.5<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.25<Hz>  50.0<J> |> should equal (Some 16.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.35<Hz>  50.0<J> |> should equal (Some 17.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.45<Hz>  50.0<J> |> should equal (Some 18.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.15<Hz> 250.0<J> |> should equal (Some 35.5<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.25<Hz> 250.0<J> |> should equal (Some 36.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.35<Hz> 250.0<J> |> should equal (Some 37.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.45<Hz> 250.0<J> |> should equal (Some 38.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.15<Hz> 450.0<J> |> should equal (Some 55.5<J s>) 
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.25<Hz> 450.0<J> |> should equal (Some 56.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.35<Hz> 450.0<J> |> should equal (Some 57.5<J s>)
        pdtNegative |> LookUp.Double.tryFindInterpolated -0.45<Hz> 450.0<J> |> should equal (Some 58.5<J s>)

    [<Test>]
    member x.``I can find a value``() =
        let pdt = 
            [
                0.1<Hz>, [  
                          0.0<J>,   0.0<J s>;
                        100.0<J>,  10.0<J s>;
                        200.0<J>,  20.0<J s>;
                        300.0<J>, 100.0<J s>;
                        400.0<J>, 150.0<J s>;
                        500.0<J>, 175.0<J s>
                    ];
                0.2<Hz>, [  
                          0.0<J>,   0.0<J s>;
                        100.0<J>,  20.0<J s>;
                        200.0<J>,  40.0<J s>;
                        300.0<J>, 200.0<J s>;
                        400.0<J>, 250.0<J s>;
                        500.0<J>, 275.0<J s>
                    ];
                0.3<Hz>, [  
                          0.0<J>,   0.0<J s>;
                        100.0<J>,  30.0<J s>;
                        200.0<J>,  60.0<J s>;
                        300.0<J>, 300.0<J s>;
                        400.0<J>, 350.0<J s>;
                        500.0<J>, 375.0<J s>
                    ];
                0.4<Hz>, [  
                          0.0<J>,   0.0<J s>;
                        100.0<J>,  40.0<J s>;
                        200.0<J>,  80.0<J s>;
                        300.0<J>, 400.0<J s>;
                        400.0<J>, 450.0<J s>;
                        500.0<J>, 475.0<J s>
                    ];
                0.5<Hz>, [  
                          0.0<J>,   0.0<J s>;
                        100.0<J>,  50.0<J s>;
                        200.0<J>, 100.0<J s>;
                        300.0<J>, 500.0<J s>;
                        400.0<J>, 550.0<J s>;
                        500.0<J>, 575.0<J s>
                    ]
            ] |> LookUp.Double.ofSeq
        let actual = LookUp.Double.tryFindInterpolated 0.48<Hz> 95.1042<J> pdt
        let expected = Some 45.65<J s>
        actual.Value |> should (equalWithin 0.01) expected.Value
