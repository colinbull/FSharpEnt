namespace FSharp.Enterprise.Tests

open System
open NUnit.Framework
open FsUnit
open FSharp.Enterprise.Cron

[<TestFixture>]
type ``Given a cron expression``() =
         
             
    [<Test>]
    member t.``I should be able create an AST``()=
         let expr = "0/5 14,18,3-39,52 * * JAN,MAR,SEP MON-FRI 2002-2010"
         let actual = parse expr
         let expected = 
            [
                Increment("0","5")
                List([Value("14"); Value("18"); Range("3","39"); Value("52")])
                Every
                Every
                List([Value("JAN"); Value("MAR"); Value("SEP")])
                Range("MON", "FRI")
                Range("2002","2010")
            ]
         actual |> should equal expected

    [<Test>]
    member t.``I Should be able to create a seq based on for days in a month``() =
        let expr = "0 0 0 1/5 JAN ? *"
        let startDate = new DateTime(2012,1,1) 

        let actual = toSeqAsOf startDate expr |> Seq.take 7 |> Seq.toList
        let expected = 
            [
                new DateTime(2012, 1, 1); new DateTime(2012, 1, 6); new DateTime(2012, 1, 11);
                new DateTime(2012, 1, 16); new DateTime(2012, 1, 21); new DateTime(2012, 1, 26);
                new DateTime(2012, 1, 31);
            ]

        actual |> should equal expected

    [<Test>]
    member t.``I Should be able to create a seq based on a complex cron``() =
        let expr = "0 0 1 14,18 JAN,MAR,SEP ? *"
        let startDate = new DateTime(2012,1,1) 

        let actual = toSeqAsOf startDate expr |> Seq.take 7 |> Seq.toList
        let expected = 
            [
             new DateTime(2012, 1, 14, 1, 0, 0); new DateTime(2012, 1, 18, 1, 0, 0);
             new DateTime(2012, 3, 14, 1, 0, 0); new DateTime(2012, 3, 18, 1, 0, 0);
             new DateTime(2012, 9, 14, 1, 0, 0); new DateTime(2012, 9, 18, 1, 0, 0);
             new DateTime(2013, 1, 14, 1, 0, 0)
            ]

        actual |> should equal expected


