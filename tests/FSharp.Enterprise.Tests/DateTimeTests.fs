namespace FSharp.Enterprise.Tests

open System
open NUnit.Framework
open FsUnit
open FSharp.Enterprise

[<TestFixture>]
type ``Given a DateTime``() = 
 
    let singleMonth month (granularity : TimeSpan) = 
          let s = (DateTimeOffset(2012, month, 01, 0, 0, 0, TimeSpan.FromHours(0.)))
          let e = s.AddMonths(1)
          let c = e.Subtract(s).TotalMinutes / granularity.TotalMinutes |> int
          s, new ResizeArray<_>(Seq.init c (fun i -> Nullable<_>(i |> float32)))
    
    [<Test>]
    member t.``I should be able to calculate the unix ticks``() = 
        let actual = DateTime(2012,1,1)
        actual.ToUnixTicks() |> should equal 1325376000000L

    [<Test>]
    member test.``Should Compute Long index with short index at None within the current year``() = 
        let (shortDayIndex, longDayIndex) = DateTime.computeShortLongHourIndicies (DateTimeOffset(DateTime.LongDay(2012).Ticks, TimeSpan.FromHours(0.))) (TimeSpan.FromHours(1.)) 
        shortDayIndex |> should equal None
        longDayIndex |> should equal (Some(2))
    
    [<Test>]
    member test.``Should Compute Short index and long index within the current year``() = 
        let (shortDayIndex, longDayIndex) = DateTime.computeShortLongHourIndicies (DateTimeOffset(DateTime.ShortDay(2012).Ticks, TimeSpan.FromHours(0.))) (TimeSpan.FromHours(1.)) 
        shortDayIndex |> should equal (Some(2))
        longDayIndex |> should equal (Some((DateTime.LongDay(2012).Subtract(DateTime.ShortDay(2012)).TotalHours |> int) + 2))

    [<Test>]
    member tet.``Does not effect a series that does cross a boundary``() = 
        let (_,data) = singleMonth 1 (TimeSpan.FromHours(1.))
        let original = data |> Seq.toList
        let series =  DateTime.normaliseForClockChange (DateTimeOffset(2012, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))) (TimeSpan.FromHours(1.)) data

        (Seq.length series) |> should equal original.Length
    
    [<Test>]
    member test.``Can handle both short and long day clock changes when five minute granularity``() =
          let (s, e, timeSpan) = (DateTimeOffset(2012, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (DateTimeOffset(2013, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromMinutes(5.))
          let hours = (e.Subtract(s).TotalMinutes / 5.) |> int
          let (sindex, lindex) = DateTime.computeShortLongHourIndicies s timeSpan
          let series = new ResizeArray<_>(Seq.init hours (fun i -> Nullable<_>(i |> float32)))

          //Add long hour and remove short
          series.RemoveRange(sindex.Value, 12)
          series.InsertRange(lindex.Value, Seq.init 12 (fun i -> Nullable<_>(1000000 |> float32)))

          105408 |> should equal series.Count
          let v = DateTime.normaliseForClockChange s timeSpan series |> Seq.toArray  
          Array.FindIndex(v, fun x -> x = Nullable<float32>()) |> should equal sindex.Value
          ((v |> Seq.toArray).[sindex.Value..sindex.Value + 11]) |> should equal (Array.init 12 (fun i -> Nullable<float32>()))
          Array.FindIndex(v, fun x -> x = Nullable<float32>(1000000.f)) |> should equal -1

    [<Test>]
    member test.``Can handle both short and long day clock changes when hourly ``() =
          let (s, e, timeSpan) = (DateTimeOffset(2012, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (DateTimeOffset(2013, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromHours(1.))
          let hours = e.Subtract(s).TotalHours |> int
          let (sindex, lindex) = DateTime.computeShortLongHourIndicies s timeSpan
          let series = new ResizeArray<_>(Seq.init hours (fun i -> Nullable<_>(i |> float32)))

          //Add long hour and remove short
          series.RemoveAt(sindex.Value)
          series.Insert(lindex.Value, Nullable<_>(10000 |> float32))

          8784 |> should equal series.Count
          let v = new ResizeArray<_>(DateTime.normaliseForClockChange s timeSpan (series |> List.ofSeq))
          v.FindIndex(fun x -> x = Nullable<float32>()) |> should equal sindex.Value
          v.FindIndex(fun x -> x = Nullable<float32>(10000.f)) |> should equal -1

    [<Test>]
    member test.``Long clock change day has 24 hours after being corrected``() = 
        let startDate, timeSpan = (DateTimeOffset(2012, 10, 1, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromHours(1.))
        let (_,march) = singleMonth startDate.Month timeSpan
        let (_,index) = DateTime.computeShortLongHourIndicies startDate timeSpan
        //A full 31 day month should have 744 hours
        744 |> should equal march.Count
        
        march.Insert(index.Value, Nullable<_>(10000 |> float32))
        745 |> should equal march.Count

        let v = DateTime.normaliseForClockChange startDate timeSpan  march |> Seq.toArray
        744 |> should equal v.Length
        Array.sub v index.Value 1 |> should equal [|Nullable<float32>(index.Value |> float32)|]
 
    [<Test>]
    member test.``Long clock change day has 288 five minute intervals after being corrected``() = 
        let startDate, timeSpan = (DateTimeOffset(2012, 10, 1, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromMinutes(5.))
        let (_,march) = singleMonth startDate.Month timeSpan
        let (_,index) = DateTime.computeShortLongHourIndicies startDate timeSpan
        //A full 31 day month should have 8928 five minutes
        8928 |> should equal march.Count
        //Remove clock change hour this is 25 days + 2 hours in (602 hours)
        march.InsertRange(index.Value, (Array.init 12 (fun i -> Nullable<float32>(100000 + i |> float32))))
        8940 |> should equal march.Count

        let v = DateTime.normaliseForClockChange startDate timeSpan march |> Seq.toArray
        8928 |> should equal (v.Length)
        Array.sub v index.Value 24 |> should equal (Array.init 24 (fun i -> Nullable<float32>(index.Value + i |> float32)))     


    [<Test>]
    member test.``Short clock change day has 24 hours after being corrected``() = 
        let startDate, timeSpan = (DateTimeOffset(2012, 03, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromHours(1.))
        let (_,march) = singleMonth startDate.Month timeSpan
        let (index,_) = DateTime.computeShortLongHourIndicies startDate timeSpan
        //A full 31 day month should have 744 hours
        744 |> should equal march.Count
        //Remove clock change hour this is 25 days + 2 hours in (602 hours)
        march.RemoveAt(index.Value)
        743 |> should equal march.Count

        let v = DateTime.normaliseForClockChange startDate timeSpan  march |> Seq.toArray
        744 |> should equal v.Length
        Array.sub v index.Value 1 |> should equal [|Nullable<float32>()|]
 
    [<Test>]
    member test.``Short clock change day has 288 five minute intervals after being corrected``() = 
        let startDate, timeSpan = (DateTimeOffset(2012, 03, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromMinutes(5.))
        let (_,march) = singleMonth startDate.Month timeSpan
        let (index,_) = DateTime.computeShortLongHourIndicies startDate timeSpan
        //A full 31 day month should have 8928 five minutes
        8928 |> should equal march.Count
        //Remove clock change hour this is 25 days + 2 hours in (602 hours)
        march.RemoveRange(index.Value, 12)
        8916 |> should equal march.Count

        let v = DateTime.normaliseForClockChange startDate timeSpan  march |> Seq.toArray
        8928 |> should equal v.Length
        Array.sub v index.Value 12 |> should equal (Array.init 12 (fun _ -> Nullable<float32>()))    

