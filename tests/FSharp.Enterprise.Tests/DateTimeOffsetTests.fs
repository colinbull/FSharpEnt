namespace FSharp.Enterprise.Tests

open NUnit.Framework
open FsUnit
open System
open FSharp.Enterprise
open FSharp.Enterprise.DateTime
open FSharp.Enterprise.DateTimeOffset

      
[<TestFixture; Category("Unit")>]
type ``Given a DateTimeOffset`` () =

    let date = DateTimeOffset(2013,5,8,23,0,0,TimeSpan.FromHours(0.0))

    let singleMonth month (granularity : TimeSpan) = 
      let s = (DateTimeOffset(2012, month, 01, 0, 0, 0, TimeSpan.FromHours(0.)))
      let e = s.AddMonths(1)
      let c = e.Subtract(s).TotalMinutes / granularity.TotalMinutes |> int
      s, new ResizeArray<_>(Seq.init c (fun i -> Nullable<_>(i |> float32)))

    [<Test>]
    member x.``I can round down to the nearest minute`` () =
        let actual = date.AddSeconds(14.) |> DateTimeOffset.roundMinute
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can round up to the nearest minute at thirty seconds`` () =
        let actual = date.AddSeconds(30.) |> DateTimeOffset.roundMinute
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can round up to the nearest minute`` () =
        let actual = date.AddSeconds(33.) |> DateTimeOffset.roundMinute
        let expected = date.AddMinutes(1.)
        actual |> should equal expected
    
    [<Test>]
    member x.``I can round down to the nearest half hour`` () =
        let actual = date.AddMinutes(14.) |> DateTimeOffset.roundHalfhour
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can round up to the nearest half hour at fifteen minutes`` () =
        let actual = date.AddMinutes(15.) |> DateTimeOffset.roundHalfhour
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can round up to the nearest half hour`` () =
        let actual = date.AddMinutes(23.) |> DateTimeOffset.roundHalfhour
        let expected = date.AddMinutes(30.)
        actual |> should equal expected
    
    [<Test>]
    member x.``I can round down to the nearest day`` () =
        let actual = date.AddHours(11.) |> DateTimeOffset.roundDay
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can round up to the nearest day at midday`` () =
        let actual = date.AddHours(12.) |> DateTimeOffset.roundDay
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can round up to the nearest day`` () =
        let actual = date.AddHours(13.) |> DateTimeOffset.roundDay
        let expected = date.AddDays(1.)
        actual |> should equal expected


    [<Test>]
    member x.``I can get the halfhour ceiling with 0 seconds past the hour`` () =
        let actual = date |> DateTimeOffset.ceilHalfhour
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour ceiling with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTimeOffset.ceilHalfhour
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour ceiling with 0 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0) |> DateTimeOffset.ceilHalfhour
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour ceiling with 1 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTimeOffset.ceilHalfhour
        let expected = date.AddMinutes(60.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour floor with 0 seconds past the hour`` () =
        let actual = date |> DateTimeOffset.floorHalfhour
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour floor with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTimeOffset.floorHalfhour
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour floor with 0 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0) |> DateTimeOffset.floorHalfhour
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour floor with 1 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTimeOffset.floorHalfhour
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute ceiling with 0 seconds past the hour`` () =
        let actual = date |> DateTimeOffset.ceilMinute
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute ceiling with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTimeOffset.ceilMinute
        let expected = date.AddMinutes(1.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute ceiling with 0 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0) |> DateTimeOffset.ceilMinute
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute ceiling with 1 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTimeOffset.ceilMinute
        let expected = date.AddMinutes(31.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute floor with 0 seconds past the hour`` () =
        let actual = date |> DateTimeOffset.floorMinute
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute floor with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTimeOffset.floorMinute
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute floor with 0 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0) |> DateTimeOffset.floorMinute
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute floor with 1 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTimeOffset.floorMinute
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the day ceiling with 0 seconds past the hour`` () =
        let actual = date |> DateTimeOffset.ceilDay
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the day ceiling with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTimeOffset.ceilDay
        let expected = date.AddMinutes(1.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the day ceiling with 0 seconds past midday`` () =
        let actual = date.AddMinutes(30.0) |> DateTimeOffset.ceilDay
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the day ceiling with 1 seconds past midday`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTimeOffset.ceilDay
        let expected = date.AddMinutes(31.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the day floor with 0 seconds past the hour`` () =
        let actual = date |> DateTimeOffset.floorDay
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the day floor with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTimeOffset.floorDay
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the day floor with 0 seconds past midday`` () =
        let actual = date.AddMinutes(30.0) |> DateTimeOffset.floorDay
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the day floor with 1 seconds past midday`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTimeOffset.floorDay
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can compute the start of the month``() = 
        let expected = DateTimeOffset(2013, 1, 1, 0,0,0, TimeSpan.FromHours(1.))
        let actual = DateTimeOffset(2013, 1, 12, 13, 45, 56, TimeSpan.FromHours(1.))
        actual.ToMonthStartDate() |> should equal expected

    [<Test>]
    member x.``I can compute the end of the month``() = 
        let expected = DateTimeOffset(2013, 1, 31, 23, 59, 59, 999, TimeSpan.FromHours(1.))
        let actual = DateTimeOffset(2013, 1, 12, 13, 45, 56, TimeSpan.FromHours(1.))
        actual.ToMonthEndDate() |> should equal expected

    [<Test>]
    member x.``I can compute the last day of the week at the end of the month``() = 
        let expected = DateTimeOffset(2013, 1, 27, 23, 59, 59, 999, TimeSpan.FromHours(1.))
        let actual = DateTimeOffset(2013, 1, 12, 13, 45, 56, TimeSpan.FromHours(1.))
        actual.LastDayOfWeekInMonth(DayOfWeek.Sunday) |> should equal expected

    [<Test>]
    member x.``I can compute the short day``() = 
        let date = DateTimeOffset(2013, 3, 31, 0,0,0, TimeSpan.FromHours(0.))
        date.IsShortDay |> should equal true

    [<Test>]
    member x.``I can compute the long day``() = 
        let date = DateTimeOffset(2013, 10, 27, 0,0,0, TimeSpan.FromHours(0.))
        date.IsLongDay |> should equal true

    [<Test>]
    member x.``I can compute the number of half hours in the day``() = 
        let date = DateTimeOffset(2013, 1, 1, 0,0,0, TimeSpan.FromHours(-1.))
        date.HalfHoursInDay |> should equal 48

    [<Test>]
    member x.``I can compute the number of half hours in the short day``() = 
        let date = DateTimeOffset(2013, 3, 31, 0,0,0, TimeSpan.FromHours(0.))
        date.HalfHoursInDay |> should equal 46

    [<Test>]
    member x.``I can compute the number of half hours in the long day``() = 
        let date = DateTimeOffset(2013, 10, 27, 0,0,0, TimeSpan.FromHours(0.))
        date.HalfHoursInDay |> should equal 50

    [<Test>]
    member x.``I can compute the number of hours in the day``() = 
        let date = DateTimeOffset(2013, 1, 1, 0,0,0, TimeSpan.FromHours(-1.))
        date.HoursInDay |> should equal 24

    [<Test>]
    member x.``I can compute the number of hours in the short day``() = 
        let date = DateTimeOffset(2013, 3, 31, 0,0,0, TimeSpan.FromHours(0.))
        date.HoursInDay |> should equal 23

    [<Test>]
    member x.``I can compute the number of hours in the long day``() = 
        let date = DateTimeOffset(2013, 10, 27, 0,0,0, TimeSpan.FromHours(0.))
        date.HoursInDay |> should equal 25

    [<Test>]
    member x.``I can compute the clock change type as NoClockChange``() = 
        let date = DateTimeOffset(2013, 1, 1, 0,0,0, TimeSpan.FromHours(-1.))
        date.ClockChange |> should equal ClockChangeType.NoClockChange

    [<Test>]
    member x.``I can compute the clock change type as ShortDay``() = 
        let date = DateTimeOffset(2013, 3, 31, 0,0,0, TimeSpan.FromHours(0.))
        date.ClockChange |> should equal ClockChangeType.Short

    [<Test>]
    member x.``I can compute the number of hours in the LongDay``() = 
        let date = DateTimeOffset(2013, 10, 27, 0,0,0, TimeSpan.FromHours(0.))
        date.ClockChange |> should equal ClockChangeType.Long

    [<Test>]
    member x.``I can compute the next half hour end``() = 
        let expected = DateTimeOffset(2013, 1, 1, 2, 0, 0, TimeSpan.FromHours(1.))
        let actual = DateTimeOffset(2013, 1, 1, 1, 30, 0, TimeSpan.FromHours(1.))
        actual.ToHalfHourEnd() |> should equal expected

    [<Test>]
    member x.``I can compute the next half hour start``() = 
        let expected = DateTimeOffset(2013, 1, 1, 1, 0, 0, TimeSpan.FromHours(1.))
        let actual = DateTimeOffset(2013, 1, 1, 1, 4, 0, TimeSpan.FromHours(1.))
        actual.ToHalfHourStart() |> should equal expected

    [<Test>]
    member x.``I can compute the day start``() = 
        let expected = DateTimeOffset(2013, 1, 1, 0, 0, 0, TimeSpan.FromHours(1.))
        let actual = DateTimeOffset(2013, 1, 1, 4, 0, 0, TimeSpan.FromHours(1.))
        actual.ToDayStart() |> should equal expected

    [<Test>]
    member x.``I can compute the day end``() = 
        let expected = DateTimeOffset(2013, 1, 1, 23, 59, 59, TimeSpan.FromHours(1.))
        let actual = DateTimeOffset(2013, 1, 1, 0, 0, 0, TimeSpan.FromHours(1.))
        actual.ToDayEnd() |> should equal expected

    [<Test>]
    member x.``I can compute the year start``() = 
        let expected = DateTimeOffset(2013, 1, 1, 0, 0, 0, TimeSpan.Zero)
        let actual = DateTimeOffset.YearStart(2013)
        actual |> should equal expected

    [<Test>]
    member x.``I can compute the year end``() = 
        let expected = DateTimeOffset(2013, 12, 31, 0, 0, 0, TimeSpan.Zero)
        let actual = DateTimeOffset.YearEnd(2013)
        actual |> should equal expected

    [<Test>]
    member x.``I can compute the number of days in year``() = 
        let actual = DateTimeOffset.TotalDaysInYear(2013)
        actual |> should equal 364.

    [<Test>]
    member test.``Should Compute Long index with short index at None within the current year``() = 
        let (shortDayIndex, longDayIndex) = DateTimeOffset.computeShortLongHourIndicies (DateTimeOffset(DateTime.LongDay(2012).Ticks, TimeSpan.FromHours(0.))) (TimeSpan.FromHours(1.)) 
        shortDayIndex |> should equal None
        longDayIndex |> should equal (Some(2))
    
    [<Test>]
    member test.``Should Compute Short index and long index within the current year``() = 
        let (shortDayIndex, longDayIndex) = DateTimeOffset.computeShortLongHourIndicies (DateTimeOffset(DateTime.ShortDay(2012).Ticks, TimeSpan.FromHours(0.))) (TimeSpan.FromHours(1.)) 
        shortDayIndex |> should equal (Some(2))
        longDayIndex |> should equal (Some((DateTime.LongDay(2012).Subtract(DateTime.ShortDay(2012)).TotalHours |> int) + 2))

    [<Test>]
    member tet.``Does not effect a series that does cross a boundary``() = 
        let (_,data) = singleMonth 1 (TimeSpan.FromHours(1.))
        let original = data |> Seq.toList
        let series =  DateTimeOffset.normaliseForClockChange (DateTimeOffset(2012, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))) (TimeSpan.FromHours(1.)) data

        (Seq.length series) |> should equal original.Length
    
    [<Test>]
    member test.``Can handle both short and long day clock changes when five minute granularity``() =
          let (s, e, timeSpan) = (DateTimeOffset(2012, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (DateTimeOffset(2013, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromMinutes(5.))
          let hours = (e.Subtract(s).TotalMinutes / 5.) |> int
          let (sindex, lindex) = DateTimeOffset.computeShortLongHourIndicies s timeSpan
          let series = new ResizeArray<_>(Seq.init hours (fun i -> Nullable<_>(i |> float32)))

          //Add long hour and remove short
          series.RemoveRange(sindex.Value, 12)
          series.InsertRange(lindex.Value, Seq.init 12 (fun i -> Nullable<_>(1000000 |> float32)))

          105408 |> should equal series.Count
          let v = DateTimeOffset.normaliseForClockChange s timeSpan series |> Seq.toArray  
          Array.FindIndex(v, fun x -> x = Nullable<float32>()) |> should equal sindex.Value
          ((v |> Seq.toArray).[sindex.Value..sindex.Value + 11]) |> should equal (Array.init 12 (fun i -> Nullable<float32>()))
          Array.FindIndex(v, fun x -> x = Nullable<float32>(1000000.f)) |> should equal -1

    [<Test>]
    member test.``Can handle both short and long day clock changes when hourly ``() =
          let (s, e, timeSpan) = (DateTimeOffset(2012, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (DateTimeOffset(2013, 01, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromHours(1.))
          let hours = e.Subtract(s).TotalHours |> int
          let (sindex, lindex) = DateTimeOffset.computeShortLongHourIndicies s timeSpan
          let series = new ResizeArray<_>(Seq.init hours (fun i -> Nullable<_>(i |> float32)))

          //Add long hour and remove short
          series.RemoveAt(sindex.Value)
          series.Insert(lindex.Value, Nullable<_>(10000 |> float32))

          8784 |> should equal series.Count
          let v = new ResizeArray<_>(DateTimeOffset.normaliseForClockChange s timeSpan (series |> List.ofSeq))
          v.FindIndex(fun x -> x = Nullable<float32>()) |> should equal sindex.Value
          v.FindIndex(fun x -> x = Nullable<float32>(10000.f)) |> should equal -1

    [<Test>]
    member test.``Long clock change day has 24 hours after being corrected``() = 
        let startDate, timeSpan = (DateTimeOffset(2012, 10, 1, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromHours(1.))
        let (_,march) = singleMonth startDate.Month timeSpan
        let (_,index) = DateTimeOffset.computeShortLongHourIndicies startDate timeSpan
        //A full 31 day month should have 744 hours
        744 |> should equal march.Count
        
        march.Insert(index.Value, Nullable<_>(10000 |> float32))
        745 |> should equal march.Count

        let v = DateTimeOffset.normaliseForClockChange startDate timeSpan  march |> Seq.toArray
        744 |> should equal v.Length
        Array.sub v index.Value 1 |> should equal [|Nullable<float32>(index.Value |> float32)|]
 
    [<Test>]
    member test.``Long clock change day has 288 five minute intervals after being corrected``() = 
        let startDate, timeSpan = (DateTimeOffset(2012, 10, 1, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromMinutes(5.))
        let (_,march) = singleMonth startDate.Month timeSpan
        let (_,index) = DateTimeOffset.computeShortLongHourIndicies startDate timeSpan
        //A full 31 day month should have 8928 five minutes
        8928 |> should equal march.Count
        //Remove clock change hour this is 25 days + 2 hours in (602 hours)
        march.InsertRange(index.Value, (Array.init 12 (fun i -> Nullable<float32>(100000 + i |> float32))))
        8940 |> should equal march.Count

        let v = DateTimeOffset.normaliseForClockChange startDate timeSpan march |> Seq.toArray
        8928 |> should equal (v.Length)
        Array.sub v index.Value 24 |> should equal (Array.init 24 (fun i -> Nullable<float32>(index.Value + i |> float32)))     


    [<Test>]
    member test.``Short clock change day has 24 hours after being corrected``() = 
        let startDate, timeSpan = (DateTimeOffset(2012, 03, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromHours(1.))
        let (_,march) = singleMonth startDate.Month timeSpan
        let (index,_) = DateTimeOffset.computeShortLongHourIndicies startDate timeSpan
        //A full 31 day month should have 744 hours
        744 |> should equal march.Count
        //Remove clock change hour this is 25 days + 2 hours in (602 hours)
        march.RemoveAt(index.Value)
        743 |> should equal march.Count

        let v = DateTimeOffset.normaliseForClockChange startDate timeSpan  march |> Seq.toArray
        744 |> should equal v.Length
        Array.sub v index.Value 1 |> should equal [|Nullable<float32>()|]
 
    [<Test>]
    member test.``Short clock change day has 288 five minute intervals after being corrected``() = 
        let startDate, timeSpan = (DateTimeOffset(2012, 03, 01, 0, 0, 0, TimeSpan.FromHours(0.))), (TimeSpan.FromMinutes(5.))
        let (_,march) = singleMonth startDate.Month timeSpan
        let (index,_) = DateTimeOffset.computeShortLongHourIndicies startDate timeSpan
        //A full 31 day month should have 8928 five minutes
        8928 |> should equal march.Count
        //Remove clock change hour this is 25 days + 2 hours in (602 hours)
        march.RemoveRange(index.Value, 12)
        8916 |> should equal march.Count

        let v = DateTimeOffset.normaliseForClockChange startDate timeSpan  march |> Seq.toArray
        8928 |> should equal v.Length
        Array.sub v index.Value 12 |> should equal (Array.init 12 (fun _ -> Nullable<float32>()))    

