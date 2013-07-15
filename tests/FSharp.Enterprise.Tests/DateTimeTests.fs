namespace FSharp.Enterprise.Tests

open System
open NUnit.Framework
open FsUnit
open FSharp.Enterprise
open FSharp.Enterprise.DateTime


[<TestFixture>]
type ``Given a DateTime``() = 
  
    let date = DateTime(2013,5,8,23,0,0)

    let singleMonth month (granularity : TimeSpan) = 
      let s = (DateTime(2012, month, 01, 0, 0, 0))
      let e = s.AddMonths(1)
      let c = e.Subtract(s).TotalMinutes / granularity.TotalMinutes |> int
      s, new ResizeArray<_>(Seq.init c (fun i -> Nullable<_>(i |> float32)))
     
    [<Test>]
    member t.``I should be able to calculate the unix ticks``() = 
        let actual = DateTime(2012,1,1)
        actual.ToUnixTicks() |> should equal 1325376000000L

    [<Test>]
    member x.``I can get the halfhour ceiling with 0 seconds past the hour`` () =
        let actual = date |> DateTime.ceilHalfhour
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour ceiling with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTime.ceilHalfhour
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour ceiling with 0 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0) |> DateTime.ceilHalfhour
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour ceiling with 1 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTime.ceilHalfhour
        let expected = date.AddMinutes(60.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour floor with 0 seconds past the hour`` () =
        let actual = date |> DateTime.floorHalfhour
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour floor with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTime.floorHalfhour
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour floor with 0 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0) |> DateTime.floorHalfhour
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the halfhour floor with 1 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTime.floorHalfhour
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute ceiling with 0 seconds past the hour`` () =
        let actual = date |> DateTime.ceilMinute
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute ceiling with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTime.ceilMinute
        let expected = date.AddMinutes(1.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute ceiling with 0 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0) |> DateTime.ceilMinute
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute ceiling with 1 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTime.ceilMinute
        let expected = date.AddMinutes(31.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute floor with 0 seconds past the hour`` () =
        let actual = date |> DateTime.floorMinute
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute floor with 1 second past the hour`` () =
        let actual = date.AddSeconds(1.0) |> DateTime.floorMinute
        let expected = date
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute floor with 0 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0) |> DateTime.floorMinute
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can get the minute floor with 1 seconds past the halfhour`` () =
        let actual = date.AddMinutes(30.0).AddSeconds(1.0) |> DateTime.floorMinute
        let expected = date.AddMinutes(30.0)
        actual |> should equal expected

    [<Test>]
    member x.``I can compute the start of the month``() = 
        let expected = DateTime(2013, 1, 1, 0,0,0)
        let actual = DateTime(2013, 1, 12, 13, 45, 56)
        actual.ToMonthStartDate() |> should equal expected

    [<Test>]
    member x.``I can compute the end of the month``() = 
        let expected = DateTime(2013, 1, 31, 23, 59, 59)
        let actual = DateTime(2013, 1, 12, 13, 45, 56)
        actual.ToMonthEndDate() |> should equal expected

    [<Test>]
    member x.``I can compute the last day of the week at the end of the month``() = 
        let expected = DateTime(2013, 1, 27, 23, 59, 59)
        let actual = DateTime(2013, 1, 12, 13, 45, 56)
        actual.LastDayOfWeekInMonth(DayOfWeek.Sunday) |> should equal expected

    [<Test>]
    member x.``I can compute the short day``() = 
        let date = DateTime(2013, 3, 31, 0,0,0)
        date.IsShortDay |> should equal true

    [<Test>]
    member x.``I can compute the long day``() = 
        let date = DateTime(2013, 10, 27, 0,0,0)
        date.IsLongDay |> should equal true

    [<Test>]
    member x.``I can compute the number of half hours in the day``() = 
        let date = DateTime(2013, 1, 1, 0,0,0)
        date.HalfHoursInDay |> should equal 48

    [<Test>]
    member x.``I can compute the number of half hours in the short day``() = 
        let date = DateTime(2013, 3, 31, 0,0,0)
        date.HalfHoursInDay |> should equal 46

    [<Test>]
    member x.``I can compute the number of half hours in the long day``() = 
        let date = DateTime(2013, 10, 27, 0,0,0)
        date.HalfHoursInDay |> should equal 50

    [<Test>]
    member x.``I can compute the number of hours in the day``() = 
        let date = DateTime(2013, 1, 1, 0,0,0)
        date.HoursInDay |> should equal 24

    [<Test>]
    member x.``I can compute the number of hours in the short day``() = 
        let date = DateTime(2013, 3, 31, 0,0,0)
        date.HoursInDay |> should equal 23

    [<Test>]
    member x.``I can compute the number of hours in the long day``() = 
        let date = DateTime(2013, 10, 27, 0,0,0)
        date.HoursInDay |> should equal 25

    [<Test>]
    member x.``I can compute the clock change type as NoClockChange``() = 
        let date = DateTime(2013, 1, 1, 0,0,0)
        date.ClockChange |> should equal ClockChangeType.NoClockChange

    [<Test>]
    member x.``I can compute the clock change type as ShortDay``() = 
        let date = DateTime(2013, 3, 31, 0, 0, 0)
        date.ClockChange |> should equal ClockChangeType.Short

    [<Test>]
    member x.``I can compute the number of hours in the LongDay``() = 
        let date = DateTime(2013, 10, 27, 0,0,0)
        date.ClockChange |> should equal ClockChangeType.Long

    [<Test>]
    member x.``I can compute the next half hour end``() = 
        let expected = DateTime(2013, 1, 1, 2, 0, 0)
        let actual = DateTime(2013, 1, 1, 1, 30, 0)
        actual.ToHalfHourEnd() |> should equal expected

    [<Test>]
    member x.``I can compute the next half hour start``() = 
        let expected = DateTime(2013, 1, 1, 1, 0, 0)
        let actual = DateTime(2013, 1, 1, 1, 4, 0)
        actual.ToHalfHourStart() |> should equal expected

    [<Test>]
    member x.``I can compute the day start``() = 
        let expected = DateTime(2013, 1, 1, 0, 0, 0)
        let actual = DateTime(2013, 1, 1, 4, 0, 0)
        actual.ToDayStart() |> should equal expected

    [<Test>]
    member x.``I can compute the day end``() = 
        let expected = DateTime(2013, 1, 1, 23, 59, 59, 999)
        let actual = DateTime(2013, 1, 1, 0, 0, 0)
        actual.ToDayEnd() |> should equal expected

    [<Test>]
    member x.``I can compute the year start``() = 
        let expected = DateTime(2013, 1, 1, 0, 0, 0)
        let actual = DateTime.YearStart(2013)
        actual |> should equal expected

    [<Test>]
    member x.``I can compute the year end``() = 
        let expected = DateTime(2013, 12, 31, 0, 0, 0)
        let actual = DateTime.YearEnd(2013)
        actual |> should equal expected

    [<Test>]
    member x.``I can compute the number of days in year``() = 
        let actual = DateTime.TotalDaysInYear(2013)
        actual |> should equal 364.

   

