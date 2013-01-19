namespace FSharp.Enterprise

open System
open System.Runtime.CompilerServices

[<AutoOpen>]
[<Extension>]
module DateTimeExtensions =

    type ClockChangeType =
        | NoClockChange
        | Short
        | Long

    type System.DateTime with
        
        [<Extension>]
        member x.ToUnixTicks() = 
            let epoch = new System.DateTime(1970, 1, 1, 0, 0, 0)
            let current = x.ToUniversalTime()
            let result = current.Subtract(epoch)
            result.TotalMilliseconds |> int64;

        [<Extension>]
        member x.ToMonthStartDate() =
            new DateTime(x.Year, x.Month, 1, 0, 0, 0, x.Kind)
        
        [<Extension>]
        member x.ToMonthEndDate() =
            new DateTime(x.Year, x.Month, DateTime.DaysInMonth(x.Year, x.Month), 0, 0, 0, x.Kind) 
        
        [<Extension>]
        member x.LastDayOfWeekInMonth(day) =
            let monthEnd = x.ToMonthEndDate()
            let wantedDay = int day
            let lastDay = int monthEnd.DayOfWeek
            let offset = 
                if lastDay > wantedDay 
                then wantedDay - lastDay 
                else wantedDay - lastDay - 7
            monthEnd.AddDays(float offset)

        /// Returns the number of half-hour periods in the day taking into account
        /// long and short days caused by autumn and spring clock changes.
        [<Extension>]
        member x.HalfHoursInDay =
            if x.IsShortDay then 46
            elif x.IsLongDay then 50
            else 48

        [<Extension>]
        member x.HoursInDay = 
            x.HalfHoursInDay / 2
        
        [<Extension>]
        member x.IsShortDay
            with get() = x.Date = DateTime.ShortDay(x.Year).Date
        
        [<Extension>]
        member x.IsLongDay
            with get() = x.Date = DateTime.LongDay(x.Year).Date
        
        [<Extension>]
        member x.ClockChange
            with get() =
                if x.IsShortDay then Short
                elif x.IsLongDay then Long
                else NoClockChange
         
        [<Extension>]                           
        member x.ToHalfHourStart() =
            if x.Minute >= 30 then
                DateTime(x.Year, x.Month, x.Day, x.Hour, 30, 0, x.Kind)                
            else
                DateTime(x.Year, x.Month, x.Day, x.Hour, 0, 0, x.Kind)

        [<Extension>]
        member x.ToHalfHourEnd() =
            if x.Minute >= 30 then
                DateTime(x.Year, x.Month, x.Day, x.Hour, 0, 0, x.Kind).AddHours(1.)                
            else
                DateTime(x.Year, x.Month, x.Day, x.Hour, 30, 0, x.Kind)
        
        [<Extension>]
        member x.ToDayStart() =
            DateTime(x.Year, x.Month, x.Day, 0, 0, 0, x.Kind)
        
        [<Extension>]
        member x.ToDayEnd() =
            x.ToDayStart().AddDays(1.)

        /// Returns the time of the next gate closure given the gate closure
        /// duration (in minutes). 
        [<Extension>]           
        member x.ToNextGateClosure(gateClosureDuration) =
            x.ToHalfHourEnd().AddMinutes(float gateClosureDuration)

        [<Extension>]             
        static member ShortDay(year) : DateTime =
            let march = DateTime(year, 3, 1)
            march.LastDayOfWeekInMonth(DayOfWeek.Sunday)

        [<Extension>]
        static member LongDay(year) : DateTime =
            let october = DateTime(year, 10, 1)
            october.LastDayOfWeekInMonth(DayOfWeek.Sunday)
        
        [<Extension>]
        static member YearStart(year) =
            DateTime(year, 1, 1)

        [<Extension>]
        static member YearEnd(year) =
            DateTime(year, 12, 31)

        [<Extension>]
        static member TotalDaysInYear(year) =
            (DateTime.YearEnd(year) - DateTime.YearStart(year)).TotalDays

        [<Extension>]
        static member DatesInYear(year) =
            let daysInYear = DateTime.TotalDaysInYear(year)
            let start = DateTime.YearStart(year)
            seq { for i in 0. .. daysInYear - 1. do 
                    yield start.AddDays(i) }

module DateTime = 
     
    open System.Linq

    let computeShortLongHourIndicies (date : DateTimeOffset) (granularity : TimeSpan) = 
        let toOption (i : int) = if i >= 0 then Some(i) else None
        if granularity.TotalMinutes >= 1440.  //if it at day granularity or bigger we don't have to worry 
        then (None, None)
        else
            (DateTime.ShortDay(date.Year).AddHours(2.).Subtract(date.DateTime).TotalMinutes / granularity.TotalMinutes |> int |> toOption, 
             DateTime.LongDay(date.Year).AddHours(2.).Subtract(date.DateTime).TotalMinutes / granularity.TotalMinutes |> int |> toOption)

    let normaliseForClockChange (startDate : DateTimeOffset) (granularity : TimeSpan) (seq : seq<'a>) = 
        let sindx, lindx = computeShortLongHourIndicies startDate granularity
        let series = new ResizeArray<_>(seq)
        Option.iter (fun longDayIndex ->
                         if  (longDayIndex < series.Count)
                         then series.RemoveRange(longDayIndex, (TimeSpan.FromHours(1.).TotalMinutes / granularity.TotalMinutes) |> int)
                    ) lindx
        Option.iter (fun shortDayIndex -> 
                        if  (shortDayIndex < series.Count)
                        then series.InsertRange(shortDayIndex, Seq.init (TimeSpan.FromHours(1.).TotalMinutes / granularity.TotalMinutes |> int) (fun _ -> new 'a())) 
                    ) sindx
        series.AsEnumerable() :> seq<_>

