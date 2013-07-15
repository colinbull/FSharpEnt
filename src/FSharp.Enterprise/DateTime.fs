namespace FSharp.Enterprise

open System
open System.Runtime.CompilerServices
open System.Linq

module DateTime = 
    
    type TimeBoundary =
        | Minute
        | Halfhour
    
    type ClockChangeType =
          | NoClockChange
          | Short
          | Long


    let zeroMinutes = TimeSpan.FromMinutes(0.0)
    let thirtyMinutes = TimeSpan.FromMinutes(30.0)
    
    [<AutoOpen>]
    [<Extension>]
    module DateTimeExtensions =
    
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
    
        type System.DateTimeOffset with
                
            [<Extension>]
            member x.ToMonthStartDate() =
                new DateTimeOffset(x.Year, x.Month, 1, 0, 0, 0, x.Offset)
            
            [<Extension>]
            member x.ToMonthEndDate() =
                new DateTimeOffset(x.Year, x.Month, DateTime.DaysInMonth(x.Year, x.Month), 23, 59, 59, x.Offset) 
            
            [<Extension>]
            member x.LastDayOfWeekInMonth(day) =
                 let monthEnd = x.ToMonthEndDate()
                 let wantedDay = int day
                 let lastDay = int monthEnd.DayOfWeek
                 let offset = 
                     let diff = wantedDay - lastDay
                     if diff > 0 then diff - 7 else diff            
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
            static member ShortDay(year) =
                let march = DateTimeOffset(DateTime(year, 3, 1))
                march.LastDayOfWeekInMonth(DayOfWeek.Sunday)
        
            [<Extension>]
            static member LongDay(year) =
                let october = DateTimeOffset(DateTime(year, 10, 1))
                october.LastDayOfWeekInMonth(DayOfWeek.Sunday)
        
            [<Extension>]
            member x.IsShortDay
                with get() = x.Date = (DateTimeOffset.ShortDay(x.Year)).Date
            
            [<Extension>]
            member x.IsLongDay
                with get() = x.Date = (DateTimeOffset.LongDay(x.Year)).Date
            
            [<Extension>]
            member x.ClockChange
                with get() =
                    if x.IsShortDay then Short
                    elif x.IsLongDay then Long
                    else NoClockChange
             
            [<Extension>]                           
            member x.ToHalfHourStart() =
                if x.Minute >= 30 then
                    DateTimeOffset(x.Year, x.Month, x.Day, x.Hour, 30, 0, x.Offset)                
                else
                    DateTimeOffset(x.Year, x.Month, x.Day, x.Hour, 0, 0, x.Offset)
        
            [<Extension>]
            member x.ToHalfHourEnd() =
                if x.Minute >= 30 then
                    DateTimeOffset(x.Year, x.Month, x.Day, x.Hour, 0, 0, x.Offset).AddHours(1.)                
                else
                    DateTimeOffset(x.Year, x.Month, x.Day, x.Hour, 30, 0, x.Offset)
            
            [<Extension>]
            member x.ToDayStart() =
                DateTimeOffset(x.Year, x.Month, x.Day, 0, 0, 0, x.Offset)
            
            [<Extension>]
            member x.ToDayEnd() =
                DateTimeOffset(x.Year, x.Month, x.Day, 23, 59, 59, x.Offset)
        
            /// Returns the time of the next gate closure given the gate closure
            /// duration (in minutes). 
            [<Extension>]           
            member x.ToNextGateClosure(gateClosureDuration) =
                x.ToHalfHourEnd().AddMinutes(float gateClosureDuration)
        
            [<Extension>]
            static member YearStart(year) =
                DateTimeOffset(DateTime(year, 1, 1))
        
            [<Extension>]
            static member YearEnd(year) =
                DateTimeOffset(DateTime(year, 12, 31))
        
            [<Extension>]
            static member TotalDaysInYear(year) =
                (DateTimeOffset.YearEnd(year) - DateTimeOffset.YearStart(year)).TotalDays
        
            [<Extension>]
            static member DatesInYear(year) =
                let daysInYear = DateTimeOffset.TotalDaysInYear(year)
                let start = DateTimeOffset.YearStart(year)
                seq { for i in 0. .. daysInYear - 1. do 
                        yield start.AddDays(i) }
            
            [<Extension>]
            member x.Ceil(timeBoundary:TimeBoundary) =
                match timeBoundary with
                | Minute ->
                    let d = DateTimeOffset(x.Year, x.Month, x.Day, x.Hour, x.Minute, 0, x.Offset)
                    let delta = x - d
                    if delta = zeroMinutes then
                        d
                    else
                        d.AddMinutes(1.0)            
                | Halfhour ->
                    let d = DateTimeOffset(x.Year, x.Month, x.Day, x.Hour, 0, 0, x.Offset)
                    let delta = x - d
                    if delta = zeroMinutes then
                        d
                    elif delta <= thirtyMinutes then
                        d.AddMinutes(30.0)
                    else
                        d.AddMinutes(60.0)
        
            [<Extension>]
            member x.Floor(timeBoundary:TimeBoundary) =
                match timeBoundary with
                | Minute ->
                    DateTimeOffset(x.Year, x.Month, x.Day, x.Hour, x.Minute, 0, x.Offset)                        
                | Halfhour ->
                    let d = DateTimeOffset(x.Year, x.Month, x.Day, x.Hour, 0, 0, x.Offset)
                    let delta = x - d
                    if delta < thirtyMinutes then
                        d
                    else
                        d.AddMinutes(30.0)

module DateTimeOffset = 
    
    open DateTime

    let ceilMinute (d:DateTimeOffset) = d.Ceil(TimeBoundary.Minute)
    let floorMinute (d:DateTimeOffset) = d.Floor(TimeBoundary.Minute)
    
    let ceilHalfhour (d:DateTimeOffset) = d.Ceil(TimeBoundary.Halfhour)
    let floorHalfhour (d:DateTimeOffset) = d.Floor(TimeBoundary.Halfhour)


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
        series :> seq<_>

