namespace FSharp.Enterprise

#if INTERACTIVE
open FSharp.Enterprise
#endif

module Segment =

    open FSharpx

    type T<'x,'y> =
        | Instantaneous of Point.T<'x,'y>
        | Discrete of Interval.T<'x> * 'y
        | Continuous of Point.T<'x,'y> * Point.T<'x,'y>

    let makeInstantaneous p = Instantaneous p
    let makeDiscrete (i,v) = Discrete (i,v)
    let makeContinuous (p1,p2) = Continuous (p1,p2)

    let emptyInstantaneous x = Instantaneous (Point.empty x)
    let emptyDiscrete i = Discrete (i,Unchecked.defaultof<'y>)
    let emptyContinuous i = Continuous (Point.empty (Interval.left i),Point.empty (Interval.right i))

    let startPoint = function
        | Instantaneous p -> p
        | Discrete (i,v) -> Point.make(Interval.left i,v) 
        | Continuous (p,_) -> p

    let endPoint = function
        | Instantaneous p -> p
        | Discrete (i,v) -> Point.make(Interval.right i,v) 
        | Continuous (_,p) -> p    

    let unmake s =
        let p1,p2 = startPoint s, endPoint s
        p1.X,p1.Y,p2.X,p2.Y

    let startX = function
        | Instantaneous p -> p.X
        | Discrete (i,_) -> Interval.left i 
        | Continuous (p,_) -> p.X

    let startY = function
        | Instantaneous p -> p.Y
        | Discrete (_,v) -> v 
        | Continuous (p,_) -> p.Y

    let endX = function
        | Instantaneous p -> p.X
        | Discrete (i,_) -> Interval.right i 
        | Continuous (_,p) -> p.X

    let endY = function
        | Instantaneous p -> p.Y
        | Discrete (_,v) -> v 
        | Continuous (_,p) -> p.Y

    let range = function
        | Instantaneous p -> Interval.make(p.X,p.X)
        | Discrete (i,_) -> i 
        | Continuous (p1,p2) -> Interval.make(p1.X,p2.X)

    let isInRange intervalType x s =
        range s |> Interval.isIn intervalType x 

    let domain = function
        | Instantaneous p -> Interval.make(p.Y,p.Y)
        | Discrete (_,v) -> Interval.make(v,v) 
        | Continuous (p1,p2) -> Interval.make(p1.Y,p2.Y)

    let isInDomain intervalType y s =
        domain s |> Interval.isIn intervalType y 

    let isFlatDomain s =
        startY s = endY s

    let map f = function 
        | Instantaneous p -> makeInstantaneous (fst (f (p,p)))
        | Discrete _ as s -> f (startPoint s, endPoint s) |> (fun (p1,p2:Point.T<'a,'b>) -> makeDiscrete (Interval.make(p1.X,p2.X),p1.Y)) 
        | Continuous (p1,p2) -> makeContinuous (f (p1,p2))
       
    let mapX f = function
        | Instantaneous p -> makeInstantaneous (Point.mapX f p)
        | Discrete (_, v) as s -> makeDiscrete (Interval.make(f (startX s), f (endX s)), v) 
        | Continuous (p1,p2) -> makeContinuous (Point.mapX f p1, Point.mapX f p2)
       
    let mapY f = function
        | Instantaneous p -> makeInstantaneous (Point.mapY f p)
        | Discrete (i, v) -> makeDiscrete (i, f v) 
        | Continuous (p1,p2) -> makeContinuous (Point.mapY f p1, Point.mapY f p2)

    let mapY2 f s1 s2 = 
        match s1, s2 with
        | Instantaneous p1, Instantaneous p2 when p1.X = p2.X -> 
                makeInstantaneous (Point.make(p1.X, f p1.Y p2.Y))
        | Discrete (i1, v1), Discrete(i2, v2) when i1 = i2 ->
                makeDiscrete (i1, f v1 v2)
        | Continuous (p1, p1'), Continuous (p2, p2') when p1.X = p2.X && p1'.X = p2'.X -> 
                makeContinuous (Point.make(p1.X, f p1.Y p2.Y), Point.make(p1'.X, f p1'.Y p2'.Y))
        | _ -> invalidArg "s2" "Time segments must be consistent with respect to time and type"

    let fold f state s =
        f state (startPoint s,endPoint s)

    let (|OverlapStart|OverlapEnd|Overlap|Internal|External|) (interval,segment) =
         let iStart, iEnd = Interval.left interval, Interval.right interval
         let segStart, segEnd = startX segment, endX segment
         if segEnd <= iStart || segStart >= iEnd
         then External (iStart, iEnd, segment)
         elif segStart >= iStart && segEnd <= iEnd
         then Internal (iStart, iEnd, segment)
         elif segStart < iStart && segEnd <= iEnd
         then OverlapStart (iStart, iEnd,segment)
         elif segStart >= iStart && segEnd > iEnd
         then OverlapEnd (iStart, iEnd, segment)
         else Overlap (iStart, iEnd, segment)
           

    let inline tryInterpolateY x s =
        if isInRange IntervalType.T.Closed x s then
            let x0,y0 = startPoint s |> fun p -> p.X, p.Y 
            let x1,y1 = endPoint s |> fun p -> p.X, p.Y 
            let y = Math.Interpolation.linear x x0 y0 x1 y1
            Some y
        else
            None

    let inline interpolateY x s=
        let x0,y0 = startPoint s |> fun p -> p.X, p.Y 
        let x1,y1 = endPoint s |> fun p -> p.X, p.Y 
        Math.Interpolation.linear x x0 y0 x1 y1

    let inline tryInterpolateX y s = 
        if isInDomain IntervalType.T.Closed y s then
            if isFlatDomain s then
                Some (startX s)
            else
                let x0,y0 = startPoint s |> fun p -> p.X, p.Y
                let x1,y1 = endPoint s |> fun p -> p.X, p.Y
                let x = Math.Interpolation.linear y y0 x0 y1 x1
                Some x
        else
            None

    let inline interpolateX y s = 
        if isFlatDomain s then
           (startX s)
        else
            let x0,y0 = startPoint s |> fun p -> p.X, p.Y
            let x1,y1 = endPoint s |> fun p -> p.X, p.Y
            Math.Interpolation.linear y y0 x0 y1 x1

    let inline divide interpolateF (xs:seq<'a>) segment =
        seq {
            yield startPoint segment
            yield! Seq.filter (fun x -> isInRange IntervalType.T.Open x segment) xs |> Seq.map (fun x -> Point.make(x, interpolateF x segment))
            yield endPoint segment
        } |> Seq.pairwise 
        |> Seq.map makeContinuous

       

    let inline intersection s1 s2 =
        let x0,y0 = startPoint s1 |> fun p -> p.X, p.Y
        let x1,y1 = endPoint s1 |> fun p -> p.X, p.Y
        let x2,y2 = startPoint s2 |> fun p -> p.X, p.Y
        let x3,y3 = endPoint s2 |> fun p -> p.X, p.Y
        Math.intersection x0 y0 x1 y1 x2 y2 x3 y3
        |> Option.getOrElseWith None (fun (x,y) -> Some(Point.make(x,y)))

    let tryFindValue interpolateF intervalType x s = 
        let interpolate = function
            | Instantaneous p -> Some p.Y
            | Discrete (_,value) -> Some value 
            | Continuous (p1,p2) -> 
                match interpolateF with
                | Some f -> f x s
                | None -> Some (startY s)
        if isInRange intervalType x s 
        then interpolate s
        else None

    /// Returns true if the predicate applied to either the start point or the
    /// end point returns true, otherwise false
    let exists pred s =
        (startPoint >> pred) s || (endPoint >> pred) s

    /// Returns true if the predicate applied to both the start point and the 
    /// end point returns true, otherwise false
    let forall pred s =
        (startPoint >> pred) s && (endPoint >> pred) s

    /// Returns true if the predicate applied to either the start value or the
    /// end value returns true, otherwise false
    let existsY pred s =
        exists (Point.y >> pred) s 

    /// Returns true if the predicate applied to both the start value and the
    /// end value returns true, otherwise false
    let forallY pred s =
        forall (Point.y >> pred) s

    let inline delta s = Interval.delta (range s)

    let inline length unitF s = delta s |> unitF

    let inline area unitF (s:T<_,float<'u>>) =
         let startValue = startY s
         let endValue = endY s
         let duration = length unitF s 
         (startValue + endValue) / 2.0 * duration

    let inline tryArea unitF (s:T<_,float<'u> option>) =
        Option.maybe { 
            let! startValue = startY s
            let! endValue = endY s
            let duration = length unitF s 
            return (startValue + endValue) / 2.0 * duration
         }

    module Time =

        open System

        type T<'v> = T<DateTimeOffset,'v>

        let deltaTime s = Interval.Time.delta (range s)

        let duration timeUnitF s = deltaTime s |> timeUnitF

        let tryFindValue segmentInterpolateF intervalType (t:DateTimeOffset) s =   
            let interpolate = function
                | Instantaneous p -> p.Y
                | Discrete (_,value) -> value 
                | Continuous (p1,p2) -> 
                    match segmentInterpolateF with
                    | Some f -> f t s
                    | None -> startY s
            if isInRange intervalType t s 
            then Some (interpolate s)
            else None

        let tryInterpolateValue<[<Measure>]'u> (time:DateTimeOffset) (s:T<float<'u>>) =
            if isInRange IntervalType.T.Closed time s then
                let y0 = startY s
                let y1 = endY s
                let startX = float (startX s).Ticks
                let endX = float (endX s).Ticks
                let x = float time.Ticks
                let y = Math.Interpolation.linear x startX y0 endX y1
                Some y
            else
                None 

        let interpolateValue<[<Measure>]'u> (time:DateTimeOffset) (s:T<float<'u>>) =
            let y0, y1 = startY s, endY s
            let startX = float (startX s).Ticks
            let endX = float (endX s).Ticks
            let x = float time.Ticks
            Math.Interpolation.linear x startX y0 endX y1

        let tryInterpolateTime value (s:T<float<'u>>) = 
            if isInDomain IntervalType.T.Closed value s then
                if isFlatDomain s then
                    Some (startX s)
                else
                    let x0 = float (startX s).Ticks
                    let x1 = float (endX s).Ticks
                    let y0 = startY s
                    let y1 = endY s 
                    let x = Math.Interpolation.linear value y0 x0 y1 x1
                    Some (DateTimeOffset(int64 x, (startX s).Offset))
            else
                None    
                
        let interpolateTime value (s:T<float<'u>>) = 
            if isFlatDomain s then
                startX s
            else
                let x0 = float (startX s).Ticks
                let x1 = float (endX s).Ticks
                let y0 = startY s
                let y1 = endY s
                let x = Math.Interpolation.linear value y0 x0 y1 x1
                (DateTimeOffset(int64 x, (startX s).Offset))               

        let intersection<[<Measure>]'u> (s1:T<float<'u>>) (s2:T<float<'u>>) =
            let y0 = startY s1
            let y1 = endY s1
            let y2 = startY s2
            let y3 = endY s2
            let x0 = float (startX s1).Ticks
            let x1 = float (endX s1).Ticks
            let x2 = float (startX s2).Ticks
            let x3 = float (endX s2).Ticks
            Math.intersection x0 (float y0) x1 (float y1) x2 (float y2) x3 (float y3)
            |> Option.getOrElseWith None (fun (t,v) ->
                let time = DateTimeOffset(int64 t, (startX s1).Offset)
                let value = LanguagePrimitives.FloatWithMeasure<'u> v 
                Some(Point.make(time, value)))