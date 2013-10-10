namespace FSharp.Enterprise

#if INTERACTIVE
open FSharp.Enterprise
#endif

module Segment =

    open FSharpx

    type T<'x,'y> =
        | Instantaneous of Point.T<'x,'y>
        | Discrete of IntervalType.T * Interval.T<'x> * 'y
        | Continuous of Point.T<'x,'y> * Point.T<'x,'y>

    let makeInstantaneous p = Instantaneous p
    let makeDiscrete (t, i,v) = Discrete (t, i,v)
    let makeContinuous (p1,p2) = Continuous (p1,p2)

    let emptyInstantaneous x = Instantaneous (Point.empty x)
    let emptyDiscrete t i = Discrete (t, i,Unchecked.defaultof<'y>)
    let emptyContinuous i = Continuous (Point.empty (Interval.left i),Point.empty (Interval.right i))

    let intervalType = function
         | Continuous _ | Instantaneous _ -> IntervalType.T.Closed
         | Discrete (t,_,_) -> t 

    let startPoint = function
        | Instantaneous p -> p
        | Discrete (_,i,v) -> Point.make(Interval.left i,v) 
        | Continuous (p,_) -> p

    let endPoint = function
        | Instantaneous p -> p
        | Discrete (_,i,v) -> Point.make(Interval.right i,v) 
        | Continuous (_,p) -> p    

    let unmake s =
        let p1,p2 = startPoint s, endPoint s
        p1.X,p1.Y,p2.X,p2.Y

    let startX = function
        | Instantaneous p -> p.X
        | Discrete (_,i,_) -> Interval.left i 
        | Continuous (p,_) -> p.X

    let startY = function
        | Instantaneous p -> p.Y
        | Discrete (_,_,v) -> v 
        | Continuous (p,_) -> p.Y

    let endX = function
        | Instantaneous p -> p.X
        | Discrete (_,i,_) -> Interval.right i 
        | Continuous (_,p) -> p.X

    let endY = function
        | Instantaneous p -> p.Y
        | Discrete (_,_,v) -> v 
        | Continuous (_,p) -> p.Y

    let range = function
        | Instantaneous p -> Interval.make(p.X,p.X)
        | Discrete (_,i,_) -> i 
        | Continuous (p1,p2) -> Interval.make(p1.X,p2.X)

    let isInRange intervalType x s =
        range s |> Interval.isIn intervalType x 

    let domain = function
        | Instantaneous p -> Interval.make(p.Y,p.Y)
        | Discrete (_,_,v) -> Interval.make(v,v) 
        | Continuous (p1,p2) -> Interval.make(p1.Y,p2.Y)

    let isInDomain intervalType y s =
        domain s |> Interval.isIn intervalType y 

    let isFlatDomain s =
        startY s = endY s

    let map f = function 
        | Instantaneous p -> makeInstantaneous (fst (f (p,p)))
        | Discrete (t,_,_) as s -> f (startPoint s, endPoint s) |> (fun (p1,p2:Point.T<'a,'b>) -> makeDiscrete (t, Interval.make(p1.X,p2.X),p1.Y)) 
        | Continuous (p1,p2) -> makeContinuous (f (p1,p2))
       
    let mapX f = function
        | Instantaneous p -> makeInstantaneous (Point.mapX f p)
        | Discrete (t,_, v) as s -> makeDiscrete (t, Interval.make(f (startX s), f (endX s)), v) 
        | Continuous (p1,p2) -> makeContinuous (Point.mapX f p1, Point.mapX f p2)
       
    let mapY f = function
        | Instantaneous p -> makeInstantaneous (Point.mapY f p)
        | Discrete (t, i, v) -> makeDiscrete (t, i, f v) 
        | Continuous (p1,p2) -> makeContinuous (Point.mapY f p1, Point.mapY f p2)

    let mapY2 f s1 s2 = 
        match s1, s2 with
        | Instantaneous p1, Instantaneous p2 when p1.X = p2.X -> 
            makeInstantaneous (Point.make(p1.X, f p1.Y p2.Y))
        | Discrete (t1, i1, v1), Discrete(t2, i2, v2) when i1 = i2 ->
            if t1 <> t2 then invalidArg "Interval Type" "The interval types must be consistent"
            makeDiscrete (t1, i1, f v1 v2)
        | Continuous (p1, p1'), Continuous (p2, p2') when p1.X = p2.X && p1'.X = p2'.X -> 
            makeContinuous (Point.make(p1.X, f p1.Y p2.Y), Point.make(p1'.X, f p1'.Y p2'.Y))
        | _ -> invalidArg "s2" "Time segments must be consistent with respect to time and type"

    let fold f state s =
        f state (startPoint s,endPoint s)

    let (|OverlapStart|OverlapEnd|Overlap|Internal|External|) (interval,s) =
        let l,r = Interval.left interval, Interval.right interval
        let x0,x1 = startX s, endX s
        if x1 <= l || x0 >= r
        then External (l,r,s)
        elif x0 >= l && x1 <= r
        then Internal (l,r,s)
        elif x0 < l && x1 <= r
        then OverlapStart (l,r,s)
        elif x0 >= l && x1 > r
        then OverlapEnd (l,r,s)
        else Overlap (l,r,s)
           
    let inline interpolateY x s =
        let x0,y0,x1,y1 = unmake s 
        Math.Interpolation.linear x x0 y0 x1 y1

    let inline tryInterpolateY x s =
        if isInRange (intervalType s) x s 
        then Some (interpolateY x s)
        else None

    let inline interpolateX y s = 
        if isFlatDomain s then
           (startX s)
        else
            let x0,y0,x1,y1 = unmake s
            Math.Interpolation.linear y y0 x0 y1 x1

    let inline tryInterpolateX y s = 
        if isInDomain IntervalType.T.Closed y s 
        then Some (interpolateX y s)
        else None

    let inline divide interpolateF xs s =
        seq {
            yield startPoint s
            yield! Seq.filter (fun x -> isInRange IntervalType.T.Open x s) xs |> Seq.map (fun x -> Point.make(x, interpolateF x s))
            yield endPoint s
        } |> Seq.pairwise 
        |> Seq.map makeContinuous

    let inline intersection s1 s2 =
        let x0,y0,x1,y1 = unmake s1
        let x2,y2,x3,y3 = unmake s2
        Math.intersection x0 y0 x1 y1 x2 y2 x3 y3
        |> Option.getOrElseWith None (fun (x,y) -> Some(Point.make(x,y)))

    let tryFindValue interpolateF intervalType x s = 
        let interpolate = function
            | Instantaneous p -> Some p.Y
            | Discrete (_,_,value) -> Some value 
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
                | Discrete (_,_,value) -> value 
                | Continuous (p1,p2) -> 
                    match segmentInterpolateF with
                    | Some f -> f t s
                    | None -> startY s
            if isInRange intervalType t s 
            then Some (interpolate s)
            else None

        let interpolateValue (time:DateTimeOffset) (s:T<float<'u>>) =
            let x0,y0,x1,y1 = unmake s
            let x = float time.Ticks
            Math.Interpolation.linear x (float x0.Ticks) y0 (float x1.Ticks) y1

        let tryInterpolateValue time s =
            if isInRange IntervalType.T.Closed time s 
            then Some (interpolateValue time s)
            else None 
                
        let interpolateTime value (s:T<float<'u>>) = 
            if isFlatDomain s then
                startX s
            else
                let x0,y0,x1,y1 = unmake s
                let x = Math.Interpolation.linear value y0 (float x0.Ticks) y1 (float x1.Ticks)
                (DateTimeOffset(int64 x, (startX s).Offset))               

        let tryInterpolateTime value s = 
            if isInDomain IntervalType.T.Closed value s 
            then Some (interpolateTime value s)
            else None    

        let intersection (s1:T<float<'u>>) (s2:T<float<'u>>) =
            let x0,y0,x1,y1 = unmake s1
            let x2,y2,x3,y3 = unmake s2
            Math.intersection (float x0.Ticks) (float y0) (float x1.Ticks) (float y1) (float x2.Ticks) (float y2) (float x3.Ticks) (float y3)
            |> Option.getOrElseWith None (fun (t,v) ->
                let time = DateTimeOffset(int64 t, (startX s1).Offset)
                let value = LanguagePrimitives.FloatWithMeasure<'u> v 
                Some(Point.make(time, value)))
