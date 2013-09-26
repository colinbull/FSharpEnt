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

    let startPoint = function
        | Instantaneous p -> p
        | Discrete (i,v) -> Point.make(Interval.left i,v) 
        | Continuous (p,_) -> p

    let endPoint = function
        | Instantaneous p -> p
        | Discrete (i,v) -> Point.make(Interval.right i,v) 
        | Continuous (_,p) -> p    

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
         then External segment
         elif segStart >= iStart && segEnd <= iEnd
         then Internal segment
         elif segStart < iStart && segEnd <= iEnd
         then OverlapStart (iStart,segment)
         elif segStart >= iStart && segEnd > iEnd
         then OverlapEnd (iEnd, segment)
         else Overlap (iStart, iEnd, segment)

    let isFlat s =
        startY s = endY s

    let inline interpolateY x s =
        if isInRange IntervalType.T.Closed x s then
            let x0,y0 = startPoint s |> fun p -> p.X, p.Y 
            let x1,y1 = endPoint s |> fun p -> p.X, p.Y 
            let y = Math.Interpolation.linear x x0 y0 x1 y1
            Some y
        else
            None

    let inline interpolateX y s = 
        if isInDomain IntervalType.T.Closed y s then
            if isFlat s then
                Some (startX s)
            else
                let x0,y0 = startPoint s |> fun p -> p.X, p.Y
                let x1,y1 = endPoint s |> fun p -> p.X, p.Y
                let x = Math.Interpolation.linear y y0 x0 y1 x1
                Some x
        else
            None

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
