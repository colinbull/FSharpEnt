#r @"D:\dev\FSharp.Enterprise\packages\FSharpx.Core.1.8.32\lib\40\FSharpx.Core.dll"
#load "Option.fs"
#load "DateTime.fs"
#load "Interval.fs"

open FSharp.Enterprise

module Point =

    type T<'x,'y> = {
        X : 'x
        Y : 'y
    }

    let make (x,y) = { X = x; Y = y }
    let x p = p.X
    let y p = p.Y
    let mapX f p = make (f p.X,p.Y)
    let mapY f p = make (p.X,f p.Y)


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
       
    let mapX f = function
        | Instantaneous p -> makeInstantaneous (Point.mapX f p)
        | Discrete (i, v) as s -> makeDiscrete (Interval.make(f (Interval.left i), f (Interval.right i)), v) 
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


module Line =
    
    open System.Runtime.Serialization     
    open FSharpx
    open FSharpx.Option
    open FSharp.Enterprise.OptionOperators

    type LineType =
        | InstantaneousSegments
        | DiscreteSegments of IntervalType.T
        | ContinuousSegments

    type T<'x,'y> = {
        Type : LineType
        Segments : Segment.T<'x,'y> array
    }

    let inline checkLineType argName ``type`` line = 
        if line.Type <> ``type`` then
            let message = sprintf "invalid line type: expected %A but was %A" ``type`` line.Type
            invalidArg argName message

    let internal makeFromSegments lineType segments =
        { Type = lineType; Segments = segments }
                                
    let make lineType points =             
        let segments =
            match lineType with
            | InstantaneousSegments ->
                points 
                |> Seq.map Segment.makeInstantaneous 
                |> Seq.toArray
            | DiscreteSegments _ ->
                if Seq.length points = 1 then
                    let p = Seq.head points
                    [| Segment.makeDiscrete (Interval.make(p.X,p.X), p.Y) |]
                else
                    Seq.pairwise points
                    |> Seq.map (fun (p1,p2) -> Segment.makeDiscrete (Interval.make(p1.X,p2.X), p1.Y))
                    |> Seq.toArray
            | ContinuousSegments _ ->
                if Seq.length points = 1 then
                    let p = Seq.head points
                    [| Segment.makeContinuous (p,p) |]
                else             
                    Seq.pairwise points
                    |> Seq.map Segment.makeContinuous 
                    |> Seq.toArray
        makeFromSegments lineType segments

    let empty lineType =
        makeFromSegments lineType [||]

    let makeInstantaneous points = make LineType.InstantaneousSegments points
    let emptyInstantaneous () = empty LineType.InstantaneousSegments

    let makeDiscrete intervalType points = make (LineType.DiscreteSegments intervalType) points
    let makeDiscreteFromSegments intervalType segments = 
        { Type = DiscreteSegments intervalType; Segments = segments }
    let emptyDiscrete intervalType = empty (LineType.DiscreteSegments intervalType)

    let makeContinuous points = make LineType.ContinuousSegments points
    let makeContinuousFromSegments intervalType segments = 
        { Type = ContinuousSegments; Segments = segments }
    let emptyContinuous () = empty LineType.InstantaneousSegments

    let isEmpty line =
        line.Segments.Length = 0

    let segmentIntervalType line = 
        match line.Type with
        | InstantaneousSegments 
        | ContinuousSegments -> IntervalType.T.Closed
        | DiscreteSegments intervalType -> intervalType

    let segments line =
        line.Segments

    let segmentCount line =
        line.Segments.Length

    let startSegment line =
        if segmentCount line > 0 
        then Some line.Segments.[0]
        else None
            
    let endSegment line =
        if segmentCount line > 0 
        then Some line.Segments.[(segmentCount line) - 1]
        else None

    let startPoint line =
        line |> startSegment |> Option.map Segment.startPoint
            
    let endPoint line =
        line |> endSegment |> Option.map Segment.endPoint

    let startX line = 
        Option.map Point.x (startPoint line)

    let endX line = 
        Option.map Point.x (endPoint line)

    let startY line = 
        Option.getOrElseWith None Point.y (startPoint line)

    let endY line = 
        Option.getOrElseWith None Point.y (endPoint line)

    let range line =
        (fun t1 t2 -> Interval.make(t1,t2)) <!> startX line <*> endX line

    let inline map f line =
        let segments = Array.map f line.Segments
        { Type = line.Type; Segments = segments }   

    let inline map2 f l1 l2 =
        if l1.Type <> l2.Type then (invalidArg "l2" "line types differ")
        let segments = Array.map2 f l1.Segments l2.Segments
        { Type = l1.Type; Segments = segments }   

    let inline mapY f line =
        let segments = Array.map (Segment.mapY f) line.Segments 
        { Type = line.Type; Segments = segments }   

    let fold f state line =
        Array.fold f state line.Segments 

    let inline sumBy f line =
        fold (fun state segment -> f segment |> Option.accumulate state) None line

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let exists p line =
        Array.exists p line.Segments

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forall p line =
        Array.forall p line.Segments

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let existsPoint p line =
        exists (Segment.exists p) line 

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forallPoint p line =
        forall (Segment.forall p) line

    /// Returns true if the predicate applied to any segments returns true, otherwise false.
    let existsValue p line =
        exists (Segment.existsY p) line 

    /// Returns true if the predicate applied to all segments returns true, otherwise false.
    let forallValue p line =
        forall (Segment.forallY p) line

    let tryPick chooser line =
        Array.tryPick chooser line.Segments

    let endXs line =
        line 
        |> fold (fun s segment -> Segment.endX segment :: s) []
        |> List.rev

    let startXs line =
        line 
        |> fold (fun s segment -> Segment.startX segment :: s) []
        |> List.rev

    let xs line =
        match line.Type with
        | LineType.InstantaneousSegments -> startXs line
        | _ -> 
            match startX line with
            | Some startX -> startX :: endXs line
            | _ -> []


    let inline intersections l1 l2 =
        [|
            for s1 in l1.Segments do
                for s2 in l2.Segments do
                    match Segment.intersection s1 s2 with
                    | Some point -> yield point
                    | None -> ()
        |]

    let tryFindSegment x line =        
        Array.tryFind (Segment.isInRange (segmentIntervalType line) x) line.Segments

    let tryFindValue segmentInterpolateF x line =            
        Array.tryPick (Segment.tryFindValue segmentInterpolateF (segmentIntervalType line) x) line.Segments

    let tryFindValues segmentInterpolateF x line =
        Array.choose (Segment.tryFindValue segmentInterpolateF (segmentIntervalType line) x) line.Segments

    /// Returns true if the predicate applied to the value at time t returns true, otherwise false.
    let isValueAtTime segmentInterpolateF t p line : bool =
        (tryFindValue segmentInterpolateF t >> p) line

    let toPoints line =
        match line.Type with
        | InstantaneousSegments -> 
            Array.fold (fun points segment -> Segment.startPoint segment :: points) [] line.Segments
        | DiscreteSegments _
        | ContinuousSegments _ ->
            match endPoint line with
            | Some endPoint ->
                let startPoints = Array.fold (fun points segment -> Segment.startPoint segment :: points) [] line.Segments
                endPoint :: startPoints
            | None -> []
        |> List.rev

    let slice segmentInterpolateF interval line =
        let segments = 
            match line.Type with
            | InstantaneousSegments ->
                Array.filter (fun segment -> Interval.isIn IntervalType.T.Closed (Segment.startX segment) interval) line.Segments
            | DiscreteSegments _ ->
                line.Segments
                |> Array.choose (fun seg -> 
                    match (interval, seg) with
                    | Segment.Overlap (iStart, iEnd, seg) ->
                        Some (Segment.makeDiscrete (interval, Segment.startY seg))
                    | Segment.Internal seg -> 
                        Some(seg)
                    | Segment.External seg -> 
                        None
                    | Segment.OverlapStart (dt,seg) ->
                        Some (Segment.map (fun (s,e) -> Point.mapX (fun _ -> dt) s, e) seg)
                    | Segment.OverlapEnd (dt,seg) ->
                        Some (Segment.map (fun (s,e) -> s, Point.map (fun (_,_) -> dt,s.Y) e) seg))
            | ContinuousSegments _ ->
                let sif = Option.get segmentInterpolateF
                line.Segments
                |> Array.choose (fun seg -> 
                    match (interval, seg) with
                    | Segment.Overlap (iStart, iEnd, seg) ->
                        Some (Segment.map (fun (s,e) -> 
                                    Point.map (fun _ -> iStart, sif iStart seg) s, 
                                    Point.map (fun _ -> iEnd, sif iEnd seg) e) seg)
                    | Segment.Internal seg -> 
                        Some(seg)
                    | Segment.External seg -> 
                        None
                    | Segment.OverlapStart (dt,seg) ->
                        Some (Segment.map (fun (s,e) -> Point.map (fun _ -> dt, sif dt seg) s, e) seg)
                    | Segment.OverlapEnd (dt,seg) ->
                        Some (Segment.map (fun (s,e) -> s, Point.map (fun _ -> dt, sif dt seg) e) seg))
        { Type = line.Type; Segments = segments}


module TimePoint =

    open System

    type T<'v> = | TimePoint of Point.T<DateTimeOffset,'v option>
    
    let make (x,y) = TimePoint (Point.make (x,y))    
    let time (TimePoint p) = p.X
    let value (TimePoint p) = p.Y
    let mapTime f (TimePoint p) = TimePoint (Point.mapX f p)
    let mapValue f (TimePoint p) = TimePoint (Point.mapY f p)
    let lift f (TimePoint p) = TimePoint (f p)
    let lift2 f (TimePoint p1) (TimePoint p2) = TimePoint (f p1 p2)


module TimeSegment =

    open System

    type T<'v> = | TimeSegment of Segment.T<DateTimeOffset,'v option>
    
    let makeInstantaneous (TimePoint.TimePoint p) = Segment.Instantaneous p
    let makeDiscrete (i,v) = Segment.Discrete (i,v)
    let makeContinuous (TimePoint.TimePoint p1,TimePoint.TimePoint p2) = Segment.Continuous (p1,p2)
    let startPoint (TimeSegment s) = Segment.startPoint s
    let endPoint (TimeSegment s) = Segment.endPoint s
    let startTime (TimeSegment s) = Segment.startX s
    let startValue (TimeSegment s) = Segment.startY s
    let endTime (TimeSegment s) = Segment.endX s
    let endValue (TimeSegment s) = Segment.endY s
    let mapTime f (TimeSegment s) = Segment.mapX f s 
    let mapValue f (TimeSegment s) = Segment.mapY f s 
    let mapValue2 f (TimeSegment s1) (TimeSegment s2) = Segment.mapY2 f s1 s2


