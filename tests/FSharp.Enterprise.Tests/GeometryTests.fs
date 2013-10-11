namespace FSharp.Enterprise.Tests

open NUnit.Framework
open FsUnit
open System
open FSharp.Enterprise.OptionOperators
open FSharp.Enterprise

module Helper =

    let d0 = DateTimeOffset(2013,5,9,0,0,0,TimeSpan.FromHours(0.0))

    let getPoints (startTime:DateTimeOffset) (timeStep:float) (values:'a []) =
        [|
            for i in 0 .. values.Length - 1 do
                yield Point.make(startTime.AddMinutes(float i * float timeStep), values.[i])
        |]

    let getPointsDiscrete (intervalType:IntervalType.T) (startTime:DateTimeOffset) (timeStep:float) (values:'a []) =
        [|
            for i in 0 .. values.Length - 1 do
                let t1 = startTime.AddMinutes(float i * float timeStep)
                yield intervalType, Interval.make(t1, t1.AddMinutes(float timeStep)), values.[i] 
        |]


              
[<TestFixture; Category("Unit")>]
type ``Given the Point Time module`` () =

    let d1 = DateTimeOffset(2013,2,5,0,0,0,TimeSpan.FromHours(0.))

    [<Test>]
    member x.``I can make an empty time point`` () =
        let time = DateTimeOffset.UtcNow
        let point:Point.Time.T<float> = Point.empty time
        Point.Time.time point |> should equal time
        Point.Time.value point |> should equal 0.0

    [<Test>]
    member x.``I can make a time point`` () =
        let time = DateTimeOffset.UtcNow
        let point = Point.make(d1, 11.0)
        (Point.Time.time point) |> should equal d1
        Point.Time.value point |> should equal 11.0

    [<Test>]
    member x.``I can make a time point with unit of measure`` () =
        let time = DateTimeOffset.UtcNow
        let point = Point.make(d1, 11.0)
        (Point.Time.time point) |> should equal d1
        Point.Time.value point |> should equal 11.0

type ``Given the Segment module`` () =

    [<Test>]
    member x.``I can divide a segment when an interval is external and preceeds the segment``() =
        let segment = Segment.makeContinuous (Point.make(5.,5.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [1.;2.] segment 
        actual |> should equal [|segment|]

    [<Test>]
    member x.``I can divide a segment when an interval is external and preceeds the segment but endpoints align``() =
        let segment = Segment.makeContinuous (Point.make(5.,5.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [10.;12.] segment 
        actual |> should equal [|segment|]


    [<Test>]
    member x.``I can divide a segment when an interval is external and preceeds the segment but startpoints align``() =
        let segment = Segment.makeContinuous (Point.make(5.,5.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [2.;5.] segment 
        actual |> should equal [|segment|]

    [<Test>]
    member x.``I can divide a segment when an interval is external and follows the segment``() = 
        let segment = Segment.makeContinuous (Point.make(5.,5.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [11.;12.] segment 
        actual |> should equal [|segment|]

    [<Test>]
    member x.``I can divide a segment when an interval is entirely internal to the segment``() = 
        let segment = Segment.makeContinuous (Point.make(0.,0.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [3.;7.] segment  
        let expected = 
            [
                Segment.makeContinuous (Point.make(0.,0.), Point.make(3.,3.))
                Segment.makeContinuous (Point.make(3.,3.), Point.make(7.,7.))
                Segment.makeContinuous (Point.make(7.,7.), Point.make(10.,10.))
            ]
        actual |> should equal expected

    [<Test>]
    member x.``I can divide a segment when an interval is entirely internal to the segment but start points coincide``() = 
        let segment = Segment.makeContinuous (Point.make(0.,0.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [0.;7.] segment  
        let expected = 
            [
                Segment.makeContinuous (Point.make(0.,0.), Point.make(7.,7.))
                Segment.makeContinuous (Point.make(7.,7.), Point.make(10.,10.))
            ]
        actual |> should equal expected

    [<Test>]
    member x.``I can divide a segment when an interval is entirely internal to the segment but end points coincide``() = 
        let segment = Segment.makeContinuous (Point.make(0.,0.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [7.;10.] segment  
        let expected = 
            [
                Segment.makeContinuous (Point.make(0.,0.), Point.make(7.,7.))
                Segment.makeContinuous (Point.make(7.,7.), Point.make(10.,10.))
            ]
        actual |> should equal expected

    [<Test>]
    member x.``I can divide a segment when an interval is entirely internal to the segment but start and end points coincide``() = 
        let segment = Segment.makeContinuous (Point.make(0.,0.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [0.;10.] segment  
        let expected = 
            [
                Segment.makeContinuous (Point.make(0.,0.), Point.make(10.,10.))
            ]
        actual |> should equal expected

    [<Test>]
    member x.``I can divide a segment when an interval bounds the segments end points``() = 
        let segment = Segment.makeContinuous (Point.make(0.,0.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [8.;12.] segment  
        let expected = 
            [
                Segment.makeContinuous (Point.make(0.,0.), Point.make(8.,8.))
                Segment.makeContinuous (Point.make(8.,8.), Point.make(10.,10.))
            ]
        actual |> should equal expected

    [<Test>]
    member x.``I can divide a segment when an interval bounds the segments start points``() = 
        let segment = Segment.makeContinuous (Point.make(0.,0.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [-2.;2.] segment  
        let expected = 
            [
                Segment.makeContinuous (Point.make(0.,0.), Point.make(2.,2.))
                Segment.makeContinuous (Point.make(2.,2.), Point.make(10.,10.))
            ]
        actual |> should equal expected

    [<Test>]
    member x.``I can divide a segment when an interval encloses the segment``() = 
        let segment = Segment.makeContinuous (Point.make(0.,0.),Point.make(10.,10.))
        let actual = Segment.divide Segment.interpolateX [-2.;12.] segment  
        let expected = 
            [
                Segment.makeContinuous (Point.make(0.,0.), Point.make(10.,10.))
            ]
        actual |> should equal expected


    [<Test>]
    member x.``the intersection calculation for two intersecting line segments should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d1
        let lineSegment1 = Segment.makeContinuous (Point.make(d0, 0.0), Point.make(d1, 100.0))
        let lineSegment2 = Segment.makeContinuous (Point.make(d2, 100.0), Point.make(d3, 0.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        Point.Time.time intersection.Value |> should equal (d0.AddHours(12.0))
        intersection.Value.Y |> should equal 50.0
  
    [<Test>]
    member x.``the intersection calculation for two parallel line segments should return None`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0.AddHours(1.0)
        let d3 = d2.AddDays(1.0)
        let lineSegment1 = Segment.makeContinuous (Point.make(d0, 0.0), Point.make(d1, 100.0))
        let lineSegment2 = Segment.makeContinuous (Point.make(d2, 0.0), Point.make(d3, 100.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        intersection |> should equal None  

    [<Test>]
    member x.``the intersection calculation for two non-intersecting non-parallel line segments should return None`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = Segment.makeContinuous (Point.make(d0, 0.0), Point.make(d1, 100.0))
        let lineSegment2 = Segment.makeContinuous (Point.make(d2, 100.0), Point.make(d3, 100.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        intersection |> should equal None

    [<Test>]
    member x.``the intersection calculation for two line segments that touch at the end point of the first segment should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = Segment.makeContinuous (Point.make(d2, 50.0), Point.make(d3, 50.0))
        let lineSegment2 = Segment.makeContinuous (Point.make(d0, 0.0), Point.make(d1, 100.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        intersection.Value.X |> should equal d3
        intersection.Value.Y |> should equal 50.0

    [<Test>]
    member x.``the intersection calculation for two line segments that touch at the start point of the first segment should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0.AddHours(12.0)
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = Segment.makeContinuous(Point.make(d2, 50.0), Point.make(d3, 50.0))
        let lineSegment2 = Segment.makeContinuous(Point.make(d0, 0.0), Point.make(d1, 100.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        intersection.Value.X |> should equal d2
        intersection.Value.Y |> should equal 50.0

    [<Test>]
    member x.``the intersection calculation for two line segments that touch at the end point of the second segment should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = Segment.makeContinuous(Point.make(d0, 0.0), Point.make(d1, 100.0))
        let lineSegment2 = Segment.makeContinuous(Point.make(d2, 50.0), Point.make(d3, 50.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        intersection.Value.X |> should equal d3
        intersection.Value.Y |> should equal 50.0

    [<Test>]
    member x.``the intersection calculation for two line segments that touch at the start point of the second segment should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0.AddHours(12.0)
        let d3 = d2.AddHours(12.0)
        let lineSegment1 = Segment.makeContinuous(Point.make(d0, 0.0), Point.make(d1, 100.0))
        let lineSegment2 = Segment.makeContinuous(Point.make(d2, 50.0), Point.make(d3, 50.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        intersection.Value.X |> should equal d2
        intersection.Value.Y |> should equal 50.0

    [<Test>]
    member x.``the intersection calculation for two collinear line segments should return None`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0.AddHours(12.0)
        let d3 = d2.AddDays(1.0)
        let lineSegment1 = Segment.makeContinuous(Point.make(d0, 0.0), Point.make(d1, 100.0))
        let lineSegment2 = Segment.makeContinuous(Point.make(d2, 50.0), Point.make(d3, 150.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        intersection |> should equal None

    [<Test>]
    member x.``the intersection calculation for two line segments with coincident start points should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d1
        let lineSegment1 = Segment.makeContinuous(Point.make(d0, 0.0), Point.make(d1, 100.0))
        let lineSegment2 = Segment.makeContinuous(Point.make(d2, 0.0), Point.make(d3, 150.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        intersection.Value.X |> should equal d0
        intersection.Value.Y |> should equal 0.0

    [<Test>]
    member x.``the intersection calculation for two line segments with coincident end points should return Some`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d0
        let d3 = d1
        let lineSegment1 = Segment.makeContinuous(Point.make(d0, 100.0), Point.make(d1, 100.0))
        let lineSegment2 = Segment.makeContinuous(Point.make(d2, 0.0), Point.make(d3, 100.0))
        let intersection = Segment.Time.intersection lineSegment1 lineSegment2
        intersection.Value.X |> should equal d1
        intersection.Value.Y |> should equal 100.0

    [<Test>]
    member x.``the interpolateTime on a flat line at the line value is correct`` () =
        let startTime = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.Zero)
        let endTime = startTime.AddMinutes(30.0)
        let segment = Segment.makeContinuous(Point.make(startTime, 100.0),Point.make(endTime, 100.0))
        let actual = Segment.Time.tryInterpolateTime 100.0 segment
        let expected = Some startTime
        actual |> should equal expected

    [<Test>]
    member x.``the interpolateTime on a line at the line value is correct`` () =
        let startTime = DateTimeOffset(2013,5,6,7,0,0,TimeSpan.FromHours(1.0))
        let endTime = startTime.AddMinutes(30.0)
        let segment = Segment.makeContinuous(Point.make(startTime, 105.0),Point.make(endTime, 95.0))
        let actual = Segment.Time.tryInterpolateTime 100.0 segment
        let expected = Some (startTime.AddMinutes(15.0))
        actual |> should equal expected

type ``Given the TimeLine module`` () =

    [<Test>]
    member x.``I can make an empty TimeLine without unit of measure`` () =
        let actual : Line.Time.T<int> = Line.make Segment.makeContinuous []
        let expected : Segment.T<int,int> array = [||]
        (actual |> Line.segments) |> should equal expected 

    [<Test>]
    member x.``I can make a TimeLine from a sequence of points containing a single point`` () =
        let points = Helper.getPoints Helper.d0 30.0 [|0.0|]        
        let actual = Line.make Segment.makeContinuous [(points.[0], points.[0])]
        let expected : Line.Time.T<float> = 
            Line.ofSegments 
                [| Segment.makeContinuous (Point.make(Helper.d0, 0.0), Point.make(Helper.d0, 0.0))|]
        actual |> should equal expected 

    [<Test>]
    member x.``I can make a TimeLine from a sequence of two points`` () =
        let points = Helper.getPoints Helper.d0 30.0 [|0.0; 100.0|]        
        let actual = Line.make Segment.makeContinuous [points.[0], points.[1]]
        let expected : Line.Time.T<float> = 
            Line.ofSegments
                [| Segment.makeContinuous(Point.make(Helper.d0, 0.0),Point.make(Helper.d0.AddMinutes(30.0), 100.0)) |]
        actual |> should equal expected 

    [<Test>]
    member x.``I can make a TimeLine from a sequence of three points`` () =
        let points = Helper.getPoints Helper.d0 30.0 [|0.0; 100.0; 200.0|]        
        let actual = Line.make Segment.makeContinuous [points.[0], points.[1]; points.[1], points.[2]]
        let expected : Line.Time.T<float> = 
            Line.ofSegments 
                    [|
                        Segment.makeContinuous(Point.make(Helper.d0, 0.0),Point.make(Helper.d0.AddMinutes(30.0), 100.0))
                        Segment.makeContinuous(Point.make(Helper.d0.AddMinutes(30.0), 100.0),Point.make(Helper.d0.AddMinutes(60.0), 200.0))
                    |]
        actual |> should equal expected 

    [<Test>]
    member x.``I can make a time interval from a line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.range line
        let expected = Some(Interval.make(Helper.d0,Helper.d0.AddMinutes(60.0)))
        actual |> should equal expected 

    [<Test>]
    member x.``I can get the start point of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.startPoint line
        let expected = Some(Point.make(Helper.d0, 0.0))
        actual |> should equal expected 

    [<Test>]
    member x.``I can get the end point of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.endPoint line
        let expected = Some(Point.make(Helper.d0.AddMinutes(60.0), 200.0))
        actual |> should equal expected 

    [<Test>]
    member x.``I can get the start point of the empty line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [||]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.startPoint line
        let expected = None
        actual |> should equal expected 

    [<Test>]
    member x.``I can get the end point of the empty line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [||]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.endPoint line
        let expected = None
        actual |> should equal expected 

    [<Test>]
    member x.``I can get all of the insection points with another line`` () =
        let d0 = DateTimeOffset(2013,5,6,0,0,0,TimeSpan.FromHours(0.))
        let d1 = d0.AddDays(1.0)
        let d2 = d1.AddDays(1.0)
        let d3 = d2.AddDays(1.0)
        let line1 = 
            Line.make Segment.makeContinuous 
                (Seq.pairwise [
                     Point.make(d0, 0.0)
                     Point.make(d1, 100.0)
                     Point.make(d2, 100.0)
                     Point.make(d3, 0.0)
                 ])
        let line2 =
            Line.make Segment.makeContinuous 
                (Seq.pairwise [
                    Point.make(d0, 50.0)
                    Point.make(d3, 50.0)
                ])
        let intersections = Line.Time.intersections line1 line2
        Array.length intersections |> should equal 2
        intersections.[0].X |> should equal (d0.AddHours(12.0))
        intersections.[0].Y |> should equal 50.0
        intersections.[1].X |> should equal (d2.AddHours(12.0))
        intersections.[1].Y |> should equal 50.0

    [<Test>]
    member x.``I can map a function over the segments of a line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 50.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.map (Segment.mapY ((*) 1000.)) line
        let expected =
            [|
                Point.make(Helper.d0, 50000.0)
                Point.make(Helper.d0.AddMinutes(30.0), 100000.0)
                Point.make(Helper.d0.AddMinutes(60.0), 200000.0)
            |] |> Seq.pairwise |> Line.make Segment.makeContinuous  
        actual |> should equal expected

    [<Test>]
    member x.``I can fold a function over the segments of a line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 50.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let energy (lineSegment:Segment.Time.T<float>) =
            let power = (Segment.startY lineSegment + Segment.endY lineSegment) / 2.0 
            let time = (lineSegment |> Segment.Time.deltaTime).TotalMinutes
            power * time 
        let actual = Line.fold (fun s lineSegment -> s + energy lineSegment) 0.0 line
        let expected = 6750.0
        actual |> should equal expected 
  
    [<Test>]
    member x.``I can get the start time of each segment of an empty line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [||]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.startXs line
        let expected = []        
        actual |> should equal expected 
 
    [<Test>]
    member x.``I can get the end time of each segment of an empty line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [||]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.endXs line
        let expected = []        
        actual |> should equal expected 
 
    [<Test>]
    member x.``I can get the start time of each segment of a line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [|Some 50.0; Some 100.0; Some 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.startXs line
        let expected = [Helper.d0.AddMinutes(0.0);Helper.d0.AddMinutes(30.0)]        
        actual |> should equal expected 
 
    [<Test>]
    member x.``I can get the end time of each segment of a line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [|Some 50.0; Some 100.0; Some 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.endXs line
        let expected = [Helper.d0.AddMinutes(30.0);Helper.d0.AddMinutes(60.0)]        
        actual |> should equal expected

    [<Test>]
    member x.``I can add points into a line at a given interval``() =
        let points = 
            [   Point.make(0., 0.); Point.make(10., 10.); 
                Point.make(35., 35.); Point.make(56., 56.);
                Point.make(116., 116.); Point.make(146., 146.) ]
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.divide Segment.interpolateX [0.;30.;60.;90.;120.;150.] line
        let expected = 
            [   Point.make(0., 0.); Point.make(10., 10.); Point.make (30., 30.) 
                Point.make(35., 35.); Point.make(56., 56.); Point.make (60., 60.); Point.make (90., 90.)
                Point.make(116., 116.); Point.make (120., 120.); Point.make(146., 146.) ]
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        actual |> should equal expected  
        
    [<Test>]
    member x.``I can add points into a line at a given time interval`` () =
        let points = Helper.getPoints Helper.d0 12.0 [| 0.0; 12.0; 24.0; 36.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.divide Segment.Time.interpolateValue ([0.;30.;60.] |> List.map (fun x -> Helper.d0.AddMinutes(x))) line
        let expected = [
                           Point.make (Helper.d0, 0.);
                           Point.make (Helper.d0.AddMinutes(12.), 12.0);
                           Point.make (Helper.d0.AddMinutes(24.), 24.0);
                           Point.make (Helper.d0.AddMinutes(30.), 30.0);
                           Point.make (Helper.d0.AddMinutes(36.), 36.0);
                       ]  |> Seq.pairwise |> Line.make Segment.makeContinuous     
        actual |> should equal expected      


type ``Given a continuous TimeLine`` () =

    [<Test>]
    member x.``I can create an empty line`` () =
        let actual = Line.empty
        let expected = Line.make Segment.makeContinuous []
        actual |> should equal expected 

    [<Test>]
    member x.``I can take a slice of the timeline with a single element`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.slice (Some Segment.Time.interpolateValue) (Helper.d0,Helper.d0.AddMinutes(30.)) line
        let expected = 
            [|
                Point.make (Helper.d0, 0.0)
                Point.make(Helper.d0.AddMinutes(30.), 100.)
            |] |> Seq.pairwise |> Line.make Segment.makeContinuous
        actual |> should equal expected 

    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval ends in the middle of a segement but spanning two segments``() =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.; 200.; 250.|] 
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let interval = Interval.make (Helper.d0.AddMinutes(15.),Helper.d0.AddMinutes(45.))
        let actual = Line.slice (Some Segment.Time.interpolateValue) interval line
        let expected = 
            [|
                Point.make (Helper.d0.AddMinutes(15.), 50.0)
                Point.make (Helper.d0.AddMinutes(30.), 100.0)
                Point.make(Helper.d0.AddMinutes(45.), 150.)
            |] |> Seq.pairwise |> Line.make Segment.makeContinuous
        actual |> should equal expected 

    [<Test>]
    member x.``I can take a slice of a TimeLine when a single segment wraps the interal completely``() =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.;|] 
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let interval = Interval.make (Helper.d0.AddMinutes(10.),Helper.d0.AddMinutes(15.))
        let actual = Line.slice (Some Segment.Time.interpolateValue) interval line
        Segment.startX (Line.startSegment actual).Value |> should equal (Helper.d0.AddMinutes(10.))
        Segment.endX (Line.startSegment actual).Value |> should equal (Helper.d0.AddMinutes(15.))
        Segment.startY (Line.startSegment actual).Value |> should (FsUnit.TopLevelOperators.equalWithin 0.1) 33.3
        Segment.endY (Line.startSegment actual).Value |> should equal 50.0

    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval is fully within the timeline``() =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.; 200.; 250.|] 
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let interval = Interval.make (Helper.d0.AddMinutes(30.),Helper.d0.AddHours(1.))
        let actual = Line.slice (Some Segment.Time.interpolateValue) interval line
        let expected = 
            [|
                Point.make (Helper.d0.AddMinutes(30.), 100.0)
                Point.make(Helper.d0.AddMinutes(60.), 200.)
            |] |> Seq.pairwise |> Line.make Segment.makeContinuous  
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value before the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.Time.tryFindValue (Some Segment.Time.interpolateValue) (Helper.d0.AddMinutes(-1.0)) line
        let expected = None
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.Time.tryFindValue (Some Segment.Time.interpolateValue) Helper.d0 line
        let expected = Some 0.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.Time.tryFindValue (Some Segment.Time.interpolateValue) (Helper.d0.AddMinutes(60.0)) line
        let expected = Some 200.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value after the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.Time.tryFindValue (Some Segment.Time.interpolateValue) (Helper.d0.AddMinutes(61.0)) line
        let expected = None
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the start of a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.Time.tryFindValue (Some Segment.Time.interpolateValue) (Helper.d0.AddMinutes(30.0)) line
        let expected = Some 100.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the within a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.Time.tryFindValue (Some Segment.Time.interpolateValue) (Helper.d0.AddMinutes(45.0)) line
        let expected = Some 150.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert a line to a seq`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0;  30.0; 60.0|]        
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.Time.toSeq IntervalType.T.Closed (Some Segment.Time.interpolateValue) (TimeSpan.FromMinutes(1.0)) line |> Seq.toArray
        let expected = [0 .. 60] |> Seq.map (fun i -> float i |> Some) |> Seq.toArray
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert an empty line to a seq`` () =
        let line : Line.Time.T<float> = Line.make Segment.makeContinuous []
        let actual = Line.Time.toSeq IntervalType.T.Closed (Some Segment.Time.interpolateValue) (TimeSpan.FromMinutes(1.0)) line
        let expected = Seq.empty
        actual |> should equal expected 

    [<Test>]
    member x.``I can append two a empty lines`` () =
        let line1 : Line.Time.T<float> = Line.make Segment.makeContinuous []
        let line2 : Line.Time.T<float> = Line.make Segment.makeContinuous []
        let actual = Line.append (Some Segment.Time.interpolateValue) line1 line2
        let expected : Line.Time.T<float> = Line.make Segment.makeContinuous []
        actual |> should equal expected
         
    [<Test>]
    member x.``I can append an empty line to a line`` () =
        let points = [|0.0 .. 10.0 .. 100.0|] |> Helper.getPoints Helper.d0 10.0         
        let line1 : Line.Time.T<float> = Line.make Segment.makeContinuous []
        let line2 = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.append (Some Segment.Time.interpolateValue) line1 line2
        let expected = line2
        actual |> should equal expected 
         
    [<Test>]
    member x.``I can append a line to an empty line`` () =
        let points = [|0.0 .. 10.0 .. 100.0|] |> Helper.getPoints Helper.d0 10.0         
        let line1 = Line.make Segment.makeContinuous (Seq.pairwise points)
        let line2 : Line.Time.T<float> = Line.make Segment.makeContinuous []
        let actual = Line.append (Some Segment.Time.interpolateValue) line1 line2
        let expected = line1
        actual |> should equal expected 
         
    [<Test>]
    member x.``I can append two lines over the same range`` () =
        let line1 = 
            [|0.0 .. 10.0 .. 100.0|] |> Helper.getPoints Helper.d0 10.0
            |> Seq.pairwise |>  Line.make Segment.makeContinuous
        let line2 =
            [|100.0 .. 10.0 .. 200.0|] |> Helper.getPoints Helper.d0 10.0
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        let actual = Line.append (Some Segment.Time.interpolateValue) line1 line2
        let expected = line2
        actual |> should equal expected 
         
    [<Test>]
    member x.``I can append two lines where the line2 start is within line1 interval`` () =
        let line1 = 
            [|0.0 .. 10.0 .. 100.0|] |> Helper.getPoints Helper.d0 10.0
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        let line2 =
            [|100.0 .. 10.0 .. 200.0|] |> Helper.getPoints (Helper.d0.AddMinutes(50.0)) 10.0
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        let actual = Line.append (Some Segment.Time.interpolateValue) line1 line2
        let expected : Line.Time.T<_> =
            let points1 = [| 0.0; 10.0; 20.0; 30.0; 40.0; 50.0 |] |> Helper.getPoints Helper.d0 10.0
            let segments1 = points1 |> Seq.pairwise |> Seq.map Segment.makeContinuous
            let points2 = [|100.0 .. 10.0 .. 200.0|] |> Helper.getPoints (Helper.d0.AddMinutes(50.0)) 10.0
            let segments2 = points2 |> Seq.pairwise |> Seq.map Segment.makeContinuous
            Line.ofSegments (Seq.append segments1 segments2 |> Seq.toArray)
        (Line.segments actual) |> should equal (Line.segments expected)
         
    [<Test>]
    member x.``I can append two lines where the line2 start is at line1 end`` () =
        let line1 = 
            [|0.0 .. 10.0 .. 100.0|] |> Helper.getPoints Helper.d0 10.0
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        let line2 =
            [|100.0 .. 10.0 .. 200.0|] |> Helper.getPoints (Helper.d0.AddMinutes(100.0)) 10.0
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        let actual = Line.append (Some Segment.Time.interpolateValue) line1 line2
        let expected : Line.Time.T<_> =
            let points1 = [|0.0 .. 10.0 .. 100.0|] |> Helper.getPoints Helper.d0 10.0
            let segments1 = points1 |> Seq.pairwise |> Seq.map Segment.makeContinuous
            let points2 = [|100.0 .. 10.0 .. 200.0|] |> Helper.getPoints (Helper.d0.AddMinutes(100.0)) 10.0
            let segments2 = points2 |> Seq.pairwise |> Seq.map Segment.makeContinuous
            Line.ofSegments (Seq.append segments1 segments2 |> Seq.toArray)
        (Line.segments actual) |> should equal (Line.segments expected)
         
    [<Test>]
    member x.``I can append two lines where the line2 start is before line1 start`` () =
        let line1 = 
            [|0.0 .. 10.0 .. 100.0|] |> Helper.getPoints (Helper.d0.AddMinutes(10.0)) 10.0
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        let line2 =
            [|100.0 .. 10.0 .. 200.0|] |> Helper.getPoints Helper.d0 10.0
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        let actual = Line.append (Some Segment.Time.interpolateValue) line1 line2
        let expected = line2 
        (Line.segments actual) |> should equal (Line.segments expected)
         
    [<Test>]
    member x.``I can append two lines where the line2 start is within line1 interval within a segment`` () =
        let line1 = 
            [|0.0 .. 10.0 .. 100.0|] |> Helper.getPoints Helper.d0 10.0
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        let line2 =
            [|100.0 .. 10.0 .. 150.0|] |> Helper.getPoints (Helper.d0.AddMinutes(45.0)) 10.0
            |> Seq.pairwise |> Line.make Segment.makeContinuous
        let actual = Line.append (Some Segment.Time.interpolateValue) line1 line2
        let expected : Line.Time.T<_> =
            let points1 = 
                [| 0.0,0.0; 10.0,10.0; 20.0,20.0; 30.0,30.0; 40.0,40.0; 45.0,45.0 |] 
                |> Array.map (fun (t,v) -> Point.make(Helper.d0.AddMinutes(t),v))
            let segments1 = points1 |> Seq.pairwise |> Seq.map Segment.makeContinuous
            let points2 = 
                [| 45.0,100.0; 55.0,110.0; 65.0,120.0; 75.0,130.0; 85.0,140.0; 95.0,150.0 |] 
                |> Array.map (fun (t,v) -> Point.make(Helper.d0.AddMinutes(t),v))
            let segments2 = points2 |> Seq.pairwise |> Seq.map Segment.makeContinuous
            Line.ofSegments (Seq.append segments1 segments2 |> Seq.toArray)
        (Line.segments actual) |> should equal (Line.segments expected)

    [<Test>]
    member x.``I can convert a line into a sequence of points`` () =
        let points = [Helper.d0, Some 0.0; Helper.d0.AddMinutes(30.0), Some 100.; Helper.d0.AddMinutes(60.0), Some 200.] |> List.map Point.make |> List.toSeq     
        let line = Line.make Segment.makeContinuous (Seq.pairwise points)
        let actual = Line.toPoints line
        let expected = [Helper.d0, Some 0.0; Helper.d0.AddMinutes(30.0), Some 100.; Helper.d0.AddMinutes(60.0), Some 200.] |> List.map Point.make |> List.toSeq
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert an empty line into a sequence of points`` () =
        let line = Line.make Segment.makeContinuous []
        let actual = Line.toPoints line
        let expected = [] |> List.toSeq
        actual |> should equal expected

    [<Test>]
    member x.``I can detect contiguity`` () =
        let line = 
            Line.make Segment.makeContinuous (
                [(0.0,0.0),(10.0,10.0); (10.0,10.0),(20.0,10.0); (20.0,10.0),(30.0,20.0)] 
                |> Seq.map (fun (p1,p2) -> Point.make p1, Point.make p2))
        let actual = Line.isContiguous (=) line
        let expected = true
        actual |> should equal expected

    [<Test>]
    member x.``I can detect a discontiguity`` () =
        let line = 
            Line.make Segment.makeContinuous (
                [(0.0,0.0),(10.0,10.0); (20.0,10.0),(30.0,20.0)] 
                |> Seq.map (fun (p1,p2) -> Point.make p1, Point.make p2))
        let actual = Line.isContiguous (=) line
        let expected = false
        actual |> should equal expected


type ``Given an instantaneous TimeLine`` () =

    [<Test>]
    member x.``I can take a slice of the timeline with a single element`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0;  100.|]        
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.slice None (Helper.d0,Helper.d0.AddMinutes(30.)) line
        let expected = 
            [|
                Point.make (Helper.d0, 0.0)
                Point.make (Helper.d0.AddMinutes(30.), 100.0)
            |] |> Line.make Segment.makeInstantaneous
        actual |> should equal expected 

    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval ends in the middle of a segement but spanning two segments``() =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.; 200.; 250.|] 
        let line = Line.make Segment.makeInstantaneous points
        let interval = Interval.make (Helper.d0.AddMinutes(15.),Helper.d0.AddMinutes(45.))
        let actual = Line.slice None interval line
        let expected = 
            [|
                Point.make (Helper.d0.AddMinutes(30.), 100.0)
            |] |> Line.make Segment.makeInstantaneous
        actual |> should equal expected 

    [<Test>]
    member x.``I can take a slice of a TimeLine when a single segment wraps the interal completely``() =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0;  100.;|] 
        let line = Line.make Segment.makeInstantaneous points
        let interval = Interval.make (Helper.d0.AddMinutes(10.),Helper.d0.AddMinutes(15.))
        let actual = Line.slice None interval line
        let expected : Line.Time.T<float> = Line.make Segment.makeInstantaneous []
        actual |> should equal expected 

    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval is fully within the timeline``() =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.; 200.; 250.|] 
        let line = Line.make Segment.makeInstantaneous points
        let interval = Interval.make (Helper.d0.AddMinutes(30.),Helper.d0.AddHours(1.))
        let actual = Line.slice None interval line
        let expected = 
            [|
                Point.make (Helper.d0.AddMinutes(30.), 100.)
                Point.make(Helper.d0.AddMinutes(60.), 200.)
            |] |> Line.make Segment.makeInstantaneous
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value before the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [|0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(-1.0)) line
        let expected = None
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the start of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [|0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.Time.tryFindValue None Helper.d0 line
        let expected = Some 0.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(60.0)) line
        let expected = Some 200.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value after the end of the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(61.0)) line
        let expected = None
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the start of a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(30.0)) line
        let expected = Some 100.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value within a segment within the line`` () =
        let points = Helper.getPoints Helper.d0 30.0 [|0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(45.0)) line
        let expected = None
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert a line to a seq`` () =
        let points = Helper.getPoints Helper.d0 30.0 [| 0.0; 30.0; 60.0|]        
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.Time.toSeq IntervalType.T.Closed None (TimeSpan.FromMinutes(1.0)) line |> Seq.toArray
        let expected = 
            seq { 
                yield Some 0.0
                yield! Seq.init 29 (fun i -> None)
                yield Some 30.0 
                yield! Seq.init 29 (fun i -> None) 
                yield Some 60.0 
            } |> Seq.toArray
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert an empty line to a seq`` () =
        let line : Line.Time.T<float> = Line.make Segment.makeInstantaneous []
        let actual = Line.Time.toSeq IntervalType.T.Closed None (TimeSpan.FromMinutes(1.0)) line
        let expected = Seq.empty
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert a line into a sequence of points`` () =
        let points = [Helper.d0, 0.0; Helper.d0.AddMinutes(30.0), 100.; Helper.d0.AddMinutes(60.0), 200.] |> List.map Point.make |> List.toSeq     
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.toPoints line
        let expected = [Helper.d0, 0.0; Helper.d0.AddMinutes(30.0), 100.; Helper.d0.AddMinutes(60.0), 200.] |> List.map Point.make |> List.toSeq
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert an empty line into a sequence of points`` () =
        let line = Line.make Segment.makeInstantaneous []
        let actual = Line.toPoints line
        let expected = [] |> List.toSeq
        actual |> should equal expected

    [<Test>]
    member x.``I can sum the segments in a line by some function`` () =
        let points = Helper.getPoints Helper.d0 30.0 [|0.0; 30.0; 60.0|]        
        let line = Line.make Segment.makeInstantaneous points
        let actual = Line.sumBy Segment.startY line
        let expected = 90.0
        actual |> should equal expected


type ``Given a Discrete TimeLine`` () =

    [<Test>]
    member x.``I can take a slice of the timeline with a single element`` () =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftClosedRightOpen Helper.d0 30.0 [|0.0; 100.|]        
        let line = Line.make Segment.makeDiscrete points
        let actual = Line.slice None (Helper.d0,Helper.d0.AddMinutes(30.)) line
        let expected = 
            [|
                Segment.makeDiscrete (IntervalType.T.LeftClosedRightOpen, Interval.make(Helper.d0, Helper.d0.AddMinutes(30.)), 0.0)
            |] |> Line.ofSegments
        actual |> should equal expected 

    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval ends in the middle of a segement but spanning two segments``() =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftClosedRightOpen Helper.d0 30.0 [|0.0; 100.; 200.; 250.|] 
        let line = Line.make Segment.makeDiscrete points
        let interval = Interval.make (Helper.d0.AddMinutes(15.),Helper.d0.AddMinutes(45.))
        let actual = Line.slice None interval line
        let expected = 
            [|
                Segment.makeDiscrete (IntervalType.T.LeftClosedRightOpen, Interval.make(Helper.d0.AddMinutes(15.), Helper.d0.AddMinutes(30.)), 0.0)
                Segment.makeDiscrete (IntervalType.T.LeftClosedRightOpen, Interval.make(Helper.d0.AddMinutes(30.), Helper.d0.AddMinutes(45.)), 100.0)
            |]|> Line.ofSegments
        actual |> should equal expected 

    [<Test>]
    member x.``I can take a slice of a TimeLine when a single segment wraps the interal completely``() =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftClosedRightOpen Helper.d0 30.0 [|0.0; 100.|] 
        let line = Line.make Segment.makeDiscrete points
        let interval = Interval.make (Helper.d0.AddMinutes(10.),Helper.d0.AddMinutes(15.))
        let actual = Line.slice None interval line
        let expected = 
            [|
                Segment.makeDiscrete (IntervalType.T.LeftClosedRightOpen, Interval.make(Helper.d0.AddMinutes(10.), Helper.d0.AddMinutes(15.)), 0.0)
            |] |> Line.ofSegments
        actual |> should equal expected 

    [<Test>]
    member x.``I can take a slice of a TimeLine when the interval is fully within the timeline``() =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftClosedRightOpen Helper.d0 30.0 [|0.0; 100.; 200.; 250.|] 
        let line = Line.make Segment.makeDiscrete points
        let interval = Interval.make (Helper.d0.AddMinutes(30.),Helper.d0.AddHours(1.))
        let actual = Line.slice None interval line
        let expected = 
            [|
                Segment.makeDiscrete (IntervalType.T.LeftClosedRightOpen, Interval.make(Helper.d0.AddMinutes(30.), Helper.d0.AddMinutes(60.)), 100.0)
            |] |> Line.ofSegments
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value before the start of the line`` () =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftClosedRightOpen Helper.d0 30.0 [|0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeDiscrete points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(-1.0)) line
        let expected = None
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the start of the line`` () =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftClosedRightOpen Helper.d0 30.0 [|0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeDiscrete points
        let actual = Line.Time.tryFindValue None (Helper.d0) line
        let expected = Some 0.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the end of the line`` () =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftOpenRightClosed Helper.d0 30.0 [| 0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeDiscrete points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(60.0)) line
        let expected = Some 100.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value after the end of the line`` () =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftOpenRightClosed Helper.d0 30.0 [|0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeDiscrete points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(91.0)) line
        let expected = None
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value at the start of a segment within the line`` () =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftClosedRightOpen Helper.d0 30.0 [|0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeDiscrete points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(30.0)) line
        let expected = Some 100.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can try find a value within a segment within the line`` () =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftClosedRightOpen Helper.d0 30.0 [|0.0; 100.0; 200.0|]        
        let line = Line.make Segment.makeDiscrete points
        let actual = Line.Time.tryFindValue None (Helper.d0.AddMinutes(45.0))line
        let expected = Some 100.0
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert a line to a seq`` () =
        let points = Helper.getPointsDiscrete IntervalType.T.LeftClosedRightOpen Helper.d0 30.0 [|0.0; 30.0; 60.0|]        
        let line = Line.make Segment.makeDiscrete points
        let actual = Line.Time.toSeq IntervalType.T.LeftClosedRightOpen None (TimeSpan.FromMinutes(1.0)) line |> Seq.toArray
        let expected = 
            seq {
                yield! Seq.init 30 (fun i -> Some 0.0); 
                yield! Seq.init 30 (fun i -> Some 30.0);
                yield! Seq.init 30 (fun i -> Some 60.0);
            } |> Seq.toArray
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert an empty line to a seq`` () =
        let line : Line.Time.T<float> = Line.make Segment.makeDiscrete []
        let actual = Line.Time.toSeq IntervalType.T.LeftOpenRightClosed None (TimeSpan.FromMinutes(1.0)) line
        let expected = Seq.empty
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert a line into a sequence of points`` () =
        let points = [
                         Segment.makeDiscrete (IntervalType.T.LeftClosedRightOpen, Interval.make(Helper.d0, Helper.d0.AddMinutes(30.0)), 0.)
                         Segment.makeDiscrete (IntervalType.T.LeftClosedRightOpen, Interval.make(Helper.d0.AddMinutes(30.0), Helper.d0.AddMinutes(60.0)), 100.)
                     ]   
        let line = Line.ofSegments points
        let actual = Line.toPoints line
        let expected = [Helper.d0, 0.0; Helper.d0.AddMinutes(30.0), 100.; Helper.d0.AddMinutes(60.0), 100.] |> List.map Point.make |> List.toSeq
        actual |> should equal expected 

    [<Test>]
    member x.``I can convert an empty line into a sequence of points`` () =
        let line = Line.make Segment.makeDiscrete []
        let actual = Line.toPoints line
        let expected = [] |> List.toSeq
        actual |> should equal expected 
        
        