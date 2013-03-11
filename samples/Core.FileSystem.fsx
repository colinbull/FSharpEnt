(**
# F# Enterprise - File System

The file system module provides a simple set of helpers over common file system operations like 
IO and Searching. Additionally the abstractions aim to making testing of these IO bound operations easier as you 
should not have to create **Fakes** or actual file system directories to test applications that use these abstractions. 
*)

#r @"..\bin\FSharp.Enterprise.dll"

open System
open System.IO
open FSharp.Enterprise

(**
##Searching for files

Traditionally searching for files in .NET required some does not support recursive file patterns. `FileSystem.Search` adds this capability
*)

let searchPath = __SOURCE_DIRECTORY__ + "/**/*.C*.fsx"
let filesFound = 
    FileSystem.Search.findFiles None searchPath
    |> Seq.toArray

(**
The when ran the above code returns a collection something like the following

    val filesFound : string [] =
      [|"D:\Appdev\FSharp.Enterprise\samples\Core.Caching.fsx";
        "D:\Appdev\FSharp.Enterprise\samples\Core.Channel.fsx";
        "D:\Appdev\FSharp.Enterprise\samples\RabbitMq.Channel.fsx";
        "D:\Appdev\FSharp.Enterprise\samples\Web.Channel.fsx"|]

`FileSystem.Search.findFiles` optionally takes a _searcher_ function. This allows you to override the searching behaviour. 
This is useful for testing.
*)

let mockedSearcher (p:FileSystem.Search.T) = 
    [
      "TestA"
      "TestB"
    ] |> Seq.map (fun name -> p.Root + "\\" + name + Path.GetExtension(p.FilePattern)) 

let mockedFilesFound = 
    FileSystem.Search.findFiles (Some mockedSearcher) searchPath
    |> Seq.toArray

(**
Which now gives the following results:

    val mockedFilesFound : string [] =
      [|"D:\Appdev\FSharp.Enterprise\samples\TestA.fsx";
        "D:\Appdev\FSharp.Enterprise\samples\TestB.fsx"|]

##Reading and Writing Files

`FSharp.Enterprise` supplies two implementations of the `IIO` interface within the FileSystem module `FileIO` and `CachedFileIO` 
(see the documentation [here for more info](Core.IO.html)).
In the following example we will use both implementations to repeat 5000 times writing `A` to a file, with the string accumlating in length. 
And then immediately reading it back.
*)

#time
let NonCachedIO = FileSystem.FileIO Serialisation.Raw.StringSerialiser
let CachedIO = FileSystem.CachedFileIO Serialisation.Raw.StringSerialiser (TimeSpan.FromMilliseconds(200.))

let writeString (io : IO.IIO) (str:string) = io.Write("TestA.txt",str)
let readString (io : IO.IIO) = io.Read("TestA.txt")

let execute io = 
    [1..5000] 
    |> List.iter (fun i -> 
                    writeString io (String.replicate i "A")
                    readString io |> ignore
                  )

let nonCached = execute NonCachedIO
let cached = execute CachedIO

(**
The results for the above code are as follows...

    Real: 00:00:04.242, CPU: 00:00:04.250, GC gen0: 51, gen1: 51, gen2: 1
    val nonCached : unit = ()
    
    Real: 00:00:00.072, CPU: 00:00:00.078, GC gen0: 18, gen1: 18, gen2: 0
    val cached : unit = ()

Of course the actual values will vary depending on the machine.
*)