namespace FSharp.Enterprise.Tests

open System
open FsUnit
open NUnit.Framework
open FSharp.Enterprise.Channel

type EchoMessage =
    | Msg of string

type EchoChannel(name) = 
    interface IChannel<string> with
        member x.Name = name
        member x.Post(msg) = ()
        member x.PostAndAsyncReply(builder) = 
            async { let reply = builder(new NullReplyChannel<_>())
                    return reply |> box |> unbox<'a>}
        member x.PostAndReply(builder) =
            let reply = builder(new NullReplyChannel<_>())
            reply |> box |> unbox<'a>
        member x.PostAndTryAsyncReply(builder) =
            async { let reply = builder(new NullReplyChannel<_>()); 
                    return Some(reply |> box |> unbox<'a>) }

[<TestFixture>]
type ``With default channel resolver``() =

    [<Test>]
    member t.``i can register and resolve channel``() = 
        let resolver = new DefaultChannelResolver() :> IChannelResolver
        let channel = new EchoChannel("echo")
        resolver.Register channel
        resolver.Resolve<string>("echo") |> should equal channel

    [<Test>]
    member t.``i can try and resolve a channel``() = 
        let resolver = new DefaultChannelResolver() :> IChannelResolver
        resolver.TryResolve<string>("echo") |> should equal None

[<TestFixture>]
type ``With channel operators``() =
    
    [<SetUp>]
    member t.Setup() =
        setChannelResolver(new DefaultChannelResolver([new EchoChannel("echo")]))

    [<Test>]
    member t.``i can post to the channel``() =
        let channel = new EchoChannel("echo")
        Assert.DoesNotThrow(fun () -> channel <-- "Foo")

    [<Test>]
    member t.``i can resolve and post to the resolved channel``() =
        Assert.DoesNotThrow(fun () -> "echo" ?<-- "Foo")

    [<Test>]
    [<ExpectedException(ExpectedMessage = "Unable to resolve channel (Foo)", ExpectedException = typeof<Exception>)>]
    member t.``throws when cannot resolve channel``() =
        ("Foo" ?<-- "Foo")

    [<Test>]
    member t.``i can get async reply``() =
        let channel = new EchoChannel("echo")
        let result : string = channel <--> (fun rc -> "Foo") |> Async.RunSynchronously
        result |> should equal "Foo" 

    [<Test>]
    member t.``i can try get async reply``() =
        let channel = new EchoChannel("echo")
        let result : string option = channel  <-?> (fun rc -> "Foo") |> Async.RunSynchronously
        result |> should equal (Some "Foo") 

    [<Test>]
    member t.``i can get a reply``() =
        let channel = new EchoChannel("echo")
        let result : string = channel <-!> (fun rc -> "Foo")
        result |> should equal "Foo"

    [<Test>]
    member t.``i can resolve and get async reply``() =
        let result : string = "echo" ?<--> (fun rc -> "Foo") |> Async.RunSynchronously
        result |> should equal "Foo" 

    [<Test>]
    member t.``i can resolve and try get async reply``() =
        let result : string option = "echo" ?<-?> (fun rc -> "Foo") |> Async.RunSynchronously
        result |> should equal (Some "Foo") 

    [<Test>]
    member t.``i can resolve and get reply``() =
        let result : string = "echo" ?<-!> (fun rc -> "Foo")
        result |> should equal "Foo" 
