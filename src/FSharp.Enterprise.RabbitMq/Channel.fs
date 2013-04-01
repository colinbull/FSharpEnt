namespace FSharp.Enterprise.RabbitMq

open System
open System.Configuration
open System.Text
open System.Threading
open FSharp.Enterprise.RabbitMq
open FSharp.Enterprise.Channel
open Newtonsoft.Json
open Microsoft.FSharp.Control
open FSharp.Enterprise.Serialisation

module Channel =
    
    type RabbitMqMsg<'a> = 
        | Msg of string * 'a
     
    type RabbitMqChannel<'request>(id, ?reader, ?writer, ?timeout) =
        
        let reader = defaultArg reader Json.ofByteArray
        let writer = defaultArg writer Json.toByteArray

        let client = new RpcClient(id, reader, writer, ?timeout = timeout)

        interface IChannel<RabbitMqMsg<'request>> with
            member x.Name with get() = id
            
            member x.Post(Msg(_,msg)) = 
                PubSub.publish (Location<'request>.For(writer = writer)) msg

            member x.PostAndAsyncReply(builder) = 
                async {
                    let address,msg = builder(new NullReplyChannel<_>()) |> function Msg(a,p) -> a,p
                    let! result = client.PostAndAsyncReply(address, msg)
                    match result with
                    | Choice1Of2(r) -> return r
                    | Choice2Of2(e) -> return raise e
                }
            
            member x.PostAndTryAsyncReply(builder) = 
                 let address,msg = builder(new NullReplyChannel<_>()) |> function Msg(a,p) -> a,p
                 client.PostAndTryAsyncReply(address, msg)

            member x.PostAndReply(builder) = 
                async {
                    let address,msg = builder(new NullReplyChannel<_>()) |> function Msg(a,p) -> a,p
                    let! result = client.PostAndAsyncReply(address, msg)
                    match result with
                    | Choice1Of2(r) -> return r
                    | Choice2Of2(e) -> return raise e
                } |> Async.RunSynchronously
