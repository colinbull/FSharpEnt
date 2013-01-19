namespace FSharp.Enterprise.RabbitMq

open System
open System.Configuration
open System.Text
open System.Threading
open FSharp.Enterprise.RabbitMq
open FSharp.Enterprise.Channel
open Newtonsoft.Json
open Microsoft.FSharp.Control

module Channel =
    
    type RabbitMqMsg<'a> = 
        | Msg of string * 'a
     
    type RabbitMqChannel<'msg>(id,?serialiser, ?timeout) =
        
        let client = new RpcClient(id, ?serialiser = serialiser, ?timeout = timeout)

        interface IChannel<RabbitMqMsg<'msg>> with
            member x.Name with get() = id
            
            member x.Post(msg) = PubSub.publish (Location.For<_>(?serialiser = serialiser)) msg

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
