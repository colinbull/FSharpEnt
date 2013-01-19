namespace FSharp.Enterprise.RabbitMq

open System
open System.Configuration
open System.Text
open System.Threading
open Newtonsoft.Json
open RabbitMQ.Client
open RabbitMQ.Client.Framing.v0_9_1
open RabbitMQ.Client.Events
open RabbitMQ.Client.MessagePatterns
open FSharp.Enterprise
open FSharp.Enterprise.Serialisation
open Core

type RpcClient(serverId : string, ?serialiser, ?timeout) =
     
     let clientCache = ref Map.empty<string,SimpleRpcClient>
     let error = new Event<exn>()
     let serialiser : ISerialiser<byte[]> = defaultArg serialiser Json.ByteSerialiser

     let client (serverId :string) =
          match (!clientCache).TryFind serverId with
          | Some(client) -> client
          | None ->
              let client = new SimpleRpcClient(createModel(), serverId)
              client.TimeoutMilliseconds <- (defaultArg timeout Timeout.Infinite)
              client.Disconnected |> Event.add (fun _ -> clientCache := (!clientCache).Remove(serverId))
              client.TimedOut |> Event.add (fun _ -> clientCache := (!clientCache).Remove(serverId))
              clientCache := (!clientCache).Add(serverId, client)
              client
    
     [<CLIEvent>]
     member x.Error = error.Publish

     member x.PostAndTryAsyncReply(address : string, request : 'request) = 
          async {
              try
                  let requestProps = new BasicProperties()
                  let client = client address
                  let result = client.Call(requestProps, serialiser.Serialise request)
                  return 
                      if result.BasicProperties.CorrelationId = requestProps.CorrelationId
                      then result.Body |> serialiser.Deserialise |> Some
                      else None
              with e ->
                  error.Trigger(e)
                  return None
          }
      
      member x.PostAndTryReply(address, request : 'request) = 
          x.PostAndTryAsyncReply(address, request) |> Async.RunSynchronously

      member x.PostAndAsyncReply(address, request : 'request) : Async<Choice<'response, exn>> = 
          async {
              try
                  let requestProps = new BasicProperties()
                  let client = client address
                  let result = client.Call(requestProps, serialiser.Serialise request)
                  return 
                      if result.BasicProperties.CorrelationId = requestProps.CorrelationId
                      then Choice1Of2(result.Body |> serialiser.Deserialise)
                      else Choice2Of2(Exception("An unexpected correlation id was received"))
              with e ->
                  error.Trigger(e)
                  return Choice2Of2(e)
          }

      member x.PostAndReply(address, request: 'request) = 
          match x.PostAndTryReply(address, request) with
          | Some(response) -> response
          | None -> failwith "Expected response, None received"
      
      member x.Post(address, request : 'request) =
          let requestProps = new BasicProperties()
          let client = client address
          client.Cast(requestProps, serialiser.Serialise request)

type RpcServer<'request, 'response>(serverId : string, onRequest : 'request -> 'response, ?serialiser) = 
      inherit SimpleRpcServer(createSubscription (Location.For<'request>(exchangeType = "direct", queue = serverId)) (createModel()))

      let serialiser : ISerialiser<byte[]> = defaultArg serialiser Json.ByteSerialiser
      let error = new Event<exn>()

      override x.HandleCall(isRedelivered, requestProperties, body, replyProperties) =
              replyProperties <- requestProperties
              replyProperties.MessageId <- Guid.NewGuid().ToString()
              serialiser.Deserialise body
              |> onRequest
              |> serialiser.Serialise

      override x.HandleCast(isRedelivered, requestProperties, body) =
              serialiser.Deserialise body
              |> onRequest
              |> ignore
      

      [<CLIEvent>]
      member x.Error = error.Publish

      member x.Start() = 
          x.MainLoop()

      member x.StartAsync(token) = 
          Async.Start(async {
              do x.MainLoop()
          }, token)
    

        
