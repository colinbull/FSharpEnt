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

type RpcClient(serverId : string, ?reader : (byte[] -> obj), ?writer : (obj -> byte[]), ?timeout) =
     
     let clientCache = ref Map.empty<string,SimpleRpcClient>
     let error = new Event<exn>()
     let reader = defaultArg reader Json.ofByteArray
     let writer = defaultArg writer Json.toByteArray

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

     member x.PostAndTryAsyncReply<'request, 'response>(address : string, request : 'request) : Async<'response option> = 
          async {
              try
                  let requestProps = new BasicProperties()
                  let client = client address
                  let result = client.Call(requestProps, writer (box request))
                  return 
                      if result.BasicProperties.CorrelationId = requestProps.CorrelationId
                      then result.Body |> reader |> unbox<'response> |> Some
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
                  let result = client.Call(requestProps, writer request)
                  return 
                      if result.BasicProperties.CorrelationId = requestProps.CorrelationId
                      then Choice1Of2(result.Body |> reader |> unbox<_>)
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
          client.Cast(requestProps, writer request)

type RpcServer<'request, 'response>(serverId : string, onRequest : 'request -> 'response, ?writer, ?reader) = 
      inherit SimpleRpcServer(createSubscription (Location<'request>.For(exchangeType = "direct", queue = serverId)) (createModel()))

      let reader = defaultArg reader Json.ofByteArray
      let writer = defaultArg writer Json.toByteArray
      let error = new Event<exn>()

      override x.HandleCall(isRedelivered, requestProperties, body, replyProperties) =
              replyProperties <- requestProperties
              replyProperties.MessageId <- Guid.NewGuid().ToString()
              reader body
              |> onRequest
              |> writer

      override x.HandleCast(isRedelivered, requestProperties, body) =
              reader body
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
    

        
