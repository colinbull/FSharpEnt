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


type Location = {
    Exchange : string
    ExchangeType : string
    Queue : string option
    RoutingKey : string
    Serialiser : ISerialiser<byte[]>
}
with 
    static member For<'a>(?exchangeType, ?routingKey, ?queue, ?serialiser) = 
           { Exchange = (typeof<'a>).FullName; 
             ExchangeType = defaultArg exchangeType "Topic"; 
             Queue = queue; 
             RoutingKey = defaultArg routingKey String.Empty 
             Serialiser = defaultArg serialiser Json.ByteSerialiser
           }

type Listener<'a> = {
    Cancel : unit -> unit
    Observer : IEvent<'a>
}

module private Core =
    
    let getConnectionString() : string * string * string = 
        match Environment.connectionStrings.Value.TryFind("RabbitMq") with
        | Some(connStr) -> connStr
        | None -> "Host=tcp://localhost:5672;Username=guest;Password=guest"
        |> (fun (s : string) -> s.Split([|';'|], StringSplitOptions.RemoveEmptyEntries))
        |> Seq.map (fun x ->
                        match x.Split([|'='|], StringSplitOptions.None) |> Seq.toList with
                        | h :: t :: [] -> t
                        | _ -> failwith "Invalid connection string, expected string of type Host=tcp://localhost:5672;Username=guest;Password=guest"
                    )
        |> Seq.toArray
        |> function
           | [|uri;username;password|] -> uri,username,password
           | _ -> failwith "Failed to parse connection string"

    let connection = 
        let (host,username,password) = getConnectionString()
        let uri = new System.Uri(host)
        let connectionFactory = new ConnectionFactory()
        connectionFactory.HostName <- uri.Host
        connectionFactory.UserName <- username
        connectionFactory.Password <- password
        connectionFactory.Port <- uri.Port
        connectionFactory.Protocol <- Protocols.DefaultProtocol
        connectionFactory.CreateConnection()

    let createModel() = 
        let model = connection.CreateModel() 
        connection.AutoClose <- true
        model
    
    let declareExchange (loc : Location) (model : IModel) =
        model.ExchangeDeclare(loc.Exchange, (loc.ExchangeType.ToLower()))
    
    let createSubscription (loc : Location) (model : IModel) = 
        declareExchange loc model
        let queueName = 
            match loc.Queue with
            | Some(q) -> model.QueueDeclare(q,false,false,true,null).QueueName
            | None -> model.QueueDeclare().QueueName
        model.QueueBind(queueName, loc.Exchange, loc.RoutingKey)
        new Subscription(model, queueName)

