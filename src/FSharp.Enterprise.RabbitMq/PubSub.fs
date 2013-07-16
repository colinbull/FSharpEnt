namespace FSharp.Enterprise.RabbitMq

open System
open System.Configuration
open System.Text
open System.Threading
open FSharp.Enterprise
open RabbitMQ.Client
open RabbitMQ.Client.Framing.v0_9_1
open RabbitMQ.Client.Events
open RabbitMQ.Client.MessagePatterns
open Core

module PubSub =
    
    let tryGetAsync (loc : Location<_>) = 
        async {
            use model = createModel()
            let msg = model.BasicGet(defaultArg loc.Queue "", true)
            return 
                match msg with
                | null -> None
                | a -> Some(loc.Reader a.Body)
        }
    
    let tryGet loc = tryGetAsync loc |> Async.RunSynchronously

    let subscribe (loc : Location<_>) =
        let cts = new CancellationTokenSource()
        let evnt = new Event<byte[]>()
        let observerThread =
                async {
                    use model = createModel()
                    let subscription = createSubscription loc model
                    try
                        while not cts.IsCancellationRequested do
                            let (success, args) = subscription.Next(500)
                            if success then evnt.Trigger(args.Body)
                    finally
                        if subscription <> null then subscription.Close() 
                 }
        Async.Start(observerThread, cts.Token)
        { Observer = evnt.Publish |> Event.map loc.Reader; Cancel = (fun () -> cts.Cancel()) }
    
    let publish (loc : Location<_>) (payload : 'a) = 
        use model = createModel()
        declareExchange loc model
        let props = model.CreateBasicProperties()
        let body = loc.Writer payload
        model.BasicPublish(loc.Exchange, loc.RoutingKey, props, body)