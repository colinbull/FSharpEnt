namespace FSharp.Enterprise

open System
open System.Threading
open Microsoft.FSharp.Control
open FSharp.Enterprise.Channel

type Agent<'a> = MailboxProcessor<'a>

type IAgent =
     inherit IDisposable 
     abstract Id : string with get, set

type IAgent<'msg> =
    inherit IAgent
    inherit IChannel<'msg>
    abstract Scan: ('msg -> Async<'a> option) -> Async<'a>
    abstract Receive: unit -> Async<'msg>
    abstract TryReceive : int option -> Async<'msg option>
    abstract QueueLength : int with get
    abstract Error : IEvent<Handler<Exception>, Exception>

type MailboxReplyChannel<'msg>(rc : AsyncReplyChannel<Reply<'msg>>) =
    interface IReplyChannel<'msg> with
        member x.Reply(reply : Reply<'msg>) = rc.Reply(reply)
        
type AgentRef<'msg>(id : string, body : (IAgent<'msg> -> Async<unit>)) as self =

    let cts = (new CancellationTokenSource())
    let mutable id = id
    let agent : Agent<'msg> = Agent<'msg>.Start((fun inbox -> body(self)), cts.Token)
        
    interface IAgent<'msg> with
        member x.Id with get() = id and set(v) = id <- v
        member x.Receive() = agent.Receive()
        member x.Scan(scanner) = agent.Scan(scanner)
        member x.TryReceive(timeout) = agent.TryReceive(defaultArg timeout 0)
        member x.QueueLength with get() = agent.CurrentQueueLength
        member x.Error = agent.Error

    interface IChannel<'msg> with
        member x.Name = (x :> IAgent<'msg>).Id
        member x.Post(msg) = agent.Post(msg)
        member x.PostAndReply(builder) = 
            match agent.PostAndReply<Reply<'a>>(fun rc -> builder(new MailboxReplyChannel<_>(rc))) with
            | Value(r) -> r
            | Exception(e) -> raise e
        member x.PostAndAsyncReply(builder) = 
            async {
                let! reply = agent.PostAndAsyncReply<Reply<'a>>(fun rc -> builder(new MailboxReplyChannel<_>(rc)))
                return 
                    match reply with
                    | Value(r) -> r
                    | Exception(e) -> raise e
            }
        member x.PostAndTryAsyncReply(builder) =
            async {
                let! reply = agent.PostAndTryAsyncReply<Reply<'a>>(fun rc -> builder(new MailboxReplyChannel<_>(rc)))
                return 
                    Option.bind (function
                                 | Value(r) -> Some r
                                 | Exception(e) -> None) reply
            }
        

    interface IDisposable with
        member x.Dispose() =
            cts.Cancel()
            cts.Dispose()

[<AutoOpen>]
module Agent = 
 
    let bindNamed name agent = 
        new AgentRef<_>(name, agent) :> IAgent<_>
    
    let bind agent =
        bindNamed (Guid.NewGuid().ToString()) agent
       
    let named (id : string) (agent : IAgent<'msg>) =
        agent.Id <- id
        agent
    
    let toChannel (agent : IAgent<'a>) = 
        agent :> IChannel<'a>

    let receive (agentBody : 'a -> Async<unit>) = 
        let rec body (agent: IAgent<'a>) = async { 
                let! msg = agent.Receive()
                do! agentBody msg
                return! body agent
            }
        body

    let tryReceive timeout (agentBody : 'a option -> Async<unit>) = 
        let rec body (agent: IAgent<'a>) = async { 
                let! msg = agent.TryReceive(timeout)
                do! agentBody msg
                return! body agent
            }
        body

    let scan scanner (agentBody : 'msg -> Async<unit>) = 
        let rec body (agent: IAgent<'msg>) = async { 
                let! msg = agent.Scan(scanner)
                do! agentBody msg
                return! body agent
            }
        body

    let roundRobinDispatch (agents : seq<IAgent<'msg>>) =
        let rec body (agents : (int * IAgent<'msg>) list) (agent : IAgent<'msg>) =
            async {
                let! msg = agent.Receive()
                match agents with
                | (i,h) :: t ->
                    do h <-- msg
                    return! body (t @ [i,h]) agent
                | [] -> failwith "Cannot round robin dispatch on an empty sequence of agents" 
            }
        bind (body (agents |> Seq.mapi (fun i x-> (i,x)) |> Seq.toList))     
    
    let shortestQueueDispatch (agents : seq<IAgent<'msg>>) =
        let rec body (agents : seq<IAgent<'msg>>) (agent : IAgent<'msg>) =
            async {
                let! msg = agent.Receive()
                (agents |> Seq.minBy (fun x -> x.QueueLength)) <-- msg
            }
        bind (body agents)     

    let onError (handler : IAgent<_> -> exn -> unit) (agent : IAgent<'msg>) =
        agent.Error |> Event.add (handler agent)
        agent
    
    let receiver (agentBody : 'a -> Async<unit>) = 
        bind (receive agentBody)

    let tryReciever timeout (agentBody : 'a option -> Async<unit>) =
        bind (tryReceive timeout agentBody)

    let scanner scanner (agentBody : 'msg -> Async<unit>) =
        bind (scan scanner agentBody)
    
    let spawnMany count errorF f = 
        Seq.init count (fun i -> receiver f |> onError errorF)

    let map (f : 'a -> 'b) (agent : IAgent<'b>) = 
        bind (receive (fun m -> async { agent <-- (f m) })) 

    let filter (f : 'a -> bool) (agent : IAgent<'a>) =
        bind (receive (fun (m : 'a) -> async { if f m then agent <-- m }))

    let intercept (f : 'msg -> Async<unit>) (agent : IAgent<'msg>) = 
        bind (receive (fun (m : 'msg) -> async { 
                  do! f m
                  do agent <-- m
              }))

    let link (agentBody : 'msg -> Async<'msg>) (linked : seq<IAgent<'msg>>) = 
        let rec body (linked : seq<IAgent<'msg>>) (agent : IAgent<'msg>)  = async {
                     let! msg = agent.Receive()
                     let! result = agentBody(msg)
                     (linked |> Seq.cast<IChannel<'msg>>) <-* result
                     return! body linked agent
                }
        bind (body linked)

    let broadcast (recipients : seq<IAgent<'msg>>) =
        link (fun m -> async { return m }) recipients
    
    let router (routeF : 'msg -> seq<IAgent<'msg>> option) =
        bind (receive (fun m ->
                          async {
                            match routeF m with
                            | Some(agents) -> (agents |> Seq.cast<IChannel<'msg>>) <-* m
                            | None -> ()
                          }
                      )
             )
    
    let splitter (splitterF : 'msg -> (seq<IAgent<'a>> * 'a) option) =
          bind (receive (fun m ->
                          async {
                            match splitterF m with
                            | Some(agents, msg) -> (agents |> Seq.cast<IChannel<'a>>) <-* msg
                            | None -> ()
                          }
                      )
             )
    
    let aggregate (replyChannel : 'a -> IReplyChannel<'b> -> 'a) 
                  (gatherF : seq<Choice<'b option,exn>> -> 'b) 
                  (agents : seq<IAgent<'a>>) 
                  (agent : IAgent<'b>) =
        bind (receive (fun m -> 
                        async {
                            let! results = 
                                seq {
                                    for agent in agents do
                                        yield async {
                                            try
                                                let! reply = agent <-?> (replyChannel m)
                                                return Choice1Of2(reply)
                                            with e ->
                                                return Choice2Of2(e)
                                        }
                                } |> Async.Parallel
                            agent  <-- gatherF(results)
                        }
                      )
             )

