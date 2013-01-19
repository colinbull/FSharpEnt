namespace FSharp.Enterprise

module Channel =

    type Reply<'a> =
        | Value of 'a
        | Exception of exn
    
    type IReplyChannel<'a> =
        abstract Reply : Reply<'a> -> unit
    
    type NullReplyChannel<'a>() =
        interface IReplyChannel<'a> with
            member x.Reply(reply) = ()

    type IChannel =
        abstract Name : string with get
    
    type IChannel<'a> =
        inherit IChannel
        abstract Post : 'a -> unit
        abstract PostAndAsyncReply<'b> : (IReplyChannel<'b> -> 'a) -> Async<'b>
        abstract PostAndReply<'b> :  (IReplyChannel<'b> -> 'a) -> 'b
        abstract PostAndTryAsyncReply<'b> : (IReplyChannel<'b> -> 'a) -> Async<'b option>

    type IChannelResolver = 
        abstract TryResolve : string -> IChannel<'msg> option
        abstract Resolve : string -> IChannel<'msg>
        abstract Resolve : unit -> seq<IChannel<'msg>>
        abstract Register : IChannel<'msg> -> unit

    type DefaultChannelResolver(?channels) =
       
        let channels = 
            (defaultArg channels Seq.empty<IChannel>)
            |> Seq.map (fun x -> x.Name, x) 
            |> Map.ofSeq
            |> ref

        interface IChannelResolver with
            
            member x.Register(channel) = 
                channels := (!channels).Add(channel.Name, channel)

            member x.Resolve<'msg>() =
                System.Linq.Enumerable.OfType<IChannel<'msg>>(!channels |> Map.toSeq |> Seq.map snd)

            member x.TryResolve<'msg>(id) = 
                match Map.tryFind id !channels with
                | Some(c) -> (c :?> IChannel<'msg>) |> Some
                | None -> None

            member x.Resolve<'msg>(id) = 
                match (x :> IChannelResolver).TryResolve<'msg>(id) with
                | Some(c) -> c
                | None -> failwithf "Unable to resolve channel (%s)" id

    let mutable internal resolver : IChannelResolver = new DefaultChannelResolver() :> IChannelResolver

    let getChannelResolver() = resolver

    let setChannelResolver(r) = resolver <- r

    [<AutoOpen>]
    module Operators =

        let lookupChannel<'msg> id = getChannelResolver().Resolve<'msg>(id)
    
        let lookupChannels<'msg> (ids : seq<string>) = Seq.map (fun id -> (lookupChannel<'msg> id)) ids
    
        let inline (<--) (channel : IChannel<'msg>) (msg : 'msg) = channel.Post(msg)
        let inline (<-*) (channels : seq<IChannel<'msg>>) (msg : 'msg) = channels |> Seq.iter (fun x -> x <-- msg)
        let inline (<-->) (channel : IChannel<'msg>) f = channel.PostAndAsyncReply(f)
        let inline (<-!>) (channel : IChannel<'msg>) f = channel.PostAndReply(f)
        let inline (<-?>) (channel : IChannel<'msg>) f = channel.PostAndTryAsyncReply(f) 
    
        //Registry operators
        let inline (?<--) (id : string) (msg : 'msg) = (lookupChannel<'msg> id) <-- msg
        let inline (?<-*) (ids : seq<string>) (msg : 'msg) = Seq.iter (fun id -> (lookupChannel<'msg> id) <-- msg) ids
        let inline (?<-->) (id : string) f = (lookupChannel<'msg> id) <--> f
        let inline (?<-!>) (id : string) f = (lookupChannel<'msg> id) <-!> f
        let inline (?<-?>) (id : string) f = (lookupChannel<'msg> id) <-?> f                   
        let inline (?<**) (msg : 'msg) = getChannelResolver().Resolve() <-* msg        