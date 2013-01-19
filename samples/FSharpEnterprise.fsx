(** 
# F# Enterprise: Library for Enterprise development

The F# Enterprise library collection aims to simplify common tasks encountered when developing enterprise applications, by 
providing a full set of helpers for:

* Asyncronous / Agent Programming
* Scheduling
* Serialisaton
* Messaging
* and much more...

This library is intended to complement other F# libraries, such as [FSharpx](http://github.com/fsharp/fsharpx) 
and [FSharp.Data](https://github.com/tpetricek/FSharp.Data)

F# Enterprise consists of the following set of dlls.

* [FSharp.Enterprise.dll](Core.html)
* [FSharp.Enterprise.RabbitMq.dll](RabbitMq.html)

This provides a set of types that implement common messaging patterns over RabbitMQ. 

* [RPC](RabbitMq.RPC.html)
* [Pubish - Subscribe](RabbitMq.PubSub.html)
* [Channel](RabbitMq.Channel.html)

*)