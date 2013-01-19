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

### FSharp.Enterprise.dll

This provides the common functions that the other libraries require. Additionally several key modules are in this module

* [Channel](Channel.html)
* [Environment](Environment.html)
* [DateTimeExtensions](DateTime.html)
* [Agents](Agent.html)
* [Serialisation](Serialisation.html)

### FSharp.Enterprise.RabbitMq.dll

This provides a set of types that implement common messaging patterns over RabbitMQ. 

* [RPC](RabbitMq.RPC.html)
* [Pubish - Subscribe](RabbitMq.PubSub.html)
* [Channel](RabbitMq.Channel.html)

*)