(** 
# F# Enterprise: Library for Enterprise development

The F# Enterprise library collection aims to simplify common tasks encountered when developing enterprise applications, by 
providing a full set of helpers for:

* Asyncronous / Agent Programming
* Scheduling
* Serialisaton
* Messaging
* Web
* and much more...

This library is intended to complement other F# libraries, such as [FSharpx](http://github.com/fsharp/fsharpx) 
and [FSharp.Data](https://github.com/tpetricek/FSharp.Data)

F# Enterprise consists of the following set of dlls.

## FSharp.Enterprise.dll
This provides a core set of modules,

* [Agent](Core.Agent.html)
* [Async](Core.Async.html)
* [Caching](Core.Caching.html)
* [Channel](Core.Channel.html)
* [DateTime](Core.DateTime.html)
* [Environment](Core.Environment.html)
* [FileSystem](Core.FileSystem.html)
* [Html](Core.Html.html)
* [IO](Core.IO.html)
* [Json](Core.Json.html)
* [Net](Core.Net.html)
* [Process](Core.Process.html)
* [Regex](Core.Regex.html)
* [Scheduling](Core.Scheduling.html)
* [Security](Core.Security.html)
* [Serialisation](Core.Serialisation.html)
* [String](Core.String.html)
* [Uri](Core.Uri.html)
* [Xml](Core.Xml.html)

## FSharp.Enterprise.RabbitMq.dll

This provides a set of types that implement common messaging patterns over RabbitMQ. 

* [RPC](RabbitMq.RPC.html)
* [Pubish - Subscribe](RabbitMq.PubSub.html)
* [Channel](RabbitMq.Channel.html)

## FSharp.Enterprise.Web.dll

This provides a set of modules and extensions that helps with web programming.

* [Channels](Web.Channel.html)
* [WebAPI](Web.WebApi.html)

*)