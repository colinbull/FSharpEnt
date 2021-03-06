﻿(**
# F# Enterprise - Channels

The following sample demonstrates the `FSharp.Enterprise.Channel` module.

**Channels** can provide a simple abstraction over a variety of sources. 
The channel interface provides a four methods

* abstract Post : 'a -> unit
* abstract PostAndAsyncReply<'b> : (IReplyChannel<'b> -> 'a) -> Async<'b>
* abstract PostAndReply<'b> :  (IReplyChannel<'b> -> 'a) -> 'b
* abstract PostAndTryAsyncReply<'b> : (IReplyChannel<'b> -> 'a) -> Async<'b option>

*)
