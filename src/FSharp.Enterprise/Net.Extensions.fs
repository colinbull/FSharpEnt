namespace FSharp.Enterprise

[<AutoOpen>]
module NetExtensions =
    
    open System.Net
    open System.Threading

    type System.Net.HttpListener with

      member x.AsyncGetContext() = 
        Async.FromBeginEnd(x.BeginGetContext, x.EndGetContext)
    
    type HttpListener with 

      static member Start(url, f) = 
        let tokenSource = new CancellationTokenSource()
        Async.Start
          ( ( async { 
                use listener = new HttpListener()
                listener.Prefixes.Add(url)
                listener.Start()
                while true do 
                  let! context = listener.AsyncGetContext()
                  Async.Start(f context, tokenSource.Token) }),
            cancellationToken = tokenSource.Token)
        tokenSource
    
      static member StartSynchronous(url, f) =
        HttpListener.Start(url, f >> async.Return)

