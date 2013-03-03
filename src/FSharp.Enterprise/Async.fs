namespace FSharp.Enterprise

module Async =
    open System

    let rec retry count interval (isRetryException:System.Exception->bool) (work:Async<'T>) = 
        async { 
            try 
                let! result = work
                return Choice1Of2 result
            with e ->
                if isRetryException e && count > 0 then
                    do! Async.Sleep interval  
                    return! retry (count - 1) interval isRetryException work
                else 
                    return Choice2Of2 e
        }
        
    let toTask (async : Async<_>) = System.Threading.Tasks.Task.Factory.StartNew(fun _ -> Async.RunSynchronously(async))