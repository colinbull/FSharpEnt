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

    let cron cron job = Cron.toAsync cron job
        
    let toTask (async : Async<_>) = System.Threading.Tasks.Task.Factory.StartNew(fun _ -> Async.RunSynchronously(async))

[<AutoOpen>]
module AsyncTypes = 
    
    open System
    open System.Threading
    open System.Threading.Tasks

    //http://moiraesoftware.com/blog/2012/01/30/FSharp-Dataflow-agents-II/
    type ResultCell<'a>() =
        let source = new TaskCompletionSource<'a>()
    
        member x.RegisterResult result = source.SetResult(result)
    
        member x.AsyncWaitResult =
            Async.FromContinuations(fun (cont,_,_) -> 
                let apply = fun (task:Task<_>) -> cont (task.Result)
                source.Task.ContinueWith(apply) |> ignore)
    
        member x.GetWaitHandle(timeout:int) =
            async { let waithandle = source.Task.Wait(timeout)
                    return waithandle }
    
        member x.GrabResult() = source.Task.Result
    
        member x.TryWaitResultSynchronously(timeout:int) = 
            if source.Task.IsCompleted then 
                Some source.Task.Result
            else 
                if source.Task.Wait(timeout) then 
                    Some source.Task.Result
                else None

    type SynchronizationContext with 

        member syncContext.RaiseEvent (event: Event<_>) args = 
            syncContext.Post((fun _ -> event.Trigger args),state=null)

        static member CaptureCurrent() = 
            match SynchronizationContext.Current with 
            | null -> new SynchronizationContext()
            | ctxt -> ctxt
    
    //http://blogs.msdn.com/b/dsyme/archive/2010/01/10/async-and-parallel-design-patterns-in-f-reporting-progress-with-events-plus-twitter-sample.aspx
    type AsyncWorker<'a>(jobs: seq<Async<'a>>) =  
 
        let jobCompleted = new Event<'a>()
        let allCompleted = new Event<'a []>()
        let cancelled = new Event<OperationCanceledException>()
        let jobErrored = new Event<exn>()
        let mutable token = Async.DefaultCancellationToken
        let mutable syncCtx = SynchronizationContext.CaptureCurrent()
        let mutable jobStore = []

        do 
            jobStore <- jobs |> Seq.toList

        new() =
            AsyncWorker(Seq.empty)

        member x.Start() =  
            let work =  
               Async.Parallel 
                    [ for job in jobStore -> 
                        async {
                                let! result = job
                                syncCtx.RaiseEvent jobCompleted result
                                return result
                              }]

            Async.StartWithContinuations(work, 
                                         syncCtx.RaiseEvent allCompleted,
                                         syncCtx.RaiseEvent jobErrored,
                                         syncCtx.RaiseEvent cancelled,
                                         token) 

        member x.AddJobs(js: seq<Async<'a>>) = 
            jobStore <- (Seq.append jobStore js) |> Seq.toList
        member x.JobCount with get() = Seq.length jobStore
        member x.CancellationToken with get() = token and set v = token <- v
        member x.SynchronizationContext with get() = syncCtx and set v = syncCtx <- v
        member x.Cancelled = cancelled.Publish
        member x.AllCompleted = allCompleted.Publish
        member x.JobCompleted  = jobCompleted.Publish
        member x.JobErrored = jobErrored.Publish
        member x.GetJobs() = jobStore

        static member Empty 
            with get() = new AsyncWorker<'a>([])