namespace FSharp.Enterprise

module Process =
    
    open System
    open System.Diagnostics

    type Message = {
        IsError : bool
        Message : string
        Timestamp : DateTimeOffset
    }

    type MessageHandler = 
        | StandardError of (string -> unit) * (string -> unit)
        | Formatted of (Message -> unit) 
        | Silent 

    let private redirectOutput (p:Process) = function
        | StandardError(errorF, messageF) ->
            p.StartInfo.RedirectStandardOutput <- true
            p.StartInfo.RedirectStandardError <- true
            p.ErrorDataReceived.Add (fun d -> if d.Data <> null then errorF d.Data)
            p.OutputDataReceived.Add (fun d -> if d.Data <> null then messageF d.Data)
            true
        | Formatted(messageF) ->
            p.StartInfo.RedirectStandardOutput <- true
            p.StartInfo.RedirectStandardError <- true
            p.ErrorDataReceived.Add (fun d -> if d.Data <> null then messageF { IsError = true; Message = d.Data; Timestamp = DateTimeOffset.UtcNow } )
            p.OutputDataReceived.Add (fun d -> if d.Data <> null then messageF { IsError = false; Message = d.Data; Timestamp = DateTimeOffset.UtcNow })
            true
        | Silent -> false

    let execute infoAction (timeOut:TimeSpan) messageHandler =
        use p = new Process()
        p.StartInfo.UseShellExecute <- false
        infoAction p.StartInfo
        let redirected = redirectOutput p messageHandler
        try
            p.Start() |> ignore
        with exn -> 
            raise (Exception(sprintf "Start of process %s failed. %s" p.StartInfo.FileName exn.Message))
    
        if redirected then
            p.BeginErrorReadLine()
            p.BeginOutputReadLine()     
      
        if timeOut = TimeSpan.MaxValue then
            p.WaitForExit()
        else
            if not <| p.WaitForExit(int timeOut.TotalMilliseconds) then
                try
                    p.Kill()
                with exn ->
                    raise (Exception(sprintf "Process %s %s timed out." p.StartInfo.FileName p.StartInfo.Arguments))
        
        p.ExitCode
    
    
    let executeElevated infoAction timeOut messageHandler =
        execute (fun si -> 
                       infoAction si
                       si.Verb <- "runas"
                )  timeOut messageHandler


    