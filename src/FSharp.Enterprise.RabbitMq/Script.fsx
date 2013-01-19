
#r @"..\packages\Newtonsoft.Json.4.5.7\lib\net40\Newtonsoft.Json.dll"
#r @"..\packages\RabbitMQ.Client.2.8.1\lib\net30\RabbitMQ.Client.dll"
#r @"bin\Debug\FSharpEnt.Common.dll"

#load "RabbitMq.fs"
#time
open System
open FSharpEnt
type Message = 
    {
        Timestamp : DateTime
        Message : string
    }

let listener = 
    RabbitMQ.listen<Message>()

listener.Observer |> Event.add (printfn "%A")

let send (txt :string) =
    RabbitMQ.publish  { Timestamp = DateTime.Now; Message = txt}

let doCancel() = listener.Cancel() 




