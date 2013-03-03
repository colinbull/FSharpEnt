(**
# F# Enterprise - Agents

The following sample demonstrates the `FSharp.Enterprise.Agents` module. Agents are a powerful pattern and
can make asyncronous programming far easier by encapsulating the computation. Communication with agents 
is achieved via passing messages to the instances of the agent. This agent can optionally mutate some internal state
or pass the result of the computation to another agent via another message. To this end entire networks of agents 
can be created. 
*)

#r @"..\bin\FSharp.Enterprise.dll"

open System
open FSharp.Enterprise

(**
### Creating a simple agent
In this example we create a simple agent that accepts a string and just prints the received message out to the console. 
*)

let simpleAgent = 
    Agent.bind (Agent.receive 
            (fun msg -> 
                 async {
                    do printfn "Recieved message %s" msg
                 }
            ))

(**
This example uses the bind and recieve combinators. This is a common pattern and thus the library also has the 'Agent.reciever' combinator
(from now on I will use the wrapped varients)

Each agent is given an id (by default this is `Guid.NewGuid()`) but this can be set when creating the agent using `Agent.bindNamed` 
or after the agent is created using the `Agent.named` combinator.

When using `MailboxProcessor<'a>` we have several methods available to select how we recieve messages. Scan, Recieve (see above) and TryReceive
within `FSharp.Enterprise.Agents` there are analogous combinators for these methods.
*)

let ifGreaterThan5 = 
    (fun msg -> 
        match msg with
        | a when a >= 5 -> async { return msg } |> Some
        | _ -> None)

let greaterThan5 = 
    Agent.scanner ifGreaterThan5
     (fun msg -> 
         async {
            do printfn "Recieved message %d" msg
         })

(**
Above we defined a selector to filter the incoming messages if the value of msg > 5. This is then passed into the
`Agent.scanner` function along with the actual body to pass the message onto if the predicate passes.
 
### Handling Errors

Error handling is always an important part of any enterprise application. With agents it is no different. However because
agents are asyncronous errors are materialised via an event.
*)

let simpleAgentWithErrorHandling = 
    simpleAgent |> Agent.onError (fun agent err -> printfn "Agent %s errored:\r\n%s" agent.Id err.Message)

(**
### A higher order agent.

Below we define an agent that has the same behaviour as the scanning agent above, however here we build the same filtering behaviour 
by combining several combinators and the simple agent we defined above. Additionally we can intercept the messages going through the pipeline.
 In this sense we can create *higher order* agents by combining several other simple agents. 
*)

let complexAgent = 
    simpleAgent
    |> Agent.map (fun x -> x.ToString())
    |> Agent.filter (fun x -> x >= 5)
    |> Agent.intercept (fun x -> async { do printfn "Intercept: %A" x })

complexAgent.Post(6)
complexAgent.Post(4)



