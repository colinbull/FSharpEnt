namespace FSharp.Enterprise

open FSharp.Enterprise

module Channel =

    module Http =

        open System
        open Channel
        open Serialisation
        open System.Net.Http
        open System.Net.Http.Formatting
         
        type Verb<'a> = 
            | Get
            | Post of 'a option
            | Put of 'a option
            | Delete

        type Request<'a> = string * Verb<'a> * MediaTypeFormatter option

        type HttpChannel<'msg>(id, ?defaultFormatter : MediaTypeFormatter) =
            let defaultFormatter = defaultArg defaultFormatter (JsonMediaTypeFormatter() :> MediaTypeFormatter)
            let client = new HttpClient()

            let makeRequest (uri, verb, formatter) =
                let formatter = defaultArg formatter defaultFormatter
                match verb with
                | Post(payload) ->
                     match payload with
                     | Some(p) -> client.PostAsync(uri, p, formatter)
                     | None -> client.PostAsync(uri, null)
                | Put(payload) ->
                     match payload with
                     | Some(p) -> client.PutAsync(uri, p, formatter)
                     | None -> client.PutAsync(uri, null)
                | Delete -> client.DeleteAsync(uri)
                | Get -> client.GetAsync(uri)
                |> Async.AwaitTask
                

            interface IChannel<Request<'msg>> with
                member x.Name with get() = id
                member x.Post(msg) = makeRequest msg |> Async.Ignore |> Async.Start
                member x.PostAndAsyncReply(builder) = 
                    async {
                        let! response = builder(new NullReplyChannel<_>()) |> makeRequest
                        return! response.Content.ReadAsAsync<_>() |> Async.AwaitTask
                    }
                member x.PostAndReply(builder) = 
                    async {
                        let! response = builder(new NullReplyChannel<_>()) |> makeRequest
                        return! response.Content.ReadAsAsync<_>() |> Async.AwaitTask
                    } |> Async.RunSynchronously
                member x.PostAndTryAsyncReply(builder) =
                    async {
                        let! response = builder(new NullReplyChannel<_>()) |> makeRequest
                        if response.IsSuccessStatusCode
                        then 
                            let! content = response.Content.ReadAsAsync<_>() |> Async.AwaitTask
                            return Some(content)
                        else return None
                    }

