﻿namespace FSharp.Enterprise

module WebApi = 

    open System.Runtime.CompilerServices

    [<Extension>]
    [<AutoOpen>]
    module Extensions =
        open System
        open System.Threading.Tasks
        open System.IO
        open Newtonsoft.Json
        open System.Net.Http
        open System.Net
        open System.Web.Http
        open System.Net.Http.Formatting
        open System.Net.Http.Headers
        open System.Web
        
        type ApiController with
            member x.TryGetUser() = 
                if x.User <> null && x.User.Identity <> null
                then Some(x.User.Identity.Name)
                else None

            member x.TryGetUserOrDefault(?defaultUser : string) = 
                match x.TryGetUser() with
                | Some(u) -> u
                | None -> defaultArg defaultUser "Unknown"

        type HttpRequestMessage with
        
             member x.Error(?err : Exception) = 
                  (match err with
                   | Some(err) -> x.CreateErrorResponse(HttpStatusCode.InternalServerError, err)
                   | None -> x.CreateErrorResponse(HttpStatusCode.InternalServerError,"Unknown")
                  )
                
             member x.Error(code, ?err : Exception) = 
                (match err with
                 | Some(err) -> x.CreateErrorResponse(code, err)
                 | None -> x.CreateErrorResponse(code,"Unknown")
                )
        
             member x.Success<'a>(code, ?payload : 'a) =
                 match payload with
                 | Some(a) -> x.CreateResponse(code, a)
                 | None -> x.CreateResponse(code)
        
             member x.Success<'a>(?payload : 'a) =
                 x.Success(HttpStatusCode.OK, ?payload = payload)
        
             member x.NotFound() = 
                 x.Error(HttpStatusCode.NotFound)
        
             member x.BadRequest() = 
                 x.Error(HttpStatusCode.BadRequest)
        
             member x.Created() = 
                 x.Success(HttpStatusCode.Created)
        
             member x.FromOption(payload : 'a option) =
                 match payload with
                 | Some(p) -> x.Success(p)
                 | None -> x.NotFound()
        
             member x.SavePostedFiles(path : string) = 
                 if (not <| x.Content.IsMimeMultipartContent()) 
                 then raise(new HttpResponseException(HttpStatusCode.UnsupportedMediaType))
                 else 
                     async {
                          let! provider = x.Content.ReadAsMultipartAsync(new MultipartFormDataStreamProvider(path)) |> Async.AwaitTask; 
                          return 
                              provider, seq {
                                   for fileData in provider.FileData do
                                       yield fileData.LocalFileName
                              }
                      }
