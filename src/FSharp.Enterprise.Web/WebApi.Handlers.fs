namespace FSharp.Enterprise.Web

module Handlers =

    open System
    open System.Threading
    open System.Threading.Tasks
    open System.IO
    open Newtonsoft.Json
    open System.Net.Http
    open System.Net
    open System.Web.Http
    open System.Net.Http.Formatting
    open System.Net.Http.Headers
    open System.Web.Http.Tracing
    open System.Web
    open System.Linq

    type TracingHandler(logger : (string -> unit)) =
         inherit DelegatingHandler()

         override x.SendAsync(request:HttpRequestMessage, cancellationToken:CancellationToken) =
            logger (String.Format("{0} : {1} - {2}", DateTime.Now, request.RequestUri.AbsolutePath, request.Method))
            base.SendAsync(request, cancellationToken);
            

    type CorsHandler() =
      inherit DelegatingHandler()
    
      let Origin = "Origin";
      let AccessControlRequestMethod = "Access-Control-Request-Method";
      let AccessControlRequestHeaders = "Access-Control-Request-Headers";
      let AccessControlAllowOrigin = "Access-Control-Allow-Origin";
      let AccessControlAllowMethods = "Access-Control-Allow-Methods";
      let AccessControlAllowHeaders = "Access-Control-Allow-Headers";
     
      override x.SendAsync(request : HttpRequestMessage, cancellationToken : CancellationToken) =
          let isCorsRequest = request.Headers.Contains(Origin)
          let isPreflightRequest = request.Method = HttpMethod.Options
          if isCorsRequest
          then
              if isPreflightRequest
              then
                  let response = new HttpResponseMessage(HttpStatusCode.OK);
                  response.Headers.Add(AccessControlAllowOrigin, request.Headers.GetValues(Origin).First());
     
                  let accessControlRequestMethod = request.Headers.GetValues(AccessControlRequestMethod).FirstOrDefault();
                  
                  if (accessControlRequestMethod <> null)
                  then response.Headers.Add(AccessControlAllowMethods, accessControlRequestMethod);
                  
                  let requestHeaders = 
                      match request.Headers.TryGetValues(AccessControlRequestHeaders) with
                      | true, headers -> headers
                      | false, _ -> Seq.empty 
                  let requestedHeaders = String.Join(", ", requestHeaders);
                  if (not <| String.IsNullOrEmpty(requestedHeaders))
                  then response.Headers.Add(AccessControlAllowHeaders, requestedHeaders);
                  
    
                  let tcs = new TaskCompletionSource<HttpResponseMessage>();
                  tcs.SetResult(response);
                  tcs.Task;
              else
                  base.SendAsync(request, cancellationToken)
                      .ContinueWith(fun (t : Task<HttpResponseMessage>) -> 
                                    let resp = t.Result
                                    resp.Headers.Add(AccessControlAllowOrigin, request.Headers.GetValues(Origin).First())
                                    resp
                                   )
          else base.SendAsync(request, cancellationToken)
