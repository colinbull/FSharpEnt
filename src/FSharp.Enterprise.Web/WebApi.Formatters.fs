namespace FSharp.Enterprise.Web

module Formatters =

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
    open FSharp.Enterprise
    
    type JsonpMediaTypeFormatter() as self =
        inherit JsonMediaTypeFormatter()
        
        do
            self.SupportedMediaTypes.Add(new MediaTypeHeaderValue("application/json"));
            self.SupportedMediaTypes.Add(new Headers.MediaTypeHeaderValue("text/javascript"));
            self.MediaTypeMappings.Add(new UriPathExtensionMapping("jsonp", new MediaTypeHeaderValue("application/json")));
            
        let isJsonP (parameterName : string) = 
            if HttpContext.Current <> null
            then 
                let request = HttpContext.Current.Request
                match request.HttpMethod with
                | "POST" -> 
                    match request.Form.[parameterName] with
                    | null -> None | a -> Some(a)
                | _ ->
                   request.Url |> Uri.queryStringAsMap |> Map.tryFind parameterName
            else None
    
        member val CallbackQueryParameter = "callback" with get, set
    
        member private x.DoWriteToStreamAsync(``type``:Type, value:obj, stream:Stream, contentHeaders:HttpContent, transportContext) =
             base.WriteToStreamAsync(``type``, value, stream, contentHeaders, transportContext)
    
        override x.WriteToStreamAsync(``type``:Type, value:obj, stream:Stream, contentHeaders:HttpContent, transportContext) =
                match isJsonP x.CallbackQueryParameter with
                | Some(callback) ->
                         Task.Factory.StartNew(fun () ->
                                let writer = new StreamWriter(stream);
                                writer.Write(callback + "(");
                                writer.Flush();
                                x.DoWriteToStreamAsync(``type``, value, stream, contentHeaders, transportContext).Wait();
                                writer.Write(")");
                                writer.Flush();
                            )
                | None -> 
                    x.DoWriteToStreamAsync(``type``, value, stream, contentHeaders, transportContext)
    
    open FSharp.Enterprise.Web.Google.DataSource
    open FSharp.Enterprise.Web.Google.DataTable

    type GoogleDataTableFormatter() as self = 
         inherit MediaTypeFormatter()

         let serialiseResult (v:obj) (tqx:TQX) (uri:Uri) = 
             match v with
             | :? DataTable as a -> 
                   DataSourceResponse.Success(a, getSignature a, tqx)
             | :? Exception as err -> 
                   DataSourceResponse.Error([ErrorWarning.ofException(uri, err)], tqx)
             |> Json.ofObject

         do
            self.SupportedMediaTypes.Add(new MediaTypeHeaderValue("application/google+datatable"));
            self.SupportedMediaTypes.Add(new MediaTypeHeaderValue("application/google+datatable"));

         override x.CanReadType(t:Type) = false

         override x.CanWriteType(t:Type) = 
            t.BaseType = typeof<DataTable> 
            || t.BaseType = typeof<Exception>

         member private x.DoWriteToStreamAsync(``type``:Type, value:obj, stream:Stream, contentHeaders:HttpContent, transportContext) =
             base.WriteToStreamAsync(``type``, value, stream, contentHeaders, transportContext)

         override x.WriteToStreamAsync(t:Type, value:obj, stream:Stream, contentHeaders:HttpContent, transportContext) =
                 async {
                      let uri = HttpContext.Current.Request.Url
                      let (query, tqx) = parseQueryString uri
                      let result = serialiseResult value tqx uri
                      let writer = new StreamWriter(stream);
                      writer.Write(tqx.responseHandler + "(");
                      writer.Write(result)
                      writer.Write(")")
                      writer.Flush();
                      x.DoWriteToStreamAsync(t, value, stream, contentHeaders, transportContext).Wait();
                      writer.Flush();
                 } |> Async.toActionTask
    