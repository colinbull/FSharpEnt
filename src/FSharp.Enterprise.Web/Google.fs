namespace FSharp.Enterprise.Web.Google

module DataTable =
    
    open System
    open System.Reflection
    open FSharp.Enterprise

    type DataTableColumnLabelAttribute(label : string) = 
        inherit Attribute()
        member x.Label with get() = label
    
    type DataTableColumnRoleAttribute(role : string) = 
        inherit Attribute()
        member x.Role with get() = role

    type DataTableColumnCustomizerAttribute(customizer : string) = 
        inherit Attribute()
        member x.Customizer with get() = customizer
    
    type DataTableColumnProps = {
        role : string
        html : bool
    }
    with 
        static member Create(?role) = 
            { role = defaultArg role null; html = true; }

    type DataTableColumn = {
        ``type`` : string
        id : string
        label : string
        p : DataTableColumnProps
    }
    with
        static member Create(dataType, ?id, ?label, ?role) =
            {
                ``type`` = dataType 
                id = defaultArg id null
                label = defaultArg label null
                p = DataTableColumnProps.Create(?role = role)
            }
    
    type DataTableCell = {
        v : obj
        f : string
    }
    with 
        static member Create(o : obj, ?stringValue : string) =
            if o <> null
            then
                let va =
                    match o with
                    | :? System.DateTime as a ->
                      sprintf "Date(%d,%d,%d,%d,%d,%d,%d)" a.Year (a.Month-1) a.Day a.Hour a.Minute a.Second a.Millisecond
                      |> box
                    | :? System.DateTimeOffset as a ->
                      sprintf "Date(%d,%d,%d,%d,%d,%d,%d)" a.Year (a.Month-1) a.Day a.Hour a.Minute a.Second a.Millisecond
                      |> box
                    | a -> a |> box
                { v = va; f = (defaultArg stringValue null) }
            else { v = null; f = null }
    
    type DataTableRow = {
        c : seq<DataTableCell>
    }
    with 
        static member Create(cells : seq<_>) = { c = cells }
    
    [<AllowNullLiteral>]
    type DataTable(tCols, tRows) =
        
        let mutable cols' = tCols
        let mutable rows' = tRows
        
        static let getTypeStr t =
            match Type.GetTypeCode(t) with
            | TypeCode.Boolean -> "boolean"
            | TypeCode.Decimal | TypeCode.Double
            | TypeCode.Int16 | TypeCode.Int32
            | TypeCode.Int64 | TypeCode.UInt16 
            | TypeCode.UInt32 | TypeCode.UInt64
            | TypeCode.Single  -> "number"
            | TypeCode.String -> "string"
            | TypeCode.DateTime -> "datetime"
            | _ -> 
                match t.FullName with
                | "System.TimeSpan" -> "timeofday"
                | "System.DateTimeOffset" -> "datetime"
                | _ -> failwithf "Cannot handle type %A only, boolean, number, string, datetime, timeofday type supported" t
        
        new() = 
           DataTable(Seq.empty, Seq.empty)
    
        member x.cols with get() = cols' and set(v) = cols' <- v
        member x.rows with get() = rows' and set(v) = rows' <- v
    
        member x.toJson() = 
            Json.ofObject x

        static member ofSeq(seq : seq<'a>) =
            let createColumn (t : PropertyInfo) =
                let typeString = getTypeStr t.PropertyType
                let colLabel = 
                    match t.GetCustomAttributes(typeof<DataTableColumnLabelAttribute>, false) |> List.ofArray with
                    | h :: t -> (h :?> DataTableColumnLabelAttribute).Label
                    | _ -> t.Name
                let role = 
                    match t.GetCustomAttributes(typeof<DataTableColumnRoleAttribute>, false) |> List.ofArray with
                    | h :: t -> (h :?> DataTableColumnRoleAttribute).Role
                    | _ -> null
                let p = 
                    match t.GetCustomAttributes(typeof<DataTableColumnCustomizerAttribute>, false) |> List.ofArray with
                    | h :: t -> (h :?> DataTableColumnCustomizerAttribute).Customizer
                    | _ -> null
                { ``type`` = typeString; id = t.Name; label = colLabel; p = DataTableColumnProps.Create(role) }
            
            let createRow (props : seq<PropertyInfo>) (instance : 'a) =
                DataTableRow.Create(props |> Seq.map (fun p -> DataTableCell.Create(p.GetValue(instance, null))))
    
            let props = typeof<'a>.GetProperties()
            let cols = props |> Seq.map (createColumn)
            let rows = seq |> Seq.map (createRow props)
            DataTable(cols, rows)

module DataSource =
       
       open System
       open System.Net.Http
       open System.Collections.Generic
       open FSharp.Enterprise
       open DataTable
       
       let private defaultOut = "json"
       let private defaultResponseHandler = "google.visualization.Query.setResponse"
       let private defaultVersion = "0.6"
       let private defaultReqId = "0"

       type TQ = string
       type TQX = {
            reqId : string
            version : string
            ``sig`` : string
            out : string
            responseHandler : string
            outFileName : string
       }
       with 
            static member empty = 
                {
                     reqId = defaultReqId
                     version = defaultVersion
                     ``sig`` = null
                     out = defaultOut
                     responseHandler = defaultResponseHandler
                     outFileName = null
                }

       type ErrorWarningReason = 
            | DataTruncated
            | Other
            | NotModified
            | UserNotAuthenticated
            | UnknownDataSourceId
            | AccessDenied
            | UnsupportedQueryOperation
            | InvalidQuery
            | InvalidRequest
            | InternalError
            | NotSupported
            | IllegalFormattingPatterns
            with
                override x.ToString() = 
                      match x with
                      | DataTruncated -> "data_truncated"
                      | Other -> "other"
                      | NotModified -> "not_modified"
                      | UserNotAuthenticated -> "user_not_authenticated"
                      | UnknownDataSourceId -> "unknown_data_source_id"
                      | AccessDenied -> "access_denied"
                      | UnsupportedQueryOperation -> "unsupported_query_operation"
                      | InvalidQuery -> "invalid_query"
                      | InvalidRequest -> "invalid_request"
                      | InternalError -> "internal_error"
                      | NotSupported -> "not_supported"
                      | IllegalFormattingPatterns -> "illegal_formatting_patterns"

       type ErrorWarning = {
            reason : string
            message : string
            detailed_message: string
       }
       with
            static member ofException(uri : Uri,ex : exn) = 
                   {
                       reason = InternalError.ToString()
                       message = ex.Message
                       detailed_message = ex.ToString()
                   }
            static member Warning(?reason : ErrorWarningReason, ?message, ?detail) =
                   {
                       reason = (defaultArg reason Other).ToString()
                       message = defaultArg message ""
                       detailed_message = defaultArg detail ""
                   }

            static member Error(?reason : ErrorWarningReason, ?message, ?detail) =
                   {
                       reason = (defaultArg reason Other).ToString()
                       message = defaultArg message ""
                       detailed_message = defaultArg detail ""
                   } 
            static member NotModified() = 
                  {
                     reason=  ErrorWarningReason.NotModified.ToString()
                     message =  ""
                     detailed_message = ""
                  }

       type DataSourceResponse = {
            version : string
            reqId : string
            status : string
            warnings : ResizeArray<ErrorWarning>
            errors : ResizeArray<ErrorWarning>
            ``sig`` : string
            table : DataTable
       }
       with
            static member Error(errors : seq<_>, tqx : TQX) = 
                {
                  version  = tqx.version
                  reqId = tqx.reqId
                  status = "error"
                  warnings = null
                  errors = new ResizeArray<_>(errors)
                  ``sig`` = null
                  table = null
                }
            static member Success(table : DataTable, signature : string ,tqx : TQX) = 
                 {
                  version  = tqx.version
                  reqId = tqx.reqId
                  status = "ok"
                  warnings = null
                  errors = null
                  ``sig`` = signature
                  table = table
                }
            static member NotModified(tqx : TQX) = 
                {
                  version  = tqx.version
                  reqId = tqx.reqId
                  status = "error"
                  warnings = null
                  errors = new ResizeArray<_>([ErrorWarning.NotModified()])
                  ``sig`` = tqx.``sig``
                  table = null
                }
       
       let parseQueryString (uri : Uri) =
           let query = Uri.queryStringAsMap uri 
           match query |> Map.tryFind("tqx") |> Option.map (String.toMap ":" ";") with
           | Some(tqxParameters) ->
                 let tqx = {
                          reqId = (defaultArg (Map.tryFind "reqId" tqxParameters) defaultReqId)
                          version = (defaultArg (Map.tryFind "version" tqxParameters) defaultVersion)
                          ``sig`` = (defaultArg (Map.tryFind "sig"  tqxParameters) null)
                          out = (defaultArg (Map.tryFind "out" tqxParameters) defaultOut)
                          responseHandler = (defaultArg (Map.tryFind "responseHandler" tqxParameters) defaultResponseHandler)
                          outFileName = (defaultArg (Map.tryFind "outFileName" tqxParameters) null)
                     }
                 (defaultArg (Map.tryFind "tq" query) null), tqx
            | None -> (defaultArg (Map.tryFind "tq" query) null), TQX.empty
       
       let getSignature (dataTable : DataTable) = 
           Math.Abs(dataTable.toJson().GetHashCode()).ToString()
       
       let success (table:DataTable) (tqx : TQX) = 
           match table with
           | null -> DataSourceResponse.NotModified(tqx)
           | table -> DataSourceResponse.Success(table, "", tqx)

       let error (err:Exception) (uri:Uri) (tqx:TQX) = 
           DataSourceResponse.Error([ErrorWarning.ofException(uri, err)], tqx)

       let private writeResponse (request : HttpRequestMessage) (tqx : TQX) (response : DataSourceResponse) = 
           let str = String.Format("{0}({1})", tqx.responseHandler, Json.ofObject response)
           let msg = request.CreateResponse(Net.HttpStatusCode.OK)
           msg.Content <- new StringContent(str)
           msg

       type HttpRequestMessage with
            member request.CreateGoogleDatasourceResponse(table : DataTable, ?exn : Exception) =
                let (query, tqx) = parseQueryString (request.RequestUri)
                try
                    match exn with
                    | Some(err) -> error err request.RequestUri tqx
                    | None -> success table tqx
                with e ->
                    error e request.RequestUri tqx
                |> writeResponse request tqx
