namespace FSharp.Enterprise

module Uri =
    open System

    let queryStringAsMap (uri : Uri) =
        uri.Query.Replace("?", "")
        |> Uri.UnescapeDataString
        |> String.toMap "=" "&"
    
    let tryCreate (uri : String) =
        match Uri.TryCreate(uri, UriKind.Absolute) with
        | true, uri -> uri |> Some
        | false, _ -> None

    let (|FTP|HTTP|HTTPS|FILE|) (uri : Uri) =
        match uri.Scheme.ToLower() with
        | "ftp" -> FTP uri
        | "http" -> HTTP uri
        | "https" -> HTTPS uri
        | "file" -> FILE uri
        | err -> failwithf "%s is an unknown scheme" err

