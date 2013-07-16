namespace FSharp.Enterprise

module String = 
    
    open System
    open System.IO

    let toOption (str : string) = 
        if String.IsNullOrEmpty(str) || String.IsNullOrWhiteSpace(str)
        then None
        else Some(str)

    let fromOption (str : string option) = 
        Option.fold (fun _ x -> x) String.Empty str

    let split (delimiter : string) splitOptions (str : string)  =
        str.Split([|delimiter|], splitOptions)
        |> Seq.map (fun x -> x.Trim())

    let toMap kvDelimiter entryDelimiter str : Map<string, string> = 
        split entryDelimiter StringSplitOptions.RemoveEmptyEntries str
        |> Seq.map (split kvDelimiter StringSplitOptions.None
                    >> Seq.toArray
                    >> function
                       | [|h;t|] -> h,t
                       | _ -> invalidArg "str" (sprintf "Unable to parse string %s to map" str)
                   )
        |> Map.ofSeq

    let capitalize after (id : string) =
        let chars = id.ToCharArray() 
        let rec replace index (prev : char) (remaining : char list) =
            match remaining with
            | h :: t when h = after -> 
                chars.[index] <- ' '
                replace (index + 1) after t
            | h :: t when prev = after || prev = '\000' -> 
                chars.[index] <- Char.ToUpper(h)
                replace (index + 1) h t
            | h :: t -> replace (index + 1) h t
            | [] -> new String(chars)
        replace 0 '\000' (id |> Seq.toList)

    let ofStreamAsync (stream:Stream) = 
            async {
                    let buffer = Array.zeroCreate (4 * 1024)
                    use output = new MemoryStream()
                    let reading = ref true
  
                    while reading.Value do
                      let! count = stream.AsyncRead(buffer, 0, buffer.Length)
                      output.Write(buffer, 0, count)
                      reading := count > 0

                    output.Seek(0L, SeekOrigin.Begin) |> ignore
                    use sr = new StreamReader(output)
                    return sr.ReadToEnd() 
           }
           
    let toStreamAsync (stream:Stream) (str:string) =
        async {
                let buffer = Array.zeroCreate (4 * 1024)
                let reading = ref true

                use sr = new StringReader(str)
                
                while reading.Value do
                    let count = sr.ReadBlock(buffer, 0, buffer.Length)
                    do! stream.AsyncWrite(buffer |> Array.map Convert.ToByte, 0, buffer.Length)
                    reading := count > 0
        }     

