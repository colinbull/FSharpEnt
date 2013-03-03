namespace FSharp.Enterprise

module String = 
    
    open System

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

