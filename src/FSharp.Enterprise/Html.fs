namespace FSharp.Enterprise

module Html = 
    
    open System
    open System.IO
    open System.Xml

    let private nullChar = Convert.ToChar(0x0)

    type HtmlAttribute = string * string
    type HtmlToken =
        | Tag of bool * string * HtmlAttribute list
        | TagEnd of string
        | Text of string
    
    let private peek (inp : #StreamReader) =
            inp.Peek() |> char
     
    let private read (inp : #StreamReader) =
            inp.Read() |> char

    let private readN n (inp : #StreamReader) = 
        let buffer = Array.zeroCreate n
        inp.ReadBlock(buffer, 0, n) |> ignore
        String(buffer)

    let private pop (inp :  #StreamReader) =
            inp.Read() |> ignore

    let private emit refCell inp =
        refCell := read inp :: !refCell

    let private emitChar refCell c = 
        refCell := c :: !refCell
    
    let private startNewAttribute attributes = 
        attributes := (ref [], ref []) :: !attributes

    let rec private emitCurrentAttrName attributes (inp : #StreamReader) =
        match !attributes with
        | [] ->  
            startNewAttribute attributes
            emitCurrentAttrName attributes inp
        | h :: _ -> emit (fst h) inp

    let private toString (cs : char list ref) =
        String(!cs |> List.rev |> Seq.toArray)

    let rec private currentAttrName attributes =
        match !attributes with
        | [] ->  String.Empty
        | h :: _ -> toString (fst h)

    let rec private emitCurrentAttrValue attributes (inp : #StreamReader) =
        match !attributes with
        | [] ->  
            startNewAttribute attributes
            emitCurrentAttrValue attributes inp
        | h :: _ -> emit (snd h) inp

    let private toAttributes attributes =
        !attributes 
        |> List.map (fun (key,value) -> toString key, toString value)

    let private (|NullChar|_|) (c : Char) =
        if (c |> int) = 0 then Some c else None

    let private (|EndOfFile|_|) (c : Char) =
        let value = c |> int
        if (value = -1 || value = 65535) then Some c else None

    let private (|UpperAtoZ|_|) (c : Char) =
        if Char.IsUpper(c) then Some c else None

    let private (|LowerAtoZ|_|) (c : Char) =
        if Char.IsLower(c) then Some c else None

    let private (|Number|_|) (c : Char) =
        if Char.IsNumber(c) then Some c else None

    let private (|Symbol|_|) (c : Char) =
        if Char.IsPunctuation(c) then Some c else None

    let private (|Whitespace|_|) (c : Char) =
        if Char.IsWhiteSpace(c) then Some c else None

    let private (|LetterDigit|_|) = function
        | LowerAtoZ c -> Some c
        | Number c -> Some c
        | UpperAtoZ c -> Some (Char.ToLower(c))
        | _ -> None

    let private (|LetterDigitSymbol|_|) = function
        | LowerAtoZ c -> Some c
        | Number c -> Some c
        | UpperAtoZ c -> Some (Char.ToLower(c))
        | Symbol c -> Some c
        | _ -> None

    let private emitSelfClosingTag name attributes = 
        let result = Tag(true, toString name, toAttributes attributes) 
        name := []
        attributes := []
        result
     
    let private emitTag name attributes = 
        let result = Tag(false, toString name, toAttributes attributes) 
        name := []
        attributes := []
        result

    let private emitEndTag name = 
        let result = TagEnd(toString name) 
        name := []
        result

    let private emitContent content = 
        let result = Text(toString content)
        content := []
        result

    let tokenise (sr : #StreamReader) =
            let currentTag : char list ref = ref []
            let attributes : (char list ref * char list ref) list ref  = ref []
            let content : char list ref  = ref []
            let rec data inp =
                    match peek inp with
                    | '<' when (!content).Length > 0 -> emitContent content
                    | '<' -> 
                        pop inp; 
                        match peek inp with
                        | '/' -> pop inp; endTag inp
                        | '!' -> docType inp
                        | _ -> openTag inp
                    | '>' -> pop inp; emitTag currentTag attributes
                    | EndOfFile _ -> emitTag currentTag attributes
                    | other -> 
                        emit content inp
                        data inp
            and docType inp =
                match peek inp with
                | '>' -> pop inp; emitTag currentTag attributes
                | other -> emit currentTag inp; docType inp
            and openTag inp =
                match peek inp with
                | LetterDigit c -> emit currentTag inp; openTag inp
                | '!' ->  pop inp; openTag inp
                | '/'  -> pop inp; pop inp; emitSelfClosingTag currentTag attributes
                | Whitespace _ -> pop inp; attributeName inp
                | '>' -> 
                    pop inp;
                    if (toString currentTag) = "meta" 
                    then 
                        let result = emitTag currentTag attributes
                        currentTag := []
                        result
                    else emitTag currentTag attributes
                | EndOfFile _ -> data inp
                | other -> pop inp; data inp
            and endTag inp =
                match peek inp with
                | LetterDigit c -> emit currentTag inp; endTag inp
                | '>' -> pop inp; emitEndTag currentTag
                | other -> pop inp; emitEndTag currentTag
            and attributeName inp =
                match peek inp with
                | '\'' | '"'  -> pop inp; attributeName inp
                | LetterDigit c -> emitCurrentAttrName attributes inp; attributeName inp;
                | '-' | '/' | ':' -> emitCurrentAttrName attributes inp; attributeName inp;
                | '=' -> pop inp; attributeValue inp;
                | other -> openTag inp
            and attributeValue inp =
                match peek inp with
                | '\'' | '"'  -> pop inp; attributeValue inp
                | '>' -> openTag inp
                | Whitespace _ -> 
                    if currentAttrName attributes = "style"
                    then emitCurrentAttrValue attributes inp; attributeValue inp;
                    else startNewAttribute attributes; openTag inp
                | LetterDigitSymbol c -> emitCurrentAttrValue attributes inp; attributeValue inp
                | other -> emitCurrentAttrValue attributes inp; attributeValue inp;
            
            seq {
               while sr.EndOfStream |> not do
                   yield data sr
            }

    module Dom =

        type HAttribute =
            | HAttribute of (string * string)
            with
                member x.Name = 
                    match x with
                    | HAttribute(name,_) -> name
                member x.Value = 
                    match x with
                    | HAttribute(_,value) -> value

        type HElement =
            | HDocument of HElement list
            | HElement of string * HAttribute list * HElement list
            | HContent of string
            with
                member x.Name 
                    with get() =
                        match x with
                        | HElement(name, _, _) -> name.ToLower()
                        | _ -> String.Empty
                member x.Value 
                    with get() =
                       let rec getValues = function
                           | HDocument(content)
                           | HElement(_, _, content) -> List.collect (getValues) content
                           | HContent c -> [c.Trim()]
                       getValues x
                member x.Children
                    with get() =
                            match x with
                            | HElement(_, _, children) -> children
                            | _ -> []
                member x.TryGetAttribute(name : string) =
                    match x with
                    | HDocument(_) -> None
                    | HElement(_,attr,_) ->
                        attr |> List.tryFind (fun a -> a.Name.ToLowerInvariant() = (name.ToLowerInvariant()))
                    | HContent _ -> None
                member x.HasAttribute(name, value : string) =
                    x.TryGetAttribute(name)
                    |> function 
                       | Some(attr) ->  attr.Value.ToLowerInvariant() = (value.ToLowerInvariant())
                       | None -> false
                        
        
        let parse (sr : #StreamReader) =
            let rec parse' elements tokens =
                match tokens with
                | Tag(true, name, attributes) :: rest ->
                   let e = HElement(name, attributes |> List.map HAttribute, [])
                   parse' (e :: elements) rest
                | Tag(false, name, attributes) :: rest ->
                    let tokens, content = parse' [] rest
                    let e = HElement(name, attributes |> List.map HAttribute, content)
                    parse' (e :: elements) tokens
                | TagEnd(name) :: rest -> rest, (elements |> List.rev)
                | Text(cont) :: rest -> parse' (HContent(cont.Trim()) :: elements) rest
                | [] -> [], (elements |> List.rev)
            HDocument(
                     tokenise sr
                     |> (Seq.toList >> parse' []) 
                     |> snd)

        let rec descendantsBy f = function
            | HDocument(elements)
            | HElement(_, _, elements) ->
                seq {
                    for element in elements do
                        if f element then yield element
                        yield! descendantsBy f element
                }
            | HContent _ -> Seq.empty
        
        let descendantsByName (name : string) =
            descendantsBy (fun e -> 
                e.Name.ToLowerInvariant() = (name.ToLowerInvariant())
                )

        let descendants = descendantsBy (fun _ -> true)

        let first f = descendants >> Seq.find f

        let values elems = Seq.map (fun (e:HElement) -> e.Value) elems

        let toXHtml (writer:TextWriter) (element:HElement) =
            let createXmlWriter(baseWriter:TextWriter) =
                let s = new System.Xml.XmlWriterSettings(Indent = false,
                                                         OmitXmlDeclaration = true, 
                                                         ConformanceLevel = System.Xml.ConformanceLevel.Auto)
                XmlWriter.Create(baseWriter, s)
            
            let rec writeElement (writer:XmlWriter) = function
                | HDocument(elems) ->
                    for elem in elems do 
                        writeElement writer elem
                | HContent(c) -> writer.WriteValue(c)
                | HElement(name, attrs, elems) ->
                    writer.WriteStartElement(name)
                    for attr in attrs do
                        match attr with
                        | HAttribute(key,value) -> writer.WriteAttributeString(key, value)
                    for elem in elems do 
                        writeElement writer elem
                    writer.WriteEndElement()

            use writer = createXmlWriter(writer)
            writeElement writer element
                
                 

