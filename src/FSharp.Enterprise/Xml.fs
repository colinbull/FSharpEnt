namespace FSharp.Enterprise

module Xml = 
    
    open System
    open System.IO
    open System.Runtime.Serialization
    open System.Xml
    open System.Xml.Linq
    open System.Reflection
    open System.Runtime.Serialization
    open System.Xml

    let (|ROOT|_|) name (elem : XElement) =  if elem.Name.LocalName = name then Some(elem) else None
    
    let (|ELEMENTS|) (elem : XElement) = elem.Elements()
    
    let (|ELEMENT|_|) name (elem : seq<XElement>) = 
        match elem |> Seq.filter( fun e -> e.Name.LocalName = name) |> Seq.toList with
        | h :: t -> Some(h.Value) 
        | [] -> None
        
    let (|ATTRIBUTE|_|) name (elem : XElement) = 
         match elem.Attribute(XName.Get(name, "")) with 
         | null -> None
         | attr -> Some(attr.Value)

    let private createSerialiser(t:Type) =
        new DataContractSerializer(t)

    let toStream (payload:'a) (stream:Stream) =
        let writer = XmlDictionaryWriter.CreateTextWriter(stream)
        try
            let dcs = createSerialiser (typedefof<'a>)
            dcs.WriteObject(writer, payload)
        finally
            writer.Close()
    
    let ofStream<'a>(stream:Stream) =
        let reader = XmlReader.Create(stream)
        try        
            let dcs = createSerialiser (typedefof<'a>)
            unbox<'a> (dcs.ReadObject(reader, true))
        finally
            reader.Close()

    let toByteArray (payload:'a) =
        use ms = new MemoryStream()
        toStream payload ms
        ms.ToArray()

    let ofByteArray (bytes:byte[]) =
        use ms = new MemoryStream(bytes)
        ofStream ms

    let toString (payload:'a) =
        use ms = new MemoryStream()
        toStream payload ms
        ms.Position <- 0L
        use sr = new StreamReader(ms)
        sr.ReadToEnd()

    let ofString xml = 
        let reader = XmlReader.Create(new StringReader(xml), new XmlReaderSettings())
        try        
            let dcs = createSerialiser (typedefof<'a>)
            unbox<'a> (dcs.ReadObject(reader, true))
        finally
            reader.Close()

