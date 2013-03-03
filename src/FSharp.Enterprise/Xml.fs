namespace FSharp.Enterprise

module Xml = 
    
    open System.IO
    open System.Runtime.Serialization
    open System.Xml
    open System.Xml.Linq

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
    
    let toStream<'a>(stream:Stream, obj:'a) =
        let writer = XmlDictionaryWriter.CreateTextWriter(stream)
        try
            let dcs = new DataContractSerializer(typeof<'a>)
            dcs.WriteObject(writer, obj)
        finally
            writer.Close()
    
    let ofStream<'a>(stream:Stream) =
        let reader = XmlReader.Create(stream)
        try        
            let dcs = new System.Runtime.Serialization.DataContractSerializer(typeof<'a>)
            dcs.ReadObject(reader, true) :?> 'a
        finally
            reader.Close()

