namespace FSharp.Enterprise

module Json = 
    
    open System
    open Microsoft.FSharp.Reflection
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open Newtonsoft.Json.Converters
    
    type UnionTypeConverter() =
        inherit JsonConverter()
    
        let doRead pos (reader: JsonReader) = 
            reader.Read() |> ignore
    
        override x.CanConvert(typ:Type) =
            let result = 
                ((typ.GetInterface(typeof<System.Collections.IEnumerable>.FullName) = null) 
                && FSharpType.IsUnion typ)
            result
    
        override x.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
            let t = value.GetType()
            let write (name : string) (fields : obj []) = 
                writer.WriteStartObject()
                writer.WritePropertyName("case")
                writer.WriteValue(name)
                if fields.Length > 0 || serializer.NullValueHandling = NullValueHandling.Include
                then  
                    writer.WritePropertyName("values")
                    serializer.Serialize(writer, fields)
                writer.WriteEndObject()   
    
            let (info, fields) = FSharpValue.GetUnionFields(value, t)
            write info.Name fields
    
        override x.ReadJson(reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =      
             let cases = FSharpType.GetUnionCases(objectType)
             
             let createUnion (reader:JsonReader) = 
                 doRead "1" reader
                 doRead "2" reader
                 let case = cases |> Array.find(fun x -> x.Name = if reader.Value = null then "None" else reader.Value.ToString())
                 doRead "3" reader
                 let fields =
                    if reader.Value <> null
                    then
                        doRead "4" reader
                        doRead "5" reader
                        [| 
                               for field in case.GetFields() do
                                   let result = serializer.Deserialize(reader, field.PropertyType)
                                   reader.Read() |> ignore
                                   yield result
                         |]
                    else [||]
                 FSharpValue.MakeUnion(case, fields)
    
             if reader.TokenType <> JsonToken.Null  
             then 
                let result = createUnion reader
                while reader.TokenType <> JsonToken.EndObject do
                    doRead "6" reader         
                result
             else
                FSharpValue.MakeUnion(cases.[0], [||])

