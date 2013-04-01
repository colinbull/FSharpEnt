namespace FSharp.Enterprise

module Json = 
    
    open System
    open System.Collections.Generic
    open System.Reflection
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

    type MapTypeConverter() =
        inherit JsonConverter()
    
        let doRead (reader:JsonReader) = 
            reader.Read() |> ignore
    
        let readKeyValuePair (serializer:JsonSerializer) 
                             (argTypes:Type []) 
                             (reader:JsonReader) =
            doRead reader // "key"
            doRead reader // value
            let key = serializer.Deserialize(reader, argTypes.[0])
            doRead reader // "value"
            doRead reader // value
            let value = serializer.Deserialize(reader, argTypes.[1])
            doRead reader // }
            FSharpValue.MakeTuple([|key;value|], FSharpType.MakeTupleType(argTypes))
    
        let readArray elementReaderF (reader:JsonReader) =
            doRead reader // [
            if reader.TokenType = JsonToken.StartArray then
                [|
                    while reader.TokenType <> JsonToken.EndArray do
                        doRead reader // {
                        if reader.TokenType = JsonToken.StartObject then
                            let element = elementReaderF reader
                            yield element
                |]
            else
                Array.empty
    
        let writeKeyValuePair (serializer:JsonSerializer) (writer:JsonWriter) kv =
            let kvType = kv.GetType()
            let k = kvType.GetProperty("Key").GetValue(kv, null)
            let v = kvType.GetProperty("Value").GetValue(kv, null)
            writer.WriteStartObject()
            writer.WritePropertyName("key")
            serializer.Serialize(writer,k)
            writer.WritePropertyName("value")
            serializer.Serialize(writer,v)
            writer.WriteEndObject()
    
        let writeArray elementWriterF 
                       (writer:JsonWriter) 
                       (kvs:System.Collections.IEnumerable) =
            writer.WriteStartArray()
            for kv in kvs do
                elementWriterF writer kv
            writer.WriteEndArray()
    
        override x.CanConvert(typ:Type) =
            typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Map<_,_>>
    
        override x.WriteJson(writer:JsonWriter, value:obj, serializer:JsonSerializer) =
            if value <> null then
                let valueType = value.GetType()
                if valueType.IsGenericType then
                    let baseType = valueType.GetGenericTypeDefinition()
                    if baseType = typedefof<Map<_,_>> then
                        let kvs = value :?> System.Collections.IEnumerable
                        writer.WriteStartObject()
                        writer.WritePropertyName("pairs")
                        writeArray (writeKeyValuePair serializer) writer kvs
                        writer.WriteEndObject()   
    
        override x.ReadJson(reader:JsonReader, 
                            objectType:Type, 
                            existingValue:obj, 
                            serializer:JsonSerializer) =
            let argTypes = objectType.GetGenericArguments()
            let tupleType = FSharpType.MakeTupleType(argTypes)
            let constructedIEnumerableType = 
                typedefof<IEnumerable<_>>
                    .GetGenericTypeDefinition()
                    .MakeGenericType(tupleType)
            if reader.TokenType <> JsonToken.Null then
                doRead reader // "pairs"
                let kvs = readArray (readKeyValuePair serializer argTypes) reader
                doRead reader // }
                let kvsCopy = System.Array.CreateInstance(tupleType, kvs.Length)
                System.Array.Copy(kvs, kvsCopy, kvs.Length)
                let methodInfo = 
                    objectType.GetMethod("Create", 
                        BindingFlags.Static ||| BindingFlags.NonPublic,
                        null, [|constructedIEnumerableType|], null)
                methodInfo.Invoke(null, [|kvsCopy|])
            else
                box Map.empty
    
    