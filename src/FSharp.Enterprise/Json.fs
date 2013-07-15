namespace FSharp.Enterprise

module Json = 
    
    open System
    open System.Collections.Generic
    open System.Reflection
    open Microsoft.FSharp.Reflection
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open Newtonsoft.Json.Converters
    open System.Text
    open System.IO
    
    type UnionTypeConverter() =
        inherit JsonConverter()
    
        let cache = new Dictionary<_,_>()
    
        let memoize f =
            fun x ->
                if cache.ContainsKey(x) then cache.[x]
                else let res = f x
                     cache.[x] <- res
                     res
    
        let doRead pos (reader: JsonReader) = 
            reader.Read() |> ignore 
    
        override x.CanConvert(t:Type) =
            let result = 
                memoize (fun (typ:Type) ->
                            ((typ.GetInterface(typeof<System.Collections.IEnumerable>.FullName) = null) 
                            && FSharpType.IsUnion typ)
                        )
            result t
    
        override x.WriteJson(writer: JsonWriter, value: obj, serializer: JsonSerializer) =
            let t = value.GetType()
            let write (name : string) (fields : obj []) = 
                writer.WriteStartObject()
                writer.WritePropertyName("case")
                writer.WriteValue(name)  
                writer.WritePropertyName("values")
                serializer.Serialize(writer, fields)
                writer.WriteEndObject()   
    
            let (info, fields) = FSharpValue.GetUnionFields(value, t)
            write info.Name fields
    
        override x.ReadJson(reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =      
             let cases = FSharpType.GetUnionCases(objectType)
             if reader.TokenType <> JsonToken.Null  
             then 
                doRead "1" reader
                doRead "2" reader
                let case = cases |> Array.find(fun x -> x.Name = if reader.Value = null then "None" else reader.Value.ToString())
                doRead "3" reader
                doRead "4" reader
                doRead "5" reader
                let fields =  [| 
                       for field in case.GetFields() do
                           let result = serializer.Deserialize(reader, field.PropertyType)
                           reader.Read() |> ignore
                           yield result
                 |] 
                let result = FSharpValue.MakeUnion(case, fields)
                while reader.TokenType <> JsonToken.EndObject do
                    doRead "6" reader         
                result
             else
                FSharpValue.MakeUnion(cases.[0], [||])
    
    type MapTypeConverter() =
        inherit JsonConverter()
            
        let doRead (reader:JsonReader) = 
            reader.Read() |> ignore
    
        let readKeyValuePair (serializer:JsonSerializer) (argTypes:Type []) (reader:JsonReader) =
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
    
        let writeArray elementWriterF (writer:JsonWriter) (kvs:System.Collections.IEnumerable) =
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
    
        override x.ReadJson(reader:JsonReader, objectType:Type, existingValue:obj, serializer:JsonSerializer) =
            let argTypes = objectType.GetGenericArguments()
            let tupleType = FSharpType.MakeTupleType(argTypes)
            let constructedIEnumerableType = typedefof<IEnumerable<_>>.GetGenericTypeDefinition().MakeGenericType(tupleType)
            if reader.TokenType <> JsonToken.Null then
                doRead reader // "pairs"
                let kvs = readArray (readKeyValuePair serializer argTypes) reader
                doRead reader // }
                let kvs' = System.Array.CreateInstance(tupleType, kvs.Length)
                System.Array.Copy(kvs, kvs', kvs.Length)
                let methodInfo = objectType.GetMethod("Create", BindingFlags.Static ||| BindingFlags.NonPublic, null, [|constructedIEnumerableType|], null)
                methodInfo.Invoke(null, [|kvs'|])
            else
                box Map.empty
    
    type TimeseriesTypeConverter() =
        inherit JsonConverter()
    
        let doRead (reader:JsonReader) = 
            reader.Read() |> ignore
    
        override x.CanConvert(typ:Type) =
            typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Timeseries.T<_>>
    
        override x.WriteJson(writer:JsonWriter, value:obj, serializer:JsonSerializer) =
            if value <> null then
                let valueType = value.GetType()
                if valueType.IsGenericType then
                    let baseType = valueType.GetGenericTypeDefinition()
                    if baseType = typedefof<Timeseries.T<_>> then
                        let startDate = valueType.GetProperty("StartDate").GetValue(value, null)
                        let interval = valueType.GetProperty("Interval").GetValue(value, null)
                        let values = valueType.GetMethod("ToArray").Invoke(value, null)
                        writer.WriteStartObject()
                        writer.WritePropertyName("startDate")
                        serializer.Serialize(writer,startDate)
                        writer.WritePropertyName("interval")
                        serializer.Serialize(writer,interval)
                        writer.WritePropertyName("values")
                        serializer.Serialize(writer,values)
                        writer.WriteEndObject()   
    
        override x.ReadJson(reader:JsonReader, objectType:Type, existingValue:obj, serializer:JsonSerializer) =
            let argTypes = objectType.GetGenericArguments()
            if reader.TokenType <> JsonToken.Null then
                doRead reader // startDate
                doRead reader // value
                let startDate = serializer.Deserialize(reader, typeof<DateTimeOffset>)
                doRead reader // interval
                doRead reader // value
                let interval = serializer.Deserialize(reader, typeof<TimeSpan>)
                doRead reader // values
                doRead reader // value
                let position = box 0
                let collectionType = typedefof<IEnumerable<_>>.MakeGenericType(argTypes.[0])
                let values = serializer.Deserialize(reader, collectionType)
                doRead reader
                Activator.CreateInstance(objectType, [|startDate;interval;position;values|])
            else
                null
    
    let settings = 
        let jss = new JsonSerializerSettings()
        jss.Converters.Add(TimeseriesTypeConverter())
        jss.Converters.Add(MapTypeConverter())
        jss.Converters.Add(new UnionTypeConverter())
        jss.Converters.Add(new IsoDateTimeConverter())
        jss.NullValueHandling <- NullValueHandling.Ignore
        jss
    
    let toJsonObject (vals : (string * obj) list) = 
        let jo = JObject()
        vals |> List.iter(fun (n,v) -> jo.Add(n, JToken.FromObject(v)))
        jo
    
    let toByteArray (payload : 'a) =
        JsonConvert.SerializeObject(payload,Formatting.None, settings)
        |> Encoding.Default.GetBytes
    
    let ofByteArray<'a> (bytes : byte[]) =
        JsonConvert.DeserializeObject<'a>(Encoding.Default.GetString(bytes), settings)
    
    let ofObject payload = 
        JsonConvert.SerializeObject(payload,Formatting.None, settings)
    
    let toObject<'a> js = 
        JsonConvert.DeserializeObject<'a>(js, settings)
    
    let deserialise js (typ:Type) = 
        JsonConvert.DeserializeObject(js, typ)
        
        