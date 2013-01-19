namespace FSharp.Enterprise

open System
open Microsoft.FSharp.Reflection
open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Net
open System.Text
open System.Threading.Tasks
open System.Web
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Converters

module Serialisation =

    type ISerialiser<'output> =
        abstract member Serialise : 'a -> 'output
        abstract member Deserialise : 'output -> 'a

    type private UnionTypeConverter() =
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
    
    module Json = 
       
        let settings = 
                let jss = new JsonSerializerSettings()
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

        let ByteSerialiser =
            { new ISerialiser<byte[]> with
                member x.Serialise(payload) = toByteArray payload
                member x.Deserialise(body) = ofByteArray body }

        let JObjectSerialiser =
            { new ISerialiser<JObject> with
                member x.Serialise(payload) = JObject.FromObject payload
                member x.Deserialise(body) =  body.ToObject<_>() }

        let StringSerialiser =
            { new ISerialiser<string> with
                member x.Serialise(payload) = ofObject payload
                member x.Deserialise(body) = toObject body }



