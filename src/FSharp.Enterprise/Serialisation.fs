namespace FSharp.Enterprise

module Serialisation =

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

    type ISerialiser<'output> =
        abstract member Serialise : 'a -> 'output
        abstract member Deserialise : 'output -> 'a

    module Json = 
        let settings = 
                let jss = new JsonSerializerSettings()
                jss.Converters.Add(new Json.UnionTypeConverter())
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
    
        let toString payload = 
            JsonConvert.SerializeObject(payload,Formatting.None, settings)
    
        let ofString<'a> js = 
            JsonConvert.DeserializeObject<'a>(js, settings)

        let toStream payload (stream : Stream) = 
            let ser = JsonSerializer.Create(settings)
            let sw = new StreamWriter(stream)
            let writer = new JsonTextWriter(sw)      
            ser.Serialize(writer, payload)
            stream

        let ofStream<'a> (stream : Stream) = 
            let ser = JsonSerializer.Create(settings)
            let sr = new StreamReader(stream)
            let reader = new JsonTextReader(sr)
            unbox<'a> (ser.Deserialize(reader, typeof<'a>))
        
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
                member x.Serialise(payload) = toString payload
                member x.Deserialise(body) = ofString body }

        let StreamSerialiser stream = 
            { new ISerialiser<Stream> with
                member x.Serialise(payload) = toStream payload stream
                member x.Deserialise(body) = ofStream body
            }



