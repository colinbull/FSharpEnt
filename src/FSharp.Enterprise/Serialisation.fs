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
    open FSharp.Enterprise
    
    module Raw = 

        let toByteArray (payload : 'a) =
            payload.ToString() |> Encoding.Default.GetBytes
    
        let ofByteArray<'a> (bytes : byte[]) =
            box(Encoding.Default.GetString(bytes)) :?> 'a
    
        let toString payload =  payload.ToString()
    
        let ofString<'a> (js:string) = (box js) :?> 'a

        let toStream payload (stream : Stream) = 
            use sw = new StreamWriter(stream)    
            sw.WriteLine(payload.ToString())
            stream

        let ofStream (stream : Stream) = 
            let sr = new StreamReader(stream)
            box(sr.ReadToEnd()) :?> 'a

    module Json = 
        let settings = 
                let jss = new JsonSerializerSettings()
                jss.Converters.Add(new Json.UnionTypeConverter())
                jss.Converters.Add(new Json.MapTypeConverter())
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
            

        let ofStream<'a> (stream : Stream) = 
            let ser = JsonSerializer.Create(settings)
            let sr = new StreamReader(stream)
            let reader = new JsonTextReader(sr)
            unbox<'a> (ser.Deserialize(reader, typeof<'a>))
        
    module Xml = 
        
        open System.Reflection
        open System.Runtime.Serialization
        open System.Xml

        let private getUnionKnownTypes (t:Type) =
            t.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic)
            |> Array.filter FSharpType.IsUnion

        let private createSerialiser(t:Type) =
            let knownTypes = 
                if FSharpType.IsUnion(t)
                then getUnionKnownTypes t
                else Array.empty

            new DataContractSerializer(t, knownTypes)

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

    module Binary = 
        
        open System.Runtime.Serialization
        open System.Runtime.Serialization.Formatters.Binary

        let toByteArray (payload:'a) =
            use ms = new MemoryStream()
            let bin = new BinaryFormatter()
            bin.Serialize(ms, payload)
            ms.ToArray()

        let ofByteArray (bytes:byte[]) =
            use ms = new MemoryStream(bytes)
            let bin = new BinaryFormatter()
            unbox<_> (bin.Deserialize(ms))


