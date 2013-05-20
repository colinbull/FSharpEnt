namespace FSharp.Enterprise

open System.IO
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary

module Binary = 

    let toByteArray (payload:'a) =
        use ms = new MemoryStream()
        let bin = new BinaryFormatter()
        bin.Serialize(ms, payload)
        ms.ToArray()

    let ofByteArray (bytes:byte[]) =
        use ms = new MemoryStream(bytes)
        let bin = new BinaryFormatter()
        unbox<_> (bin.Deserialize(ms))


