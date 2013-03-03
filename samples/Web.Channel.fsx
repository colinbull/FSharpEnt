

(**
# F# Enterprise Web - Channels

`FSharp.Enterprise.Web.dll` provides an implementation of a channel to provide communication over http. 
the `HttpChannel<'a>` wraps the `System.Net.Http.WebClient` 

*)

#r @"..\bin\FSharp.Enterprise.dll"
#r @"..\bin\FSharp.Enterprise.Web.dll"
#r @"..\packages\Newtonsoft.Json.4.5.11\lib\net40\Newtonsoft.Json.dll"
#r @"System.Net.Http.dll" 
#r @"System.Net.Http.Formatting.dll"
open FSharp.Enterprise
open FSharp.Enterprise.Channel
open System.Net.Http.Formatting

(**
To use the Http channel we need to provide a (MediaTypeFormatter)[http://msdn.microsoft.com/en-us/library/system.net.http.formatting.mediatypeformatter(v=vs.108).aspx]
*)

let formatter = new JsonMediaTypeFormatter()
formatter.SerializerSettings <- Serialisation.Json.settings

(**
    Here we use the default Json formatter but we override the serialiser settings with ones provided by the core library (see. here)[Core.Json.html]

*)

let http = new Http.HttpChannel<_>("Example", new System.Net.Http.Formatting.JsonMediaTypeFormatter())



