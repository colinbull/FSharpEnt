(**
# F# Enterprise Web - Html

The HTML modules provides a way to tokenise, parse and process HTML documents. 
*)

#r @"..\bin\FSharp.Enterprise.dll"

open System
open System.IO
open System.Text
open FSharp.Enterprise

(**
## Tokenisation

Tokenisation is

> The process of breaking a stream of text up into words, phrases, symbols, or other meaningful elements called tokens. The list of tokens becomes input for further processing such as parsing or text mining. Tokenization is useful both in linguistics (where it is a form of text segmentation), and in computer science, where it forms part of lexical analysis.

according to [Wikipedia](http://en.wikipedia.org/wiki/Tokenization). The following example shows the results of processing a simple HTML string. 
*)

let str =
     """<html><body><p align="right">Begin &amp; start</p><p align="right">Begin &amp; end</p></body></html>"""

let tokenisedResult = 
    use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
    use sr = new StreamReader(ms)
    Html.tokenise sr |> Seq.toList
(** 
The result of the above code gives

    val tokenisedResult : Html.HtmlToken list =
      [Tag (false,"html",[]); Tag (false,"body",[]);
       Tag (false,"p",[("align", "right")]); Text "Begin &amp; start"; TagEnd "p";
       Tag (false,"p",[("align", "right")]); Text "Begin &amp; end"; TagEnd "p";
       TagEnd "body"; TagEnd "html"]

## Parsing

Parsing is

> The process of analysing a string of symbols, either in natural language or in computer languages, according to the rules of a formal grammar. 

again according to wikipedia. Parsing takes the sequence of tokens from the tokenising phase and builds a DOM which represents the underlying document.
Parsing in the `Html` module is handled by `Html.Dom.parse` function. This wraps the above tokenisation function. Running the same example as above
*)
let parsedResult = 
    use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
    use sr = new StreamReader(ms)
    Html.Dom.parse sr

(**
    val parsedResult : Html.Dom.HElement =
      HDocument
        [HElement
           ("html",[],
            [HElement
               ("body",[],
                [HElement
                   ("p",[HAttribute ("align", "right")],
                    [HContent "Begin &amp; start"]);
                 HElement
                   ("p",[HAttribute ("align", "right")],
                    [HContent "Begin &amp; end"])])])]
    
It is clear that this gives us something we can begin to manipulate and query.

##Querying the dom

Once we have our parsed tree, we might want to being to query it to extract information or indeed convert it into some completely other form. Since the dom tree is a 
recursive structure, standard F# modules like 'List' and 'Seq' will get us a long in processing this tree. But never the less some helper functions are still provided
*)

let allPElements =  Html.Dom.descendantsBy (fun elem -> elem.Name = "p") parsedResult |> Seq.toArray

(**
    val allPElements : Html.Dom.HElement [] =
      [|HElement
          ("p",[HAttribute ("align", "right")],[HContent "Begin &amp; start"]);
        HElement
          ("p",[HAttribute ("align", "right")],[HContent "Begin &amp; end"])|]

This walks the tree selecting all elements which statisfies the given predicate. 
*)

let firstPElement = Html.Dom.first (fun elem -> elem.Name = "p") parsedResult

(**

    val firstPElement : Html.Dom.HElement =
      HElement
        ("p",[HAttribute ("align", "right")],[HContent "Begin &amp; start"])

This walks the tree and selects the first element that matches the predicate. If no such elements are found a `KeyNotFoundException` will be thrown.    
*)

let allContent = allPElements |> Html.Dom.values |> Seq.toArray

(**
    val allContent : string list [] =
      [|["Begin &amp; start"]; ["Begin &amp; end"]|]

This walks the tree extracting the values from all of the `HContent` elements found within the tree.

##Writing out XHTML

Once we have a dom, we may wish to write it out. This can be achieved using the `toXHtml` function.
*)

let html = 
    let sb = new StringBuilder()
    use sr = new StringWriter(sb)
    Html.Dom.toXHtml sr parsedResult
    sb.ToString()

(**
    <html>
      <body>
        <p align="right">Begin &amp;amp; start</p>
        <p align="right">Begin &amp;amp; end</p>
      </body>
    </html>
*)

