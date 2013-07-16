namespace FSharp.Enterprise.Tests

open System
open System.IO
open System.Text
open NUnit.Framework
open FsUnit
open FSharp.Enterprise.Html
open FSharp.Enterprise.Html.Dom

[<TestFixture>]
type ``Given parsed Html``() =
    
    let read (str:string) =
        let ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        new StreamReader(ms)

    [<Test>]
    member x.``I should be able to find descendants a tag by name``() =
        let html = """<html><body><div id="foo">Foo</div></body></html>"""
        let actual = 
            read html
            |> parse
            |> descendantsBy (fun e -> e.Name = "div")
            |> Seq.head
        let expected = 
            Dom.HElement("div", [HAttribute("id", "foo")], [HContent("Foo")])
        actual |> should equal expected

    [<Test>]
    member x.``I should be able to find an element with an id``() =
        let html = """<html><body><div id="foo">Foo</div></body></html>"""
        let actual = 
            read html
            |> parse
            |> descendantsBy (fun e -> e.HasAttribute("id", "foo"))
            |> Seq.toList
        let expected = 
            [
                HElement("div", [HAttribute("id", "foo")], [HContent("Foo")])
            ]
        actual |> should equal expected

    [<Test>]
    member x.``I should be able to get all descendants of a document``() =
        let html = """<html><body><div id="foo">Foo</div></body></html>"""
        let actual = 
            read html
            |> parse
            |> descendants
            |> Seq.toList
        let expected = 
            [
                HElement("html", [], [HElement("body", [], [HElement("div", [HAttribute("id", "foo")], [HContent("Foo")])])]) 
                HElement("body", [], [HElement("div", [HAttribute("id", "foo")], [HContent("Foo")])]) 
                HElement("div", [HAttribute("id", "foo")], [HContent("Foo")])
                HContent("Foo")
            ]
        actual |> should equal expected

    [<Test>]
    member x.``I can write out xhtml``() =
        let html = """<html><body><div id="foo">Foo</div></body></html>"""
        let actual = 
            let sb = new StringBuilder()
            use sr = new StringWriter(sb)
            read html
            |> parse
            |> write sr
            sb.ToString()
        actual |> should equal html

