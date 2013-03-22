namespace FSharp.Enterprise.Tests

open System
open System.IO
open System.Text
open NUnit.Framework
open FsUnit
open FSharp.Enterprise
open FSharp.Enterprise.Html

[<TestFixture>]
type ``Given html``() =
    
    [<Test>]
    member t.``I should be able to tokenise an empty string``() =
        let str =
            """"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = []
        actual |> should equal expected
    
    [<Test>]
    member t.``I should not fail when EOF during an open tag``() =
        let str =
            """<br"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = [Tag (false,"br",[])]
        actual |> should equal expected

    [<Test>]
    member t.``I should be able parse a self closing tag``() =
        let str =
            """<br/>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = [Tag (true,"br",[])]
        actual |> should equal expected

    [<Test>]
    member t.``I should be able parse a well formed tag open/close tag``() =
        let str =
            """<br></br>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = [Tag (false,"br",[]); TagEnd "br"]
        actual |> should equal expected
    
    [<Test>]
    member t.``I should be able parse a simple open/close with unquoted attributes``() =
        let str =
            """<br value=1></br>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = [Tag (false,"br",[("value","1")]); TagEnd "br"]
        actual |> should equal expected

    [<Test>]
    member t.``I should be able parse a simple open/close with single quoted attributes``() =
        let str =
            """<br value='1'></br>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = [Tag (false,"br",[("value","1")]); TagEnd "br"]
        actual |> should equal expected

    [<Test>]
    member t.``I should be able parse a simple open/close with double quoted attributes``() =
        let str =
            """<br value="1"></br>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = [Tag (false,"br",[("value","1")]); TagEnd "br"]
        actual |> should equal expected

    [<Test>]
    member t.``I should be able parse a well formed tag open/close tag with content``() =
        let str =
            """<br>Hello</br>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = [Tag (false,"br",[]); Text "Hello"; TagEnd "br"]
        actual |> should equal expected

    [<Test>]
    member t.``I should be able parse a well formed tag open/close tag with multiple attributes``() =
        let str =
            """<br value=1 x='5'></br>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = [Tag (false,"br",[("x","5");("value","1")]); TagEnd "br"]
        actual |> should equal expected

    [<Test>]
    member t.``I should be able parse a well formed tag``() =
        let str =
            """<br>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = [Tag (false,"br",[])]
        actual |> should equal expected

    [<Test>]
    member t.``I should be able to tokenise well-formed html``() =
        let str =
            """<html><body><p align="right">Begin &amp; back</p></body></html>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = 
            [
               Tag (false,"html",[]); 
               Tag (false,"body",[]);
               Tag (false,"p",[("align","right")]); 
               Text "Begin &amp; back";
               TagEnd "p"; 
               TagEnd "body"; 
               TagEnd "html";
            ]
        actual |> should equal expected
    
    [<Test>]
    member t.``I should be able to tokenise mel-formed html``() =
        let str =
            """<html><body><p align=right"" dir='rtl'> Begin &amp; back </p>" "</body></html>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = 
            [
             Tag (false,"html",[]); 
             Tag (false,"body",[]);
             Tag (false,"p",[("dir","rtl"); ("align","right")]);
             Text " Begin &amp; back "; 
             TagEnd "p"; 
             Text "\" \""; 
             TagEnd "body";
             TagEnd "html";
            ]
        actual |> should equal expected

    [<Test>]
    member t.``I should be able to tokenise well-formed html with html4 doctype``() =
        let str =
            """<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"><html><body><p align="right">Begin &amp; back</p></body></html>"""
        use ms = new MemoryStream(Encoding.UTF8.GetBytes(str))
        use sr = new StreamReader(ms)
        let actual = Html.tokenise sr |> Seq.toList
        let expected = 
            [
               Tag (false, "!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"", [])
               Tag (false,"html",[]); 
               Tag (false,"body",[]);
               Tag (false,"p",[("align","right")]); 
               Text "Begin &amp; back";
               TagEnd "p"; 
               TagEnd "body"; 
               TagEnd "html";
            ]
        actual |> should equal expected

