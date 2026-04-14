// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace MiniCrawlerTests

open System
open NUnit.Framework
open FsUnit
open System.Net.Http
open MiniCrawler

[<TestFixture>]
type ``extractLinks tests`` () =

    [<Test>]
    member _.``extractLinks returns empty array when there are no links`` () =
        let html = "<html><body><p>No links here</p></body></html>"

        let result = extractLinks html

        result |> should equal [||]

    [<Test>]
    member _.``extractLinks finds one http link in required format`` () =
        let html = "<a href=\"http://example.com\">"

        let result = extractLinks html

        result |> should equal [| "http://example.com" |]

    [<Test>]
    member _.``extractLinks finds multiple links`` () =
        let html =
            "<a href=\"http://a.com\">" +
            "<div>text</div>" +
            "<a href=\"http://b.com\">"

        let result = extractLinks html

        result |> should equal [| "http://a.com"; "http://b.com" |]

    [<Test>]
    member _.``extractLinks removes duplicate links`` () =
        let html =
            "<a href=\"http://a.com\">" +
            "<a href=\"http://a.com\">"

        let result = extractLinks html

        result |> should equal [| "http://a.com" |]

    [<Test>]
    member _.``extractLinks ignores https links`` () =
        let html = "<a href=\"https://example.com\">"

        let result = extractLinks html

        result |> should equal [||]

    [<Test>]
    member _.``extractLinks ignores links not matching exact required form`` () =
        let html =
            "<a class=\"x\" href=\"http://a.com\">" +
            "<a href='http://b.com'>" +
            "<a    href=\"http://c.com\">"

        let result = extractLinks html

        result |> should equal [||]


[<TestFixture>]
type ``crawlAsync tests`` () =

    [<Test>]
    member _.``crawlAsync returns lengths for all successfully downloaded linked pages`` () =
        let fakeDownload url =
            async {
                match url with
                | "http://root.com" ->
                    return
                        "<a href=\"http://a.com\">" +
                        "<a href=\"http://b.com\">"
                | "http://a.com" ->
                    return "hello"
                | "http://b.com" ->
                    return "abcdef"
                | _ ->
                    return failwith "Unexpected URL"
            }

        let result =
            crawlAsync fakeDownload "http://root.com"
            |> Async.RunSynchronously

        result |> should equal [| ("http://a.com", 5); ("http://b.com", 6) |]

    [<Test>]
    member _.``crawlAsync skips pages that fail to download`` () =
        let fakeDownload url =
            async {
                match url with
                | "http://root.com" ->
                    return
                        "<a href=\"http://a.com\">" +
                        "<a href=\"http://b.com\">"
                | "http://a.com" ->
                    return "hello"
                | "http://b.com" ->
                    return failwith "Download failed"
                | _ ->
                    return failwith "Unexpected URL"
            }

        let result =
            crawlAsync fakeDownload "http://root.com"
            |> Async.RunSynchronously

        result |> should equal [| ("http://a.com", 5) |]

    [<Test>]
    member _.``crawlAsync returns empty array when source page has no valid links`` () =
        let fakeDownload url =
            async {
                match url with
                | "http://root.com" ->
                    return "<html><body>No links</body></html>"
                | _ ->
                    return failwith "Unexpected URL"
            }

        let result =
            crawlAsync fakeDownload "http://root.com"
            |> Async.RunSynchronously

        result |> should equal [||]

    [<Test>]
    member _.``crawlAsync downloads each distinct link only once`` () =
        let requested = System.Collections.Generic.List<string>()

        let fakeDownload url =
            async {
                requested.Add(url)

                match url with
                | "http://root.com" ->
                    return
                        "<a href=\"http://a.com\">" +
                        "<a href=\"http://a.com\">"
                | "http://a.com" ->
                    return "hello"
                | _ ->
                    return failwith "Unexpected URL"
            }

        let result =
            crawlAsync fakeDownload "http://root.com"
            |> Async.RunSynchronously

        result |> should equal [| ("http://a.com", 5) |]
        requested |> Seq.toList |> should equal [ "http://root.com"; "http://a.com" ]