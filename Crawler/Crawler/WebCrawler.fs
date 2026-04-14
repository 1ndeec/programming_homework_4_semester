// Copyright (c) Murat Khamatyanov. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

module MiniCrawler

open System
open System.Text.RegularExpressions


/// <summary>
/// Extracts all links from the given HTML text that match the exact form
/// <a href="http://..."> and returns them without duplicates.
/// </summary>
let extractLinks (html: string) =
    let pattern = "<a href=\"(http://[^\"]*)\">"
    Regex.Matches(html, pattern)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> Seq.distinct
    |> Seq.toArray


/// <summary>
/// Downloads the given page, extracts all matching links from it,
/// and downloads those linked pages in parallel with their content lengths.
/// </summary>
let crawlAsync (downloadAsync: string -> Async<string>) (url: string) =
    async {
        let! mainPage = downloadAsync url
        let links = extractLinks mainPage

        let downloadOne link =
            async {
                try
                    let! content = downloadAsync link
                    return Some (link, content.Length)
                with
                | _ -> return None
            }

        let! results =
            links
            |> Array.map downloadOne
            |> Async.Parallel

        return results |> Array.choose id
    }