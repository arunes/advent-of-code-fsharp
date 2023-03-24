module Puzzles.Year2015.Day10

open System.Text.RegularExpressions

(* this takes very long time to calculate
let loopChars acc current =
    let (last, occ, result) = acc

    if last <> current && last <> ' ' then
        (current, 1, $"{result}{occ}{last}")
    else
        (current, occ + 1, result)

let loopResults acc current =
    let charList = acc |> Seq.toList
    let (last, occ, result) = charList |> List.fold loopChars (' ', 0, "")
    let final = $"{result}{occ}{last}"
    printfn "#%d input length: %d" current charList.Length
    final

let lookAndSay times = [ 1..times ] |> List.fold loopResults "3113322113"
*)

let private getResult input =
    Regex.Matches(input, @"(\d)\1*")
    |> Seq.cast
    |> Seq.map (fun (rm: Match) -> $"{rm.Length}{rm.Value.Substring(0, 1)}")
    |> String.concat ""

// Part 1 & 2
let lookAndSay times =
    [ 1..times ]
    |> List.fold (fun acc _ -> getResult acc) "3113322113"
    |> String.length
