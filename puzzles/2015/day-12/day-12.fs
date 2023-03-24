module Puzzles.Year2015.Day12

open System.IO
open FSharp.Data
open System.Text.RegularExpressions

// Part 1
let sumOfAllNumbers =
    let json = "./puzzles/2015/day-12/input.txt" |> File.ReadAllText

    Regex.Matches(json, "\"[^\"]*\"|(-\d+|\d+)")
    |> Seq.cast
    |> Seq.where (fun (rm: Match) -> rm.Groups[1].Success)
    |> Seq.sumBy (fun (rm: Match) -> int rm.Groups[1].Value)

// Part 2
let sumOfNumbersThatAreNotRed =
    let rec sumNodes (nodes: list<(string * JsonValue)>, sum: int) =
        match nodes with
        | [] -> sum
        | (key, node) :: tail ->
            let childList = node.Properties() |> Array.toList

            let anyRed =
                childList
                |> List.where (fun (k, n) -> n.Properties().Length = 0 && n.AsArray().Length = 0)
                |> List.tryFind (fun (k, n) -> n.AsString() = "red")

            if anyRed.IsSome then
                sumNodes (tail, sum)
            elif childList.Length = 0 then
                try
                    let value = node.AsInteger()
                    sumNodes (tail, sum + value)
                with _ ->
                    let arr = node.AsArray() |> Array.map (fun jv -> ("", jv)) |> Array.toList
                    let arraySum = sumNodes (arr, 0)
                    sumNodes (tail, sum + arraySum)
            else
                let childSum = if childList.Length = 0 then 0 else sumNodes (childList, 0)
                sumNodes (tail, sum + childSum)


    let node = "./puzzles/2015/day-12/input.txt" |> File.ReadAllText |> JsonValue.Parse
    let nodes = node.Properties() |> Array.toList
    sumNodes (nodes, 0)
