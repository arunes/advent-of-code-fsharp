module Puzzles.Year2015.Day08

open System.Text.RegularExpressions
open System.IO

let getLines = File.ReadAllLines("./puzzles/2015/day-08/input.txt")

let clear (input: string) =
    let withoutQuotes = input.Substring(1, input.Length - 2)
    Regex.Replace(withoutQuotes.Replace("\\\"", "1").Replace("\\\\", "1"), @"\\x[0-9a-f]{2}", "1")

let encode (input: string) =
    let withQuotes = "\"" + input.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""
    withQuotes

// Part 1
let diffFromCleaned =
    getLines
    |> Seq.map (fun line -> (line, clear line))
    |> Seq.sumBy (fun (input, cleaned) -> input.Length - cleaned.Length)

// Part 2
let diffFromEncoded =
    getLines
    |> Seq.map (fun line -> (line, encode line))
    |> Seq.sumBy (fun (input, encoded) -> encoded.Length - input.Length)
