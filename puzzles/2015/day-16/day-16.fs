module Puzzles.Year2015.Day16

open System.IO
open System.Text.RegularExpressions

type Aunt = { No: int; Things: Map<string, int> }

let private getAunts () : list<Aunt> =
    "./puzzles/2015/day-16/input.txt"
    |> File.ReadAllLines
    |> Seq.map (fun line -> Regex.Match(line, @"Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)"))
    |> Seq.cast
    |> Seq.map (fun (rm: Match) ->
        { No = int rm.Groups[1].Value
          Things =
            Map.empty
                .Add(rm.Groups[2].Value, int rm.Groups[3].Value)
                .Add(rm.Groups[4].Value, int rm.Groups[5].Value)
                .Add(rm.Groups[6].Value, int rm.Groups[7].Value) })
    |> Seq.toList

let private clues =
    Map.empty
        .Add("children", 3)
        .Add("cats", 7)
        .Add("samoyeds", 2)
        .Add("pomeranians", 3)
        .Add("akitas", 0)
        .Add("vizslas", 0)
        .Add("goldfish", 5)
        .Add("trees", 3)
        .Add("cars", 2)
        .Add("perfumes", 1)

let private compareNotExact clueName auntValue =
    let clueValue = clues[clueName]

    match clueName with
    | "cats"
    | "trees" -> clueValue < auntValue
    | "pomeranians"
    | "goldfish" -> clueValue > auntValue
    | _ -> clueValue = auntValue

// Part 1
let findMyAunt () =
    let aunt =
        getAunts ()
        |> List.where (fun aunt ->
            let allFound =
                aunt.Things
                |> Map.toSeq
                |> Seq.forall (fun (name, value) -> clues.ContainsKey(name) && clues[name] = value)

            allFound)
        |> List.head

    aunt.No

// Part 2
let findMyAuntNotExact () =
    let aunt =
        getAunts ()
        |> List.where (fun aunt ->
            let allFound =
                aunt.Things
                |> Map.toSeq
                |> Seq.forall (fun (name, value) -> clues.ContainsKey(name) && (compareNotExact name value))

            allFound)
        |> List.head

    aunt.No
