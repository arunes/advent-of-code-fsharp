module Puzzles.Year2015.Day05

open System.IO

let private vowels = [ 'a'; 'e'; 'i'; 'o'; 'u' ]
let private badCombinations = [ "ab"; "cd"; "pq"; "xy" ]

let private getListOfStrings = File.ReadAllLines("./puzzles/2015/day-05/input.txt")

let private isNice (input: string) =
    let anyBad = Seq.exists (fun (comb: string) -> input.Contains(comb)) badCombinations

    let numberOfVowels =
        input
        |> Seq.toArray
        |> Seq.filter (fun chr -> Seq.contains chr vowels)
        |> Seq.length

    let anyPairs = input |> Seq.pairwise |> Seq.exists (fun (a, b) -> a = b)
    numberOfVowels >= 3 && anyPairs && not anyBad

let rec private anyRepeatingPair (list: list<char>) : bool =
    match list with
    | [] -> false // no more list - just return false
    | head :: second :: tail ->
        let pair = $"{head}{second}"
        let restOfTheInput = System.String.Concat tail

        if restOfTheInput.Contains(pair) then
            true
        else
            anyRepeatingPair (second :: tail) // calculate next one
    | _ -> false // less than 2 elements

let rec private anySkipMatchingLetters (list: list<char>) : bool =
    match list with
    | [] -> false // no more list - just return false
    | head :: second :: third :: tail ->
        if head = third then
            true
        else
            anySkipMatchingLetters (second :: third :: tail) // calculate next one
    | _ -> false // less than 2 elements

let private isNicePart2 input =
    let hasRepeatingPair = anyRepeatingPair (Seq.toList input)
    let hasAnySkipMatch = anySkipMatchingLetters (Seq.toList input)
    hasRepeatingPair && hasAnySkipMatch


// Part 1
let howManyAreNice = getListOfStrings |> Seq.filter isNice |> Seq.length

// Part 2
let howManyAreNicePart2 = getListOfStrings |> Seq.filter isNicePart2 |> Seq.length
