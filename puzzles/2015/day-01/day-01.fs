module Puzzles.Year2015.Day01

open System.IO

let private getInstructions =
    let fileContent = File.ReadAllText("./puzzles/2015/day-01/input.txt")
    Seq.toList fileContent

let private getDirection input =
    match input with
    | '(' -> 1
    | ')' -> -1
    | _ -> 0

// Part 1
let findFloor =
    let instructions = getInstructions
    instructions |> List.map getDirection |> List.sum

// Part 2
let findWhenInBasement =
    let instructions = getInstructions

    instructions
    |> List.scan (fun total current -> total + (getDirection current)) 0
    |> List.findIndex (fun floor -> floor < 0)
