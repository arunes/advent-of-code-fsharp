module Puzzles.Year2015.Day06

open System.IO
open System.Text.RegularExpressions

let private changeLights grid direction lightFn =
    let regex =
        Regex(@"(.+) ([0-9]{1,3}),([0-9]{1,3}) through ([0-9]{1,3}),([0-9]{1,3})")

    let groups = regex.Match(direction).Groups
    let action = groups[1].Value
    let fromX = int groups[2].Value
    let fromY = int groups[3].Value
    let toX = int groups[4].Value
    let toY = int groups[5].Value

    grid
    |> Array2D.map (fun cell ->
        let (x, y, light) = cell

        if x >= fromX && x <= toX && y >= fromY && y <= toY then
            (x, y, (lightFn light action))
        else
            cell)

let rec private runThroughGrid grid directions lightFn =
    match directions with
    | [] -> grid
    | first :: tail -> runThroughGrid (changeLights grid first lightFn) tail lightFn

// Part 1
let turnOnAndOff =
    let grid = Array2D.init 1000 1000 (fun x y -> (x, y, false))
    let directions = File.ReadAllLines("./puzzles/2015/day-06/input.txt") |> Seq.toList

    let getLightStatus current action =
        match action with
        | "turn on" -> true
        | "turn off" -> false
        | "toggle" -> if current = true then false else true
        | _ -> false

    let result = runThroughGrid grid directions getLightStatus

    result
    |> Array2D.map (fun cell ->
        let (_, _, light) = cell
        if light = true then 1 else 0)
    |> Seq.cast<int>
    |> Seq.sum


// Part 2
let toggleBrightness =
    let grid = Array2D.init 1000 1000 (fun x y -> (x, y, 0))
    let directions = File.ReadAllLines("./puzzles/2015/day-06/input.txt") |> Seq.toList

    let getLightStatus current action =
        match action with
        | "turn on" -> current + 1
        | "turn off" -> if current = 0 then 0 else current - 1
        | "toggle" -> current + 2
        | _ -> current

    let result = runThroughGrid grid directions getLightStatus

    result
    |> Array2D.map (fun cell ->
        let (_, _, light) = cell
        light)
    |> Seq.cast<int>
    |> Seq.sum

