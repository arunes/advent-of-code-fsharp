module Puzzles.Year2015.Day23

open System.IO

type Instruction =
    { Index: int
      Command: string
      Register: char
      Offset: int }

let getInstructions () =
    "./puzzles/2015/day-23/input.txt"
    |> File.ReadAllLines
    |> Seq.mapi (fun idx line ->
        let parts = line.Split(' ') |> Array.map (fun part -> part.Trim([| ' '; ',' |]))

        { Index = idx
          Command = parts[0]
          Register = if parts[0] <> "jmp" then parts[1][0] else '_'
          Offset =
            match parts[0] with
            | "jmp" -> int parts[1]
            | "jio"
            | "jie" -> int parts[2]
            | _ -> 0 })
    |> Seq.toList

let updateRegister register updater (registers: Map<char, int>) =
    registers
    |> Map.change register (fun x ->
        match x with
        | Some s -> Some(updater s)
        | _ -> None)

let doModifications instruction registers =
    match instruction.Command with
    | "hlf" -> updateRegister instruction.Register (fun v -> v / 2) registers
    | "tpl" -> updateRegister instruction.Register (fun v -> v * 3) registers
    | "inc" -> updateRegister instruction.Register (fun v -> v + 1) registers
    | _ -> registers

let getNewIndex instruction value index =

    match instruction.Command with
    | "jmp" -> instruction.Offset + index
    | "jie" ->
        if value % 2 = 0 then
            instruction.Offset + index
        else
            index + 1
    | "jio" -> if value = 1 then instruction.Offset + index else index + 1
    | _ -> index + 1

let rec moveAround (registers: Map<char, int>) index instructions =
    if index > ((instructions |> List.length) - 1) then
        registers
    else
        let instruction = instructions.Item index

        let registers = registers |> doModifications instruction

        let index = getNewIndex instruction registers[instruction.Register] index
        moveAround registers index instructions

// Part 1
let runWithInitialZeros () =
    let initialRegister = Map.empty.Add('_', 0).Add('a', 0).Add('b', 0)
    let result = getInstructions () |> moveAround initialRegister 0
    result['b']

// Part 2
let runWithInitialOne () =
    let initialRegister = Map.empty.Add('_', 0).Add('a', 1).Add('b', 0)
    let result = getInstructions () |> moveAround initialRegister 0
    result['b']
