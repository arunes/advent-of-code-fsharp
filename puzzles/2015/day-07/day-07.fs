module Puzzles.Year2015.Day07

open System
open System.IO

let private isInt (s: string) = s |> Seq.forall Char.IsDigit

let private addToResults (key: string, value: int, values: Map<string, int>) =
    if values.ContainsKey(key) then
        values
    else
        values.Add(key, value)

let rec private getValue (key: string, commandList: list<string>, values: Map<string, int>) =
    if isInt key then // if key is a value return it
        (int key, values)
    elif values.ContainsKey(key) then // if we have the value return it
        (values[key], values)
    else
        let command = commandList |> List.find (fun cmd -> cmd.EndsWith($"-> {key}"))
        let operation = command.Split(" ->")[0]
        let operationParts = operation.Split(' ')

        match operationParts.Length with
        | 1 ->
            let key1 = operationParts[0]

            if isInt key1 then
                let result = int key1
                (result, addToResults (key, result, values)) // direct assignment (int)
            else
                let (value1, _) = (getValue (key1, commandList, values))
                (value1, values) // direct assignment (var)

        | 2 -> // NOT {x}
            let key1 = operationParts[1].Trim()
            let (value1, _) = (getValue (key1, commandList, values))
            let result = 65535 - value1
            (result, addToResults (key, result, values))
        | 3 -> // {x} {AND|OR|LSHIFT|RSHIFT} {y}
            let key1 = operationParts[0].Trim()
            let (value1, newValues) = getValue (key1, commandList, values)

            let key2 = operationParts[2].Trim()
            let (value2, newValues) = getValue (key2, commandList, newValues)

            let result =
                match operationParts[1] with
                | "AND" -> value1 &&& value2
                | "OR" -> value1 ||| value2
                | "LSHIFT" -> value1 <<< value2
                | "RSHIFT" -> value1 >>> value2
                | _ -> 0

            (result, addToResults (key, result, newValues))
        | _ -> (0, values) // N/A


let rec private processor (commandList: list<string>, values: Map<string, int>) =
    match commandList with
    | [] -> values
    | first :: tail ->
        let key = (first.Split("->")[1]).Trim()
        let (value, newValues) = getValue (key, commandList, values)
        let newValues = addToResults (key, value, newValues)
        processor (tail, newValues)


// Part 1
let getWireASignal =
    let commandList = File.ReadAllLines("./puzzles/2015/day-07/input.txt") |> Seq.toList

    let values = Map.empty<string, int>
    let result = processor (commandList, values)
    result["a"]

// Part 2
let getOverridenSignal =
    let commandList = File.ReadAllLines("./puzzles/2015/day-07/input.txt") |> Seq.toList

    let values = Map.empty<string, int>.Add("b", 46065)
    let result = processor (commandList, values)
    result["a"]
