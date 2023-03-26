module Puzzles.Year2015.Day19

open System.IO
open System.Text.RegularExpressions

let private getInput () =
    let lines = "./puzzles/2015/day-19/input.txt" |> File.ReadAllLines

    let formulas =
        seq {
            for line in lines do
                if line.Contains("=>") then
                    yield (line.Split(' ')[0], line.Split(' ')[2])
        }

    (lines |> Seq.last, formulas |> Seq.toList)


let rec private doReplace replacements input =
    seq {
        match replacements with
        | [] -> ()
        | (find, replace) :: tail ->
            let matches = Regex.Matches(input, find) |> Seq.cast

            for (m: Match) in matches do
                yield input.Remove(m.Index, m.Length).Insert(m.Index, replace)

            yield! doReplace tail input
    }

// Part 1
let numberOfDistinctMolecules () =
    let (input, replacements) = getInput ()
    doReplace replacements input |> Seq.toList |> List.distinct |> List.length


// Part 2
// https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju/
let howLongWillItTake () =
    let (input, _) = getInput ()

    let elements = input |> Seq.filter (fun c -> System.Char.IsUpper(c)) |> Seq.length
    let rn = Regex.Count(input, "Rn")
    let y = Regex.Count(input, "Y")
    elements - 2 * rn - 2 * y - 1
