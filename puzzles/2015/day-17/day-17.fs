module Puzzles.Year2015.Day17

open System.IO

let private getBuckets () =
    "./puzzles/2015/day-17/input.txt"
    |> File.ReadAllLines
    |> Seq.map int
    |> Seq.toList

let private distubeWater buckets target =
    let rec loop buckets target current result =
        seq {
            match buckets with
            | bucket :: tail ->
                // with current bucket we hit the target
                if bucket + current = target then
                    yield bucket :: result |> List.toArray // add bucket to the result
                // with current bucket we still need to fill some
                elif bucket + current < target then
                    // call this again with current increased and bucket added to result
                    yield! loop tail target (bucket + current) (bucket :: result)

                yield! loop tail target current result
            | _ -> ()
        }

    loop buckets target 0 []

// Part 1
let getNumberOfCombinations () =
    let buckets = getBuckets ()
    distubeWater buckets 150 |> Seq.length

// Part 2
let getMinOfCombinations () =
    let buckets = getBuckets ()
    let combinations = distubeWater buckets 150 |> Seq.toList
    let minComb = combinations |> List.minBy (fun comb -> comb.Length) |> Seq.length
    combinations |> List.where (fun comb -> comb.Length = minComb) |> Seq.length
