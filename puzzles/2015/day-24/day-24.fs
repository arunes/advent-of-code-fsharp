module Puzzles.Year2015.Day24

open System.IO


let getBoxes () =
    "./puzzles/2015/day-24/input.txt"
    |> File.ReadAllLines
    |> Seq.map (fun line -> line |> int)
    |> Seq.toList

let findFewestBoxes boxes idealWeight exclude =
    boxes
    |> List.filter (fun box ->
        let excluded = exclude |> List.tryFind (fun ex -> ex = box)
        excluded.IsNone)
    |> List.fold
        (fun (totalWeight, stack) box ->
            let newWeight = totalWeight + box

            if newWeight > idealWeight then // this overflows, don't add
                (totalWeight, stack)
            else
                (newWeight, box :: stack))
        (0, [])

let lowestQEFor3Boxes () =
    let boxes = getBoxes () |> List.sortDescending
    let idealWeight = (boxes |> List.sum) / 3

    let (_, boxes) = findFewestBoxes boxes idealWeight []
    boxes |> List.fold (fun a b -> a * int64 b) 1L


let lowestQEFor4Boxes () =
    let boxes = getBoxes () |> List.sortDescending
    let idealWeight = (boxes |> List.sum) / 4

    let (_, optimalBoxes) = findFewestBoxes boxes idealWeight []
    let optimalBoxCount = optimalBoxes |> List.length

    let others =
        optimalBoxes
        |> List.map (fun box -> findFewestBoxes boxes idealWeight [ box ])
        |> List.filter (fun (w, boxes) -> w = idealWeight && (List.length boxes) = optimalBoxCount)


    let optimalQE = optimalBoxes |> List.fold (fun a b -> a * int64 b) 1L

    let otherQES =
        others
        |> List.map (fun (w, boxes) -> boxes |> List.fold (fun a b -> a * int64 b) 1L)

    optimalQE :: otherQES |> List.min
