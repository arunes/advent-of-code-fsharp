module Puzzles.Year2015.Day18

open System.IO

let private getGrid () =
    "./puzzles/2015/day-18/input.txt"
    |> File.ReadAllLines
    |> Seq.mapi (fun x line -> line |> Seq.toList |> Seq.mapi (fun y status -> (x, y, status = '#')))
    |> array2D

let private toggleLights times cornersOn grid =
    let gridMax = Array2D.length1 grid

    let getNeighbors x y =
        [ (x - 1, y - 1) // top left
          (x - 1, y) // top
          (x - 1, y + 1) // top right
          (x, y + 1) // right
          (x + 1, y + 1) // bottom right
          (x + 1, y) // bottom
          (x + 1, y - 1) // bottom left
          (x, y - 1) ] // left
        |> List.filter (fun (nx, ny) -> nx >= 0 && ny >= 0 && nx < gridMax && ny < gridMax)

    let toggle (x, y, isOn, cGrid: (int * int * bool) array2d) =
        let neighbors =
            getNeighbors x y
            |> List.filter (fun (nx, ny) ->
                let (_, _, isOn) = cGrid[nx, ny]

                isOn)
            |> List.length

        let isCorner =
            (x = 0 && y = 0) // top left
            || (x = gridMax - 1 && y = 0) // top right
            || (x = gridMax - 1 && y = gridMax - 1) // bottom right
            || (x = 0 && y = gridMax - 1) // bottom left

        if isOn && (neighbors = 2 || neighbors = 3) then
            (x, y, true)
        elif not isOn && neighbors = 3 then
            (x, y, true)
        elif cornersOn && isCorner then
            (x, y, true)
        else
            (x, y, false)

    [ 1..times ]
    |> Seq.fold (fun acc _ -> Array2D.map (fun (x, y, isOn) -> toggle (x, y, isOn, acc)) acc) grid

// Part 1
let howManyLightsAreOn times =
    (getGrid ())
    |> toggleLights times false
    |> Array2D.map (fun cell ->
        let (_, _, isOn) = cell
        if isOn = true then 1 else 0)
    |> Seq.cast<int>
    |> Seq.sum

// Part 2
let howManyLightsAreOnCornersOn times =
    (getGrid ())
    |> toggleLights times true
    |> Array2D.map (fun cell ->
        let (_, _, isOn) = cell
        if isOn = true then 1 else 0)
    |> Seq.cast<int>
    |> Seq.sum
