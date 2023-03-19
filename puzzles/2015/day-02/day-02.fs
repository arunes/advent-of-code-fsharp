module Puzzles.Year2015.Day02

open System.IO

type Dimension =
    { Length: int; Width: int; Height: int }

let private getPackages fileName =
    let packages = File.ReadAllLines(fileName)

    packages
    |> Array.map (fun dimensions ->
        let dim = dimensions.Split 'x' |> Array.map int

        { Length = dim[0]
          Width = dim[1]
          Height = dim[2] }

    )
    |> Array.toList


// Calculate Wrapper
let private actualWrapperNeeded dimension =
    (2 * dimension.Length * dimension.Width)
    + (2 * dimension.Width * dimension.Height)
    + (2 * dimension.Height * dimension.Length)

let private extraWrapperNeeded dimension =
    let length = dimension.Length * dimension.Width
    let width = dimension.Width * dimension.Height
    let height = dimension.Height * dimension.Length
    [ length; width; height ] |> List.min

let private totalWrapperNeeded dimension =
    let actual = actualWrapperNeeded dimension
    let extra = extraWrapperNeeded dimension
    actual + extra


// Calculate Ribbon
let private actualRibbonNeeded dimension =
    let length = dimension.Length + dimension.Width
    let width = dimension.Width + dimension.Height
    let height = dimension.Height + dimension.Length
    [ length; width; height ] |> List.min |> (fun total -> total * 2)

let private ribbonBowNeeded dimension =
    dimension.Length * dimension.Width * dimension.Height

let private totalRibbonNeeded dimension =
    let actual = actualRibbonNeeded dimension
    let bow = ribbonBowNeeded dimension
    actual + bow


// Part 1
let allWrapperNeeded =
    let packages = getPackages "./puzzles/2015/day-02/input.txt"
    packages |> List.map totalWrapperNeeded |> List.sum

// Part 2
let allRibbonNeeded =
    let packages = getPackages "./puzzles/2015/day-02/input.txt"
    packages |> List.map totalRibbonNeeded |> List.sum
