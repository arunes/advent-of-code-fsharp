module Puzzles.Year2015.Day03

open System.IO

type Location = { X: int; Y: int }
type House = { Location: Location; Presents: int }
type Status = { Current: House; All: List<House> }

type Direction =
    | Right
    | Top
    | Down
    | Left
    | None

type DirectionWithIndex = { Direction: Direction; Index: int }


let private getInstructions =
    let fileContent = File.ReadAllText("./puzzles/2015/day-03/input.txt")

    Seq.toList fileContent
    |> List.map (fun direction ->
        match direction with
        | '>' -> Right
        | '^' -> Top
        | 'v' -> Down
        | '<' -> Left
        | _ -> None)


let private getNewHouse x y =
    { Location = { X = x; Y = y }
      Presents = 1 }

let private getNewLocation current direction =
    match direction with
    | Top -> { X = current.X; Y = current.Y - 1 }
    | Right -> { X = current.X + 1; Y = current.Y }
    | Down -> { X = current.X; Y = current.Y + 1 }
    | Left -> { X = current.X - 1; Y = current.Y }
    | None -> current

let private getHouseListWithUpdatedPresents houses existingHouse newLocation =
    let newHouses =
        houses
        |> List.map (fun house ->
            // find the house and increase the present count
            match house.Location with
            | loc when loc.X = newLocation.X && loc.Y = newLocation.Y ->
                { house with
                    Presents = house.Presents + 1 }
            | _ -> house)

    { Current = existingHouse
      All = newHouses }

let private givePresents houses currentHouse direction =
    let newLocation = getNewLocation currentHouse.Location direction
    let existingHouse = List.tryFind (fun house -> house.Location = newLocation) houses

    if existingHouse.IsNone then
        let newHouse = (getNewHouse newLocation.X newLocation.Y)

        { Current = newHouse
          All = newHouse :: houses }
    else
        getHouseListWithUpdatedPresents houses existingHouse.Value newLocation

// Part 1
let givePresentsAsSanta =
    let initHouse = (getNewHouse 0 0)

    let initResult =
        { Current = initHouse
          All = [ initHouse ] }

    let results =
        List.fold (fun acc direction -> givePresents acc.All acc.Current direction) initResult getInstructions

    results.All.Length

// Part 2
let givePresentsAsSantaAndRobot =
    let initHouse = (getNewHouse 0 0)

    let initResult =
        { Current = initHouse
          All = [ initHouse ] }

    let allInstructions =
        List.mapi (fun idx elm -> { Direction = elm; Index = idx + 1 }) getInstructions

    let robotsInstructions = List.filter (fun elm -> elm.Index % 2 = 0) allInstructions
    let santasInstructions = List.except robotsInstructions allInstructions

    let santasResults =
        List.fold
            (fun acc direction -> givePresents acc.All acc.Current direction)
            initResult
            (List.map (fun elm -> elm.Direction) santasInstructions)

    let robotsResults =
        List.fold
            (fun acc direction -> givePresents acc.All acc.Current direction)
            initResult
            (List.map (fun elm -> elm.Direction) robotsInstructions)

    let allResults = santasResults.All @ robotsResults.All
    
    allResults |> List.distinctBy (fun elm -> elm.Location) |> List.length