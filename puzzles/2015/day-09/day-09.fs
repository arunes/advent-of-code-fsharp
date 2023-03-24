module Puzzles.Year2015.Day09

open System.Text.RegularExpressions
open System.IO

let private distances =
    "./puzzles/2015/day-09/input.txt"
    |> File.ReadAllLines
    |> Seq.map (fun line ->
        let regex = Regex.Match(line, @"(\w+) to (\w+) = (\d+)")
        let fromCity = regex.Groups[1].Value
        let toCity = regex.Groups[2].Value
        let distance = int regex.Groups[3].Value

        (fromCity, toCity, distance))

let private cities =
    let fromCities =
        distances |> Seq.map (fun (fromCity, _, _) -> fromCity) |> Seq.toList

    let toCities = distances |> Seq.map (fun (_, toCity, _) -> toCity) |> Seq.toList
    (fromCities @ toCities) |> List.distinct


let permute list =
    let rec inserts e =
        function
        | [] -> [ [ e ] ]
        | x :: xs as list -> (e :: list) :: [ for xs' in inserts e xs -> x :: xs' ]

    List.fold (fun accum x -> List.collect (inserts x) accum) [ [] ] list

let private routesWithDistances =
    (permute cities)
    |> List.map (fun route ->
        let miles =
            route
            |> List.pairwise
            |> List.map (fun (fromCity, toCity) ->
                let (_, _, mls) =
                    distances
                    |> Seq.find (fun (a, b, c) -> (a = fromCity && b = toCity) || (a = toCity && b = fromCity))

                mls)
            |> List.sum

        miles)

// Part 1
let shortestRoute = routesWithDistances |> List.min

// Part 2
let longestRoute = routesWithDistances |> List.max
