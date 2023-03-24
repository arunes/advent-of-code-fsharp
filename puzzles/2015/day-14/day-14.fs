module Puzzles.Year2015.Day14

open System.IO
open System.Text.RegularExpressions

type Stats =
    { Speed: int
      FlyDuration: int
      RestDuration: int }

type RaceStatus =
    { RestUntil: int
      FlyUntil: int
      DistanceFlew: int
      Points: int }

let getReindeers () : Map<string, Stats> =
    "./puzzles/2015/day-14/input.txt"
    |> File.ReadAllLines
    |> Seq.map (fun line ->
        Regex.Match(line, @"(\w+) can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+) seconds."))
    |> Seq.cast
    |> Seq.map (fun (rm: Match) ->
        (rm.Groups[1].Value,
         { Speed = (int rm.Groups[2].Value)
           FlyDuration = (int rm.Groups[3].Value)
           RestDuration = (int rm.Groups[4].Value) }))
    |> Map.ofSeq

let race duration =
    let reindeers = getReindeers ()

    let initialStatus =
        reindeers.Keys
        |> Seq.map (fun name ->
            (name,
             { RestUntil = 0
               FlyUntil = reindeers[name].FlyDuration
               DistanceFlew = 0
               Points = 0 }))
        |> Map.ofSeq

    let getNewStatus current second reindeer =
        let isRunning = current.FlyUntil >= second

        let distanceFlew =
            if isRunning then
                current.DistanceFlew + reindeer.Speed
            else
                current.DistanceFlew

        let restUntil =
            if current.RestUntil = second then
                0
            elif current.FlyUntil = second then
                second + reindeer.RestDuration
            else
                current.RestUntil

        let flyUntil =
            if current.FlyUntil = second then
                0
            elif current.RestUntil = second then
                second + reindeer.FlyDuration
            else
                current.FlyUntil

        { DistanceFlew = distanceFlew
          RestUntil = restUntil
          FlyUntil = flyUntil
          Points = current.Points }

    [ 1..duration ]
    |> List.fold
        (fun (acc: Map<string, RaceStatus>) second ->
            let allStatuses =
                acc.Keys
                |> Seq.map (fun name ->
                    let reindeer = reindeers[name]
                    let newStats = getNewStatus acc[name] second reindeer
                    (name, newStats))
                |> Map.ofSeq

            let maxDistance =
                (allStatuses
                 |> Map.toSeq
                 |> Seq.sortByDescending (fun (name, status) -> status.DistanceFlew)
                 |> Seq.head
                 |> snd)
                    .DistanceFlew

            allStatuses
            |> Map.toSeq
            |> Seq.map (fun (name, status) ->
                if status.DistanceFlew = maxDistance then
                    (name,
                     { status with
                         Points = status.Points + 1 })
                else
                    (name, status))
            |> Map.ofSeq)
        initialStatus

// Part 1
let raceTraditional duration =
    let winner =
        race duration
        |> Map.toSeq
        |> Seq.sortByDescending (fun (name, status) -> status.DistanceFlew)
        |> Seq.head
        |> snd

    winner.DistanceFlew

// Part 2
let raceWithPoints duration =
    let winner =
        race duration
        |> Map.toSeq
        |> Seq.sortByDescending (fun (name, status) -> status.Points)
        |> Seq.head
        |> snd

    winner.Points
