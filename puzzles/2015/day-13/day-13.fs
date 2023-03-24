module Puzzles.Year2015.Day13

module Day09 = Puzzles.Year2015.Day09

open System.IO
open System.Text.RegularExpressions

let private getPeople addMe =
    let people =
        "./puzzles/2015/day-13/input.txt"
        |> File.ReadAllLines
        |> Seq.fold
            (fun (acc: Map<string, Map<string, int>>) line ->
                let regexMatch =
                    Regex.Match(line, @"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.")

                let person1 = regexMatch.Groups[1].Value
                let person2 = regexMatch.Groups[4].Value

                let happiness =
                    (int regexMatch.Groups[3].Value)
                    * (if regexMatch.Groups[2].Value = "gain" then 1 else -1)

                if acc.ContainsKey(person1) then
                    acc.Change(
                        person1,
                        (function
                        | Some b -> Some(b.Add(person2, happiness))
                        | None -> None)
                    )
                else
                    acc.Add(person1, Map.empty.Add(person2, happiness)))
            Map.empty<string, Map<string, int>>

    if addMe then
        let newPeople =
            people.Keys
            |> Seq.map (fun name -> name, people[name].Add("Me", 0))
            |> Map.ofSeq

        let names =
            people.Keys
            |> Seq.fold (fun (acc: Map<string, int>) p -> acc.Add(p, 0)) Map.empty<string, int>

        newPeople.Add("Me", names)
    else
        people

let private getHappiness (people: Map<string, Map<string, int>>, person, index, arrangment: list<string>, toLeft) =
    let totalPeople = arrangment.Length

    let person2 =
        if toLeft then
            if index = totalPeople - 1 then
                List.head arrangment
            else
                List.item (index + 1) arrangment
        else if index = 0 then
            List.last arrangment
        else
            List.item (index - 1) arrangment

    people[person][person2]


let arrangmentWithHappiness (people: Map<string, Map<string, int>>) =
    people.Keys
    |> Seq.toList
    |> Day09.permute
    |> List.map (fun arrangment ->
        let happiness =
            arrangment
            |> List.mapi (fun idx person ->
                let toLeft = getHappiness (people, person, idx, arrangment, true)
                let toRight = getHappiness (people, person, idx, arrangment, false)
                toLeft + toRight)
            |> List.sum

        happiness)
    |> List.max

// Part 1
let totalHappiness =
    let people = getPeople false
    arrangmentWithHappiness people

// Part 2
let totalHappinessWithMe =
    let people = getPeople true
    arrangmentWithHappiness people
