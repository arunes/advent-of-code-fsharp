module Puzzles.Year2015.Day15

open System.IO
open System.Text.RegularExpressions

type Properties =
    { Capacity: int
      Durability: int
      Flavor: int
      Texture: int
      Calories: int }

type Variation = { Ingredient: string; TeaSpoons: int }

let private getIngredients () : Map<string, Properties> =
    "./puzzles/2015/day-15/input.txt"
    |> File.ReadAllLines
    |> Seq.map (fun line ->
        Regex.Match(
            line,
            @"(\w+): capacity (-\d+|\d+), durability (-\d+|\d+), flavor (-\d+|\d+), texture (-\d+|\d+), calories (-\d+|\d+)"
        ))
    |> Seq.cast
    |> Seq.map (fun (rm: Match) ->
        (rm.Groups[1].Value,
         { Capacity = (int rm.Groups[2].Value)
           Durability = (int rm.Groups[3].Value)
           Flavor = (int rm.Groups[4].Value)
           Texture = (int rm.Groups[5].Value)
           Calories = (int rm.Groups[6].Value) }))
    |> Map.ofSeq

let private distValues list limit =
    let rec loop list (remaining, result) =
        seq {
            match list with
            | [] -> yield result
            | [ head ] -> yield (head, limit - remaining) :: result
            | head :: tail ->
                for n in 0 .. limit - remaining do
                    yield! loop tail (remaining + n, (head, n) :: result)
        }

    loop list (0, [])


let private mixCookies () =
    let allIngredients = getIngredients ()

    let mixIt recipe =
        recipe
        |> List.map (fun (name, amount) ->
            let ingredient = allIngredients[name]

            { Capacity = amount * ingredient.Capacity
              Durability = amount * ingredient.Durability
              Flavor = amount * ingredient.Flavor
              Texture = amount * ingredient.Texture
              Calories = amount * ingredient.Calories })

    let calculateScore (recipe: list<Properties>) =
        let capacity = recipe |> List.sumBy (fun props -> props.Capacity)
        let durability = recipe |> List.sumBy (fun props -> props.Durability)
        let flavor = recipe |> List.sumBy (fun props -> props.Flavor)
        let texture = recipe |> List.sumBy (fun props -> props.Texture)
        let calories = recipe |> List.sumBy (fun props -> props.Calories)
        let posOrZero number = if number < 0 then 0 else number

        (posOrZero capacity * posOrZero durability * posOrZero flavor * posOrZero texture, calories)

    distValues (Seq.toList allIngredients.Keys) 100
    |> Seq.map mixIt
    |> Seq.map calculateScore

// Part 1
let getBestCookie () =
    mixCookies () |> Seq.maxBy (fun (score, _) -> score) |> fst

// Part 2
let getBestCookieWithSetCalories calorie =
    mixCookies ()
    |> Seq.where (fun (_, calories) -> calories = calorie)
    |> Seq.maxBy (fun (score, _) -> score)
    |> fst
