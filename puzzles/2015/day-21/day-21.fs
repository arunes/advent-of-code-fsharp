module Puzzles.Year2015.Day21

open System.IO
open System.Text.RegularExpressions

type Player =
    { Name: string
      HitPoints: int
      Damage: int
      Armor: int }

type ItemType =
    | Weapon
    | Armor
    | Ring

type Item =
    { Type: ItemType
      Name: string
      Cost: int
      Damage: int
      Armor: int }

let private getShopItems () =
    let lines = "./puzzles/2015/day-21/shop.txt" |> File.ReadAllLines

    let getStats itemType line =
        let m = Regex.Match(line, @"(.+)\s{2,}(\d+)\s{5}(\d)\s{7}(\d)")

        { Type = itemType
          Name = m.Groups[1].Value.Trim()
          Cost = int m.Groups[2].Value
          Damage = int m.Groups[3].Value
          Armor = int m.Groups[4].Value }

    let weapons =
        [ 1..5 ] |> Seq.map (fun index -> lines[index] |> getStats Weapon) |> Seq.toList

    let armors =
        [ 8..12 ] |> Seq.map (fun index -> lines[index] |> getStats Armor) |> Seq.toList

    let rings =
        [ 15..20 ] |> Seq.map (fun index -> lines[index] |> getStats Ring) |> Seq.toList

    let nakedArmor =
        { Type = Armor
          Name = "Naked"
          Cost = 0
          Damage = 0
          Armor = 0 }

    let nakedRing =
        { Type = Ring
          Name = "Naked"
          Cost = 0
          Damage = 0
          Armor = 0 }

    weapons @ nakedArmor :: armors @ nakedRing :: rings

let private damageDealt (player1: Player, player2: Player) =
    if player1.Damage - player2.Armor < 1 then
        1
    else
        player1.Damage - player2.Armor

let rec private fight player1 player2 turn =
    let damage = damageDealt (player1, player2)

    let player2After =
        { player2 with
            HitPoints = player2.HitPoints - damage }

    if player2After.HitPoints <= 0 then
        player1
    else
        fight player2After player1 (turn + 1)

let private allDamageArmorCombinations () =
    let items = getShopItems ()

    let combineAll xs ys zs =
        [ for x in xs do
              for y in ys do
                  for z in zs do
                      yield x, y, z ]

    let weapons = items |> List.filter (fun itm -> itm.Type = Weapon)
    let armors = items |> List.filter (fun itm -> itm.Type = Armor)
    let rings = items |> List.filter (fun itm -> itm.Type = Ring)

    let ringCombos =
        rings
        |> List.allPairs rings
        |> List.filter (fun (r1, r2) -> r1.Name = "Naked" || r1 <> r2)


    // sort from cheapest
    combineAll weapons armors ringCombos

let rec private wearAndFight sets expectedWinner =
    let boss =
        { Name = "Boss"
          HitPoints = 104
          Damage = 8
          Armor = 1 }

    match sets with
    | [] -> None
    | set :: tail ->
        let (weapon, armor, (ring1, ring2)) = set
        let totalDamage = weapon.Damage + armor.Damage + ring1.Damage + ring2.Damage
        let totalArmor = weapon.Armor + armor.Armor + ring1.Armor + ring2.Armor

        let player =
            { Name = "Henry Case"
              HitPoints = 100
              Damage = totalDamage
              Armor = totalArmor }

        let winner = fight player boss 1

        if winner.Name = expectedWinner then
            Some(set)
        else
            wearAndFight tail expectedWinner

// Part 1
let cheapestWin () =
    let playerSets =
        allDamageArmorCombinations ()
        |> List.sortBy (fun (w, a, (r1, r2)) -> w.Cost + a.Cost + r1.Cost + r2.Cost)

    let winningSet = wearAndFight playerSets "Henry Case"

    if winningSet.IsSome then
        let (weapon, armor, (ring1, ring2)) = winningSet.Value
        weapon.Cost + armor.Cost + ring1.Cost + ring2.Cost
    else
        0

// Part 2
let mostExpensiveLoss () =
    let playerSets =
        allDamageArmorCombinations ()
        |> List.sortByDescending (fun (w, a, (r1, r2)) -> w.Cost + a.Cost + r1.Cost + r2.Cost)

    let losingSet = wearAndFight playerSets "Boss"

    if losingSet.IsSome then
        let (weapon, armor, (ring1, ring2)) = losingSet.Value
        weapon.Cost + armor.Cost + ring1.Cost + ring2.Cost
    else
        0
