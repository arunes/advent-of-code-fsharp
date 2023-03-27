module Puzzles.Year2015.Day20

let private getFactors number =
    let root = number |> double |> sqrt |> int

    seq {
        for factor in [ 1..root ] do
            if number % factor = 0 then
                yield factor

                if factor <> number / factor then
                    yield (number / factor)
    }

// Part 1
let rec giveInfitePresents currentHouse giftLimit =
    let gifts = currentHouse |> getFactors |> Seq.sumBy (fun f -> f * 10)

    if gifts >= giftLimit then
        currentHouse
    else
        giveInfitePresents (currentHouse + 1) giftLimit


// Part 2
let rec giveLimitedPresents currentHouse giftLimit =
    let gifts =
        currentHouse
        |> getFactors
        |> Seq.filter (fun f -> f * 50 >= currentHouse)
        |> Seq.sumBy (fun f -> f * 11)

    if gifts >= giftLimit then
        currentHouse
    else
        giveLimitedPresents (currentHouse + 1) giftLimit
