module Puzzles.Year2015.Day04

open System.Security.Cryptography
open System.Text

let md5Hash (input: string) =
    use md5 = MD5.Create()

    input
    |> Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)


let rec getHashStartingWith (input: string, startsWith: string, list: list<int>) : int =
    match list with
    | [] -> -1 // no more list - just return -1
    | head :: tail ->
        let md5String = md5Hash $"{input}{head}"

        if md5String.StartsWith(startsWith) then
            head
        else
            getHashStartingWith (input, startsWith, tail) // calculate next one

let getLowestHash =
    let possibilities = [ 100000..999999 ]
    getHashStartingWith ("yzbqklnj", "00000", possibilities)

let getLowestHashSixZeroes =
    let possibilities = [ 1000000..9999999 ]
    getHashStartingWith ("yzbqklnj", "000000", possibilities)
