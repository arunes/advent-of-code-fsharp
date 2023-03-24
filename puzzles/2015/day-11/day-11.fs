module Puzzles.Year2015.Day11

let letterToNumber (letter: char) = int letter - int 'a'
let numberToLetter (number: int) = char (number + int 'a')
let wordToNumbers (word: string) = word |> Seq.map letterToNumber

let rec triplewise (list: list<int>, result: list<(int * int * int)>) =
    match list with
    | [] -> result
    | first :: second :: third :: tail ->
        let newResult = result @ [ (first, second, third) ]
        triplewise (([ second; third ] @ tail), newResult)
    | _ -> result

let numbersToWord (ints: seq<int>) =
    ints |> Seq.map numberToLetter |> Seq.map string |> String.concat ""

let isValid (password: string) =
    let nonValidChars = [ 'i'; 'o'; 'l' ]
    let passwordNumbers = password |> wordToNumbers |> Seq.toList
    let triples = triplewise (passwordNumbers, [])
    let pairs = passwordNumbers |> List.pairwise

    // have increasing 3 sequence
    let hasIncreasingSeq =
        triples |> List.tryFind (fun (a, b, c) -> c - b = 1 && b - a = 1)

    // have bad letters
    let hasBadLetters = nonValidChars |> Seq.tryFind (fun c -> password.Contains(c))
    let allEqPairs = pairs |> List.where (fun (a, b) -> a = b)

    // 2 pairs not overlapping
    let hasGoodPairs =
        allEqPairs.Length > 1
        && (allEqPairs |> List.distinct).Length = allEqPairs.Length

    hasIncreasingSeq.IsSome && hasBadLetters.IsNone && hasGoodPairs


let private increment password =
    (password |> wordToNumbers, (true, []))
    ||> Seq.foldBack (fun current acc ->
        let (shouldIncrease, result) = acc
        let newNumber = current + (if shouldIncrease then 1 else 0)
        let overflow = newNumber > 25
        (overflow, (newNumber - (if overflow then 26 else 0) :: result)))
    |> snd
    |> numbersToWord

// Part 1 & 2
let rec getNextPassword password =
    let newPassword = increment password

    if isValid newPassword then
        newPassword
    else
        getNextPassword newPassword
