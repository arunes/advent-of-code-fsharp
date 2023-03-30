module Puzzles.Year2015.Day25

let rec findPassword (cX, cY) (rX, rY) lastPass =
    let password = (lastPass * 252533L) % 33554393L
    let (nX, nY) = if cY = 1 then (1, cX + 1) else (cX + 1, cY - 1)

    if cX = rX && cY = rY then
        password
    else
        findPassword (nX, nY) (rX, rY) password

let getPassword () =
    findPassword (1, 2) (3075, 2981) 20151125L
