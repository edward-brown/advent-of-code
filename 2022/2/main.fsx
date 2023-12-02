let input = System.IO.File.ReadLines("2022/2/input.txt")

let getRoundScore (myMove: string) (theirMove: string) =
    let shapeScore =
        match (myMove) with
        | "X"
        | "A" -> 1
        | "Y"
        | "B" -> 2
        | "Z"
        | "C" -> 3
        | _ -> 0

    let score =
        match (theirMove, myMove) with
        | ("A", "X")
        | ("A", "A")
        | ("B", "Y")
        | ("B", "B")
        | ("C", "Z")
        | ("C", "C") -> 3
        | ("C", "X")
        | ("A", "Y")
        | ("B", "Z") -> 6
        | _ -> 0

    shapeScore + score

let part1 =
    (0, input)
    ||> Seq.fold (fun acc x ->
        let moves = x.Split " "
        let theirMove = moves |> Array.head
        let myMove = moves |> Array.last

        acc + (getRoundScore myMove theirMove))

let part2 =
    (0, input)
    ||> Seq.fold (fun acc x ->
        let moves = x.Split " "
        let theirMove = moves |> Array.head
        let roundEnd = moves |> Array.last

        let myMove =
            match (theirMove, roundEnd) with
            | ("A", "X") -> "Z"
            | ("B", "X") -> "X"
            | ("C", "X") -> "Y"
            | (_, "Y") -> theirMove
            | ("A", "Z") -> "Y"
            | ("B", "Z") -> "Z"
            | ("C", "Z") -> "X"
            | _ -> "INVALID MOVE"

        acc + (getRoundScore myMove theirMove))
