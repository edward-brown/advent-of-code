let input = System.IO.File.ReadAllLines("2023/4/input.txt") |> Array.toSeq

let isNumber (s: string) : bool =
    match System.Int32.TryParse(s) with
    | (true, _) -> true
    | _ -> false

let getWinningNumbers (line: string) : int seq =
    let split = line.Split(":").[1].Split("|")
    let winningNumbers = split.[0].Split(" ") |> Seq.filter isNumber |> Seq.map int
    let myNumbers = split.[1].Split(" ") |> Seq.filter isNumber |> Seq.map int

    winningNumbers
    |> Seq.map (fun x ->
        match myNumbers |> Seq.contains x with
        | true -> Some x
        | false -> None)
    |> Seq.choose id

let getScore (x: int seq) : int =
    x
    |> Seq.fold
        (fun acc x ->
            match acc with
            | 0 -> acc + 1
            | _ -> acc * 2)
        0

let part1 = input |> Seq.map getWinningNumbers |> Seq.map getScore |> Seq.sum

// Totally misread that
let part2 =
    input
    |> Seq.fold
        (fun acc x ->
            match acc with
            | (true, _) -> acc
            | (false, _) ->
                match x.Split(":").[1].Split("|") with
                | [| x; y |] ->
                    match x.Split(" ") |> Seq.filter isNumber |> Seq.map int |> Seq.contains 0 with
                    | true -> (true, y.Split(" ") |> Seq.filter isNumber |> Seq.map int |> Seq.sum)
                    | false -> (false, 0)
                | _ -> (false, 0))
        (false, 0)
