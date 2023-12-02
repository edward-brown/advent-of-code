let input = System.IO.File.ReadAllLines("2022/1/input.txt")

type State = { Totals: int List; Sum: int }

let totals =
    ({ Totals = []; Sum = 0 }, input)
    ||> Array.fold (fun acc a ->
        match a with
        | "" ->
            { Totals = acc.Totals @ [ acc.Sum ]
              Sum = 0 }
        | _ -> { acc with Sum = acc.Sum + int a })
    |> (fun x -> { x with Totals = x.Totals @ [ x.Sum ] })
    |> (fun x -> x.Totals)

let part1 =
    totals
    |> Seq.mapi (fun i v -> i, v)
    |> Seq.maxBy snd

let part2 =
    totals
    |> List.sortDescending
    |> List.take 3
    |> List.sum
