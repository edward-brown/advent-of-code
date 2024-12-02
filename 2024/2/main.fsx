let input = System.IO.File.ReadAllLines("2024/2/input.txt")

let increasingFailCount (x: (int * int) seq) =
    x |> Seq.map (fun (a, b) -> a > b) |> Seq.filter (fun a -> not a) |> Seq.length

let decreasingFailCount (x: (int * int) seq) =
    x |> Seq.map (fun (a, b) -> a < b) |> Seq.filter (fun a -> not a) |> Seq.length

let safeDistancesFailCount (x: (int * int) seq) =
    x
    |> Seq.map (fun (a, b) -> (a - b) |> abs)
    |> Seq.map (fun a -> a > 0 && a < 4)
    |> Seq.filter (fun a -> not a)
    |> Seq.length

let failCount (x: (int * int) seq) =
    (increasingFailCount x) + (decreasingFailCount x) + (safeDistancesFailCount x)

let parse () =
    input |> Seq.map (fun x -> x.Split(' ') |> Seq.map int)

// 356
let part1 =
    parse ()
    |> Seq.map (fun x -> x |> Seq.pairwise |> failCount = 0)
    |> Seq.filter (fun x -> x)
    |> Seq.length

//
let part2 =
    parse ()
    |> Seq.map (fun x -> x |> Seq.pairwise |> failCount < 2)
    |> Seq.filter (fun x -> x)
    |> Seq.length
