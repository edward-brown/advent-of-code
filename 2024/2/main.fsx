let input = System.IO.File.ReadAllLines("2024/2/input.txt")

let distanceSafe (a: int) (b: int) = abs (a - b) > 0 && abs (a - b) < 4

let allIncreasing (x: (int * int) seq) : bool =
    x |> Seq.map (fun (a, b) -> a > b && (distanceSafe a b)) |> Seq.filter (fun a -> not a) |> Seq.length = 0

let allDecreasing (x: (int * int) seq) : bool =
    x |> Seq.map (fun (a, b) -> a < b && (distanceSafe a b)) |> Seq.filter (fun a -> not a) |> Seq.length = 0

let isSafe (x: (int * int) seq) : bool = (allIncreasing x) || (allDecreasing x)

let isSafeWithNRemoved (x: int seq) =
    x 
    |> Seq.mapi (fun (i: int) _ -> x |> Seq.removeAt i |> Seq.pairwise |> isSafe)
    |> Seq.filter (fun x -> x)
    |> Seq.length > 0

let parse () =
    input |> Seq.map (fun x -> x.Split(' ') |> Seq.map int)

// 356
let part1 =
    parse ()
    |> Seq.map (fun x -> x |> Seq.pairwise |> isSafe)
    |> Seq.filter (fun x -> x)
    |> Seq.length

// 413
let part2 =
    parse ()
    |> Seq.map (isSafeWithNRemoved)
    |> Seq.filter (fun x -> x)
    |> Seq.length