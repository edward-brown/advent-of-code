let input = System.IO.File.ReadAllLines("2024/1/input.txt")

let allIncreasing (x: (int * int) seq) =
    x |> Seq.map (fun (a, b) -> not (a > b)) |> Seq.contains false

let allDecreasing (x: (int * int) seq) =
    x |> Seq.map (fun (a, b) -> not (a < b)) |> Seq.contains false

let isSafe (x: (int * int) seq) = allDecreasing x || allIncreasing x

let safe1 = [ 7; 6; 4; 2; 1 ]
let safe2 = [ 1; 2; 7; 8; 9 ]
let notSafe1 = [ 9; 7; 6; 2; 1 ]
let notSafe2 = [ 1; 3; 2; 4; 5 ]
let notSafe3 = [ 8; 6; 4; 4; 1 ]
let notSafe4 = [ 1; 3; 6; 7; 9 ]
