let input = System.IO.File.ReadLines("2022/3/input.txt")

let toSet (a: string) : char Set = a |> Set.ofSeq

let getCompartments (rucksack: string) : (char Set * char Set) =
    let len = rucksack.Length / 2
    let compA = rucksack.Substring(0, len) |> toSet
    let compB = rucksack.Substring(len, len) |> toSet
    (compA, compB)

let letters = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ]

let priority (a: char) : int =
    (letters |> List.findIndex (fun x -> a = x)) + 1

let prioritize (compA: char Set, compB: char Set) =
    Set.intersect compA compB
    |> Set.map (fun x -> priority x)
    |> Set.toSeq
    |> Seq.sum

let part1 = input |> Seq.map (getCompartments >> prioritize) |> Seq.sum

let part2 =
    input
    |> Seq.chunkBySize 3
    |> Seq.map (Seq.map Set.ofSeq >> Set.intersectMany >> (Seq.sumBy priority))
    |> Seq.sum
