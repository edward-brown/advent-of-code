let input = System.IO.File.ReadLines("2022/4/input.txt")

let getParts (line: string) = line.Split ','

let getRange (sectionIds: string) =
    let ids = sectionIds.Split '-' |> Array.map int
    (ids[0], ids[1])

let pairs = input |> Seq.map (getParts >> Array.map getRange)

let doesContainAll (a1: int, a2: int) (b1: int, b2: int) =
    (a1 >= b1 && a2 <= b2) || (b1 >= a1 && b2 <= a2)

let doesContainAny (a1: int, a2: int) (b1: int, b2: int) =
    (a1 >= b1 && a1 <= b2) || (b1 >= a1 && b1 <= a2)

let part1 = pairs |> Seq.filter (fun x -> doesContainAll x[0] x[1]) |> Seq.length

let part2 = pairs |> Seq.filter (fun x -> doesContainAny x[0] x[1]) |> Seq.length
