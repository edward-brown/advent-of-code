let input = System.IO.File.ReadAllLines("2024/1/input.txt")

let part1  =
    input
    |> Seq.map (fun x -> (x.Split ' ') |> (fun a -> int a.[0], int a.[3])) 
    |> Seq.toList
    |> List.unzip
    |> (fun (x, y) -> List.zip (List.sort x) (List.sort y))
    |> Seq.sumBy (fun (x, y) -> (x - y) |> abs)

let part2  =
    input 
    |> Seq.map (fun x -> (x.Split ' ') |> (fun a -> int a.[0], int a.[3])) 
    |> Seq.toList
    |> List.unzip
    |> (fun (x, y) -> x |> List.sumBy(fun a -> a * (y |> List.filter(fun b -> a = b) |> List.length)))