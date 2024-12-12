open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines("2024/3/input.txt")

type token =
    | Mul of (int * int)
    | Do 
    | Dont

type acc = {
    enabled: bool
    results: (int * int) seq
}

let tokenize (line: string) : token seq =
    let mutable m = Regex.Match(line, @"(mul\((?<a>[0-9]+),(?<b>[0-9]+)\)|do\(\)|don't\(\))")
    [
        while m.Success do
            match m.ToString() with
            | "do()" -> Do
            | "don't()" -> Dont
            | _ -> 
                let a = int m.Groups.["a"].Value
                let b = int m.Groups.["b"].Value
                Mul (a, b)
            m <- m.NextMatch()
    ]

let part1 =
    input
    |> Seq.map (fun x -> tokenize x |> Seq.choose (function | Mul (a,b) -> Some (a, b) | _ -> None))
    |> Seq.concat
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.sum

let part2 =
    input
    |> Seq.map (tokenize)
    |> Seq.concat
    |> Seq.fold (fun (acc: acc) x -> 
        match x with
        | Do when not acc.enabled -> { acc with enabled = true }
        | Dont when acc.enabled -> { acc with enabled = false }
        | Mul(a, b) when acc.enabled -> { acc with results = Seq.append acc.results [(a, b)]  }
        | _ -> acc
    ) { enabled = true; results = [] }
    |> fun x -> x.results
    |> Seq.map (fun (a, b) -> a * b)
    |> Seq.sum