let input = System.IO.File.ReadAllLines("2023/1/input.txt")

let letters = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ]

let words =
    [ "one", 1
      "two", 2
      "three", 3
      "four", 4
      "five", 5
      "six", 6
      "seven", 7
      "eight", 8
      "nine", 9 ]

let getValue (line: string) : int =
    line
    |> Seq.filter (fun x -> letters |> List.contains x |> not)
    |> fun x -> sprintf "%c%c" (Seq.head x) (Seq.last x)
    |> int

let replace (line: string) : string =
    words
    |> Seq.fold
        (fun acc (word, value) ->
            match acc.IndexOf(word) with
            | -1 -> acc
            | i -> acc.Remove(i, word.Length - 1).Insert(i, string value))
        line

let part2 (line: string) : string =
    line
    |> Seq.fold
        (fun acc x ->
            let b = sprintf "%s%c" acc x
            replace b)
        ""

input |> Seq.map (part2 >> getValue) |> Seq.sum

// Correct 54845
