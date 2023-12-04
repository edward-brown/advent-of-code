let input = System.IO.File.ReadAllLines("2023/3/input.txt") |> Array.toList

let isSymbol (c: char) : bool =
    match c with
    | c when System.Char.IsDigit(c) -> false
    | '.' -> false
    | _ -> true

// Make this nicer
let isAdj (x: int) (y: int) (lines: string list) : bool =
    let mutable r = []

    r <- if x > 0 then r @ [ lines.[y].[x - 1] ] else r // left

    r <-
        if x < lines.[y].Length - 1 then
            r @ [ lines.[y].[x + 1] ]
        else
            r // Right

    r <- if y > 0 then r @ [ lines.[y - 1].[x] ] else r // Up

    r <-
        if y < lines.Length - 1 then
            r @ [ lines.[y + 1].[x] ]
        else
            r // Down

    r <- if x > 0 && y > 0 then r @ [ lines.[y - 1].[x - 1] ] else r // Up Left

    r <-
        if x < lines.[y].Length - 1 && y > 0 then
            r @ [ lines.[y - 1].[x + 1] ]
        else
            r // Up Right

    r <-
        if x > 0 && y < lines.Length - 1 then
            r @ [ lines.[y + 1].[x - 1] ]
        else
            r // Down Left

    r <-
        if x < lines.[y].Length - 1 && y < lines.Length - 1 then
            r @ [ lines.[y + 1].[x + 1] ]
        else
            r // Down Right

    r |> Seq.map (isSymbol) |> Seq.filter (fun x -> x) |> Seq.length > 0

let checkNumber (indexs: (int * int) list) (lines: string list) : bool =
    indexs
    |> List.map (fun (x, y) -> isAdj x y lines)
    |> List.filter (fun x -> x)
    |> List.length > 0

type Acc =
    { index: int
      numbers: int list
      currentNumberIndex: (int * int) list
      currentNumber: int }

let part1 =
    input
    |> Seq.mapi (fun i x ->
        x
        |> Seq.fold
            (fun (acc: Acc) c ->
                match c with
                | '.' ->
                    match acc.currentNumber with
                    | 0 -> { acc with index = acc.index + 1 }
                    | _ ->
                        match checkNumber acc.currentNumberIndex input with
                        | true ->
                            { acc with
                                index = acc.index + 1
                                numbers = acc.numbers @ [ acc.currentNumber ]
                                currentNumberIndex = []
                                currentNumber = 0 }
                        | false ->
                            { acc with
                                index = acc.index + 1
                                currentNumberIndex = []
                                currentNumber = 0 }
                | digit when System.Char.IsDigit(digit) ->
                    { acc with
                        index = acc.index + 1
                        currentNumberIndex = acc.currentNumberIndex @ [ (acc.index, i) ]
                        currentNumber = int (sprintf "%d%c" acc.currentNumber digit) }
                | c when isSymbol c ->
                    match acc.currentNumber with
                    | 0 -> { acc with index = acc.index + 1 }
                    | _ ->
                        match checkNumber acc.currentNumberIndex input with
                        | true ->
                            { acc with
                                index = acc.index + 1
                                numbers = acc.numbers @ [ acc.currentNumber ]
                                currentNumberIndex = []
                                currentNumber = 0 }
                        | false ->
                            { acc with
                                index = acc.index + 1
                                currentNumberIndex = []
                                currentNumber = 0 }
                | _ -> { acc with index = acc.index + 1 })
            { index = 0
              numbers = []
              currentNumberIndex = []
              currentNumber = 0 })
    |> Seq.map (fun x ->
        match x.currentNumberIndex.Length > 0 with
        | true ->
            match checkNumber x.currentNumberIndex input with
            | true -> x.numbers @ [ x.currentNumber ]
            | false -> x.numbers
        | false -> x.numbers)
    |> Seq.fold (fun acc x -> acc @ x) []
    |> Seq.sum
