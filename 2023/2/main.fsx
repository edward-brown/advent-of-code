open System.Text.RegularExpressions
open System

let input = System.IO.File.ReadAllLines("2023/2/input.txt")

type Hand = { red: int; green: int; blue: int }

let bestHand = { red = 12; green = 13; blue = 14 }

let handPossible (hand: Hand) : bool =
    hand.red <= bestHand.red
    && hand.green <= bestHand.green
    && hand.blue <= bestHand.blue

let gamePossible (hands: Hand seq) : bool =
    hands |> Seq.map handPossible |> Seq.forall id

let matchHand (color: string) (hand: string) : int option =
    let m = Regex.Match(hand, $@"([0-9]+) {color}")

    match m.Success with
    | true -> Some(int m.Groups.[1].Value)
    | false -> None

let parseHand (hand: string) : Hand =
    let r = matchHand "red" hand
    let g = matchHand "green" hand
    let b = matchHand "blue" hand

    { red = Option.defaultValue 0 r
      green = Option.defaultValue 0 g
      blue = Option.defaultValue 0 b }

let parseGame (game: string) : Hand seq = game.Split(';') |> Seq.map parseHand

let part1 =
    input
    |> Seq.map (parseGame >> gamePossible)
    |> Seq.mapi (fun i x -> if x then i + 1 else 0)
    |> Seq.sum

let part2 =
    input
    |> Seq.map (parseGame)
    |> Seq.map (
        Seq.fold
            (fun acc x ->
                { red = if x.red > acc.red then x.red else acc.red
                  green = if x.green > acc.green then x.green else acc.green
                  blue = if x.blue > acc.blue then x.blue else acc.blue })
            { red = 0; green = 0; blue = 0 }
    )
    |> Seq.map (fun x -> x.red * x.green * x.blue)
    |> Seq.sum
