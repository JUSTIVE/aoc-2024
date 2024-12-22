open Utilities

let parseLine = str =>
  switch str {
  | "A X" => 3 + 1
  | "B Y" => 3 + 2
  | "C Z" => 3 + 3
  | "A Y" => 6 + 2
  | "B Z" => 6 + 3
  | "C X" => 6 + 1
  | "A Z" => 0 + 3
  | "B X" => 0 + 1
  | "C Y" => 0 + 2
  | _ => 0
  }

let q1 = data =>
  String.split(data, "\n")
  ->Array.map(parseLine)
  ->Array_.Int.sum
  ->Int.toString

let transformer = str =>
  switch str {
  | "A X" => "A Z"
  | "B Y" => "B Y"
  | "C Z" => "C X"
  | "A Y" => "A X"
  | "B Z" => "B Z"
  | "C X" => "C Y"
  | "A Z" => "A Y"
  | "B X" => "B X"
  | "C Y" => "C Z"
  | _ => str
  }

let q2 = data =>
  String.split(data, "\n")
  ->Array.map(transformer)
  ->Array.map(parseLine)
  ->Array_.Int.sum
  ->Int.toString
