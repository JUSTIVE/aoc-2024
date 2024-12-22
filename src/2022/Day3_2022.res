open Utilities
let parse = str => {
  let len = String.length(str)
  (str->String.slice(~start=0, ~end=len / 2), str->String.sliceToEnd(~start=len / 2))
}

let parts = str => String.split(str, "")->Set.fromArray

let evalChar = str =>
  str->String.toLowerCase == str
    ? str->String.charCodeAt(0)->Int.fromFloat - Char.code('a') + 1
    : str->String.charCodeAt(0)->Int.fromFloat - Char.code('A') + 27

let q1 = data => {
  String.split(data, "\n")
  ->Array.map(parse)
  ->Array.map(x => {
    let (a, b) = Tuple2_.map(x, parts)
    Set.intersection(a, b)
    ->Set.toArray
    ->Array.map(evalChar)
    ->Array_.Int.sum
  })
  ->Array_.Int.sum
  ->Int.toString
}

// evalChar('z')->Js.log
// evalChar('Z')->Js.log
