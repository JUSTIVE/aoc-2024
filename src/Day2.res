open Utilities

let parseLine = line => String_.toIntList(line)

let parseFile = data =>
  String.split(data, "\n")
  ->Array.filter(x => x != "")
  ->Array.map(parseLine)

let checkCase = x => {
  let window = List_.Int.slidingWindow(x)->List.map(Tuple2_.Int.diff)
  List.every(window, x => Js.Math.abs_int(x) <= 3) &&
  List.map(window, x => Js.Math.sign_int(x))
  ->List_.Int.sum
  ->Js.Math.abs_int == List.length(x) - 1
}

let q1 = data => {
  parseFile(data)
  ->Array.filter(checkCase)
  ->Array.length
}

let q2 = data => {
  parseFile(data)
  ->List.fromArray
  ->List.partition(checkCase)
  ->Tuple2_.mapSnd(snd =>
    List.filter(snd, x => List_.diagonalRemovedPermutations(x)->List.some(checkCase))
  )
  ->Tuple2_.map(List.length)
  ->Tuple2_.Int.sum
}
