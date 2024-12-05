open Utilities

let parseLine = str =>
  switch String.split(str, "   ")->Array.map(Int_.parse) {
  | [Some(a), Some(b)] => Some((a, b))
  | _ => None
  }

let parseFile = data =>
  String.split(data, "\n")
  ->Array.filterMap(parseLine)
  ->List.fromArray
  ->List.unzip

let q1 = data => {
  parseFile(data)
  ->Tuple2_.map(List_.Int.sortAsc)
  ->Tuple2_.toList
  ->List.map(Tuple2_.Int.diffAbs)
  ->List_.Int.sum
}

let q2 = data => {
  let (lista, listb) = parseFile(data)
  let listBCounts = List_.histogram(listb)->Map_.map(List.length)

  List.filterMap(lista, x => Map.get(listBCounts, x)->Option.map(y => x * y))->List_.Int.sum
}
