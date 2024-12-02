open Utilities

let parseLine = str =>
  switch String.split(str, "   ")->Array.map(x => Int.fromString(x, ~radix=10)) {
  | [Some(a), Some(b)] => Some((a, b))
  | _ => None
  }

let parseFile = data =>
  String.split(data, "\n")
  ->Array.filterMap(parseLine)
  ->List.fromArray
  ->List.unzip

let q1 = data => {
  let parsed = parseFile(data)
  let (sorteda, sortedb) = Tuple2_.map(parsed, List_.Int.sortAsc)

  List.zip(sorteda, sortedb)
  ->List.map(Tuple2_.Int.diffAbs)
  ->List_.Int.sum
  ->Int.toString
}

let q2 = data => {
  let (lista, listb) = parseFile(data)
  let listBCounts = List_.histogram(listb)->Map_.map(List.length)

  List.filterMap(lista, x => Map.get(listBCounts, x)->Option.map(y => y * x))
  ->List_.Int.sum
  ->Int.toString
}
