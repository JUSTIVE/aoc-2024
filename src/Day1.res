open Utilities

let data = RescriptBun.Fs.readFileSync("./src/Day1.txt")->Buffer.toString

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

let parsed = parseFile(data)
let (lista, listb) = parseFile(data)
let (sorteda, sortedb) = Tuple2_.map(parsed, List_.Int.sort)
let listBCounts = List_.histogram(listb)->Map_.map(List.length)

let q1 =
  List.zip(sorteda, sortedb)
  ->List.map(Tuple2_.Int.diff)
  ->List_.Int.sum
  ->Int.toString

let q2 =
  List.filterMap(lista, x => Map.get(listBCounts, x)->Option.map(y => y * x))
  ->List_.Int.sum
  ->Int.toString
