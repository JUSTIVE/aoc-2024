let parseLine = str =>
  switch String.split(str, "   ")->Array.map(x => x->Int.fromString(~radix=10)) {
  | [Some(a), Some(b)] => Some((a, b))
  | _ => None
  }

let parseFile = data =>
  String.split(data, "\n")
  ->Array.filterMap(parseLine)
  ->List.fromArray
  ->List.unzip

let data = RescriptBun.Fs.readFileSync("./src/Day1.txt")->Buffer.toString

let (lista, listb) = parseFile(data)
let polish = x => List.sort(x, Int.compare)

let listBHistogram = Utilities.histogram(listb)

let q1 =
  List.zip(polish(lista), polish(listb))
  ->List.map(((a, b)) => Utilities.intDiff(a, b))
  ->Utilities.sum
  ->Int.toString

let q2 =
  lista
  ->List.filterMap(x =>
    listBHistogram
    ->Map.get(x)
    ->Option.map(y => List.length(y) * x)
  )
  ->Utilities.sum
  ->Int.toString
