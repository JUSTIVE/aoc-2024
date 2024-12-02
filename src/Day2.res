open Utilities

let parseLine = line =>
  String.split(line, " ")
  ->Array.map(x => Int.fromString(x, ~radix=10))
  ->Array.map(Option.getUnsafe)
  ->List.fromArray

let parseFile = data => String.split(data, "\n")->Array.map(parseLine)

let q1 = data => {
  let parsed = parseFile(data)
  let checkCase = x => {
    let windows1 = List_.Int.slidingWindow(x)->List.map(Tuple2_.Int.diff)

    // windows->List.toArray->Js.log
    // windows->List.map(x => Js.Math.sign_int(x))->List_.Int.sum->Js.log
    // Js.log2(windows->List.length - 1, "")

    windows1->List.every(x => Js.Math.abs_int(x) <= 2) &&
      windows1->List.map(x => Js.Math.sign_int(x))->List_.Int.sum->Js.Math.abs_int ==
        x->List.length - 1
  }
  parsed->Array.filter(checkCase)->Array.length->Int.toString
}
