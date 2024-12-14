open Utilities

let newLineAlt = "_"
let space = len => `[A-Z${newLineAlt}]{${len->Int.toString}}`
let col = data => data->String.split("\n")->Array.at(0)->Option.getOr("")->String.length
let flattenData = data => data->String.replaceAll("\n", newLineAlt)

let run = (patarr, data) =>
  patarr
  ->Array.map(pattern =>
    data
    ->String.match(pattern)
    ->Option.mapOr(0, x => x->Array.filterMap(identity)->Array.length)
  )
  ->Array_.Int.sum

let q1 = data => {
  let col_ = col(data)
  let rowPatternGen = ((a, b, c, d): (string, string, string, string), offset: int) => {
    let gap = space(col_ + offset)
    RegExp.fromStringWithFlags(`(?=(${a}${gap}${b}${gap}${c}${gap}${d}))`, ~flags="gi")
  }
  let patGen = (a, b, c, d) =>
    [
      RegExp.fromStringWithFlags(`(${a}${b}${c}${d})`, ~flags="gi"),
      ...Array.fromInitializer(~length=3, i => i - 1)->Array.map(offset =>
        rowPatternGen((a, b, c, d), offset)
      ),
    ]

  [...Array.toReversed(patGen("X", "M", "A", "S")), ...patGen("S", "A", "M", "X")]->run(
    data->flattenData,
  )
}

let q2 = data => {
  let col_ = col(data)
  let patGen = ((a, b, d, e)) => {
    RegExp.fromStringWithFlags(
      `(?=(${a}${space(1)}${b}${space(col_ - 1)}A${space(col_ - 1)}${d}${space(1)}${e}))`,
      ~flags="gi",
    )
  }

  [
    patGen(("M", "M", "S", "S")),
    patGen(("S", "M", "S", "M")),
    patGen(("S", "S", "M", "M")),
    patGen(("M", "S", "M", "S")),
  ]->run(data->flattenData)
}
