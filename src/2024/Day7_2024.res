open Utilities
let parse = data =>
  data
  ->String.split("\n")
  ->Array.filterMap(x =>
    switch x->String.split(":")->List.fromArray {
    | list{h, t} =>
      h
      ->Float_.parse
      ->Option.map(h => (
        h,
        String.trim(t)
        ->String.split(" ")
        ->Array.filterMap(Float_.parse),
      ))
    | _ => None
    }
  )

let rec solve = (acc, oracle, values) => {
  oracle >= acc &&
    switch values {
    | list{h, ...t} => solve(acc +. h, oracle, t) || solve(acc *. h, oracle, t)
    | _ => acc == oracle
    }
}

let q1 = data => {
  data
  ->parse
  ->Array.filterMap(((oracle, values)) => {
    switch values->List.fromArray {
    | list{h, ...t} => solve(h, oracle, t) ? Some(oracle) : None
    | _ => None
    }
  })
  ->monitor
  ->Array_.Float.sum
  ->Float.toString
}
