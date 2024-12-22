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

let rec solve = (acc, oracle, values, useConcat) => {
  oracle >= acc &&
    switch values {
    | list{h, ...t} =>
      solve(acc +. h, oracle, t, useConcat) ||
      solve(acc *. h, oracle, t, useConcat) ||
      (useConcat &&
      solve(acc *. Math.pow(10., ~exp=Math.floor(Math.log10(h)) +. 1.) +. h, oracle, t, useConcat))
    | _ => acc == oracle
    }
}

let run = (data, useConcat) =>
  data
  ->parse
  ->Array.filterMap(((oracle, values)) => {
    switch values->List.fromArray {
    | list{h, ...t} => solve(h, oracle, t, useConcat) ? Some(oracle) : None
    | _ => None
    }
  })
  ->Array_.Float.sum
  ->Float.toString

let q1 = data => run(data, false)

let q2 = data => run(data, true)
