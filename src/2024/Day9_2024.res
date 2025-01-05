open Utilities
module Disc = {
  type t = array<option<float>>

  let fromString = x => {
    let rec gen = (l, acc, isValue, v) => {
      switch l {
      | list{h, ...t} =>
        isValue
          ? gen(
              t,
              Array.concat(acc, Array.make(~length=h->Float.toInt, Some(v))),
              !isValue,
              v +. 1.,
            )
          : gen(t, Array.concat(acc, Array.make(~length=h->Float.toInt, None)), !isValue, v)
      | _ => acc
      }
    }
    let blocks =
      x
      ->String.split("")
      ->Array.filterMap(Float_.parse)
      ->List.fromArray
      ->gen([], true, 0.)

    blocks
  }
  let render = (x: t) => {
    x
    ->Array.map(x => {
      switch x {
      | Some(x) => x->Float.toString
      | None => " "
      }
    })
    ->Array.join("")
  }
  let compact = (x: t) => {
    let rec work = x => {
      // Js.log(x->render)
      x->Array.length == x->Array.filterMap(identity)->Array.length
        ? x
        : {
            let trimmed =
              x
              ->Array.at(-1)
              ->Option.flatMap(identity)
              ->Option.mapOr(x->Array.toReversed->Array_.dropWhile(identity)->Array.toReversed, _ =>
                x
              )
            let first = trimmed->Array.findIndex(x => x == None)
            let last =
              trimmed
              ->Array.at(-1)
              ->Option.flatMap(identity)
            work(
              trimmed
              ->Array.with(first, last)
              ->Array.slice(~start=0, ~end=trimmed->Array.length - 1),
            )
          }
    }

    work(x)
  }

  let checksum = (t: t) =>
    t
    ->Array.filterMap(identity)
    ->Array.mapWithIndex((x, i) => x *. i->Int.toFloat)
    ->Array_.Float.sum
}

let q1 = (data: string) => {
  data
  ->Disc.fromString
  ->Disc.compact
  ->Disc.checksum
  ->Float.toString
}
