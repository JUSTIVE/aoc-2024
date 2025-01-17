let identity = x => x
let monitor = x => {
  Js.log(x)
  x
}

let is = x => y => x == y
module Bool = {
  type t = bool
  let negate = x => !x
}

module Map_ = {
  let map = (a, f) =>
    Map.entries(a)
    ->Array.fromIterator
    ->Array.map(((k, v)) => (k, f(v)))
    ->Map.fromArray
  let update = (m, k, v) => {
    Map.set(m, k, v)
    m
  }
}

module Set_ = {
  let update = (s, v) => {
    let x = Set.toArray(s)
    Set.fromArray([...x, v])
  }
}

module Array_ = {
  module Int = {
    let sum = a => Array.reduce(a, 0, (a, b) => a + b)
    let sortAsc = a => Array.toSorted(a, Core__Int.compare)
    let sortDesc = a => sortAsc(a)->Array.toReversed
  }
  module Float = {
    let sum = a => Array.reduce(a, 0., (a, b) => a +. b)
  }
  module BigInt = {
    let sum = a => Array.reduce(a, 0n, (a, b) => Core__BigInt.add(a, b))
  }
  let reject = (a, f) => Array.filter(a, x => !f(x))
  let dropWhile = (a, f) => {
    let res = ref([])
    let flaggd = ref(false)
    Array.forEach(a, x => {
      switch f(x) {
      | Some(_) => res.contents->Array.push(x)
      | None =>
        if flaggd.contents {
          res.contents->Array.push(x)
        }
        flaggd.contents = true
      }
    })
    res.contents
  }
}

module List_ = {
  let histogram = (a: List.t<'a>) =>
    List.reduce(a, Map.make(), (acc, x) =>
      Map_.update(
        acc,
        x,
        acc
        ->Map.get(x)
        ->Option.mapOr(list{x}, y => List.add(y, x)),
      )
    )
  let diagonalRemovedPermutations = x =>
    List.mapWithIndex(x, (_, i) => x->List.filterWithIndex((_, j) => i != j))

  module Int = {
    let sum = a => List.reduce(a, 0, (a, b) => a + b)
    let sortAsc = a => List.sort(a, Core__Int.compare)
    let sortDesc = a => sortAsc(a)->List.reverse
    let slidingWindow = (a: list<int>) => List.zip(a, a->List.tail->Option.getOr(list{}))
  }
}

module Tuple2_ = {
  let map = ((a, b), f) => (f(a), f(b))
  let fold = ((a, b), f) => f(a, b)
  let mapSnd = ((a, b), f) => (a, f(b))
  let toList = ((a, b)) => List.zip(a, b)
  module Int = {
    let diffAbs = ((a, b)) => Js.Math.abs_int(a - b)
    let diff = ((a, b)) => a - b
    let sum = ((a, b)) => a + b
  }
  module List = {
    let join = ((a, b)) => list{...a, ...b}
  }
}

module Int_ = {
  let parse = x => Int.fromString(x, ~radix=10)
  let toString = x => Int.toString(x, ~radix=10)
}

module Float_ = {
  let parse = x => Float.fromString(x)
}

module String_ = {
  let toIntList = x =>
    String.splitByRegExp(x, %re("/\s+/"))
    ->Array.filterMap(x => Option.flatMap(x, Int_.parse))
    ->List.fromArray
  let updateAt = (s, i, v) => {
    s->String.substring(~start=0, ~end=i) ++ v ++ s->String.substringToEnd(~start=i + 1)
  }
}

module Js_ = {
  module Dict = {
    let update = (d, k, v) => {
      d->Js.Dict.set(k, v)
      d
    }
  }
}
