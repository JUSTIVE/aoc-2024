let identity = x => x

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

  module Int = {
    let sum = a => List.reduce(a, 0, (a, b) => a + b)
    let sortAsc = a => List.sort(a, Core__Int.compare)
    let sortDesc = a => sortAsc(a)->List.reverse
    let slidingWindow = (a: list<int>) => List.zip(a, a->List.tail->Option.getOr(list{}))
  }
}

module Tuple2_ = {
  let map = ((a, b), f) => (f(a), f(b))
  module Int = {
    let diffAbs = ((a, b)) => Js.Math.abs_int(a - b)
    let diff = ((a, b)) => a - b
  }
}
