let identity = x => x

module List_ = {
  let histogram = (a: List.t<'a>) =>
    List.reduce(a, Map.make(), (acc, x) => {
      let newValue =
        acc
        ->Map.get(x)
        ->Option.mapOr(list{x}, y => List.add(y, x))
      Map.set(acc, x, newValue)
      acc
    })

  module Int = {
    let sum = a => List.reduce(a, 0, (a, b) => a + b)
    let sort = a => List.sort(a, Core__Int.compare)
  }
}

module Tuple2_ = {
  let map = ((a, b), f) => (f(a), f(b))
  module Int = {
    let diff = ((a, b)) => Js.Math.abs_int(a - b)
  }
}

module Map_ = {
  let map = (a, f) =>
    Map.entries(a)
    ->Array.fromIterator
    ->Array.map(((k, v)) => (k, f(v)))
    ->Map.fromArray
}
