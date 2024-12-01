let identity = x => x
let sum = (a: List.t<int>) => List.reduce(a, 0, (a, b) => a + b)
let histogram = (a: List.t<'a>) =>
  List.reduce(a, Map.make(), (acc, x) => {
    let newValue =
      acc
      ->Map.get(x)
      ->Option.mapOr(list{x}, y => List.add(y, x))
    Map.set(acc, x, newValue)
    acc
  })

let intDiff = (a, b) => Js.Math.abs_int(a - b)
