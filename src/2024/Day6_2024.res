module Direction = {
  type t = Left | Right | Up | Down
  let fromString = x =>
    switch x {
    | "^" => Some(Up)
    | ">" => Some(Right)
    | "<" => Some(Left)
    | "v" => Some(Down)
    | _ => None
    }
  let turnRight = dir =>
    switch dir {
    | Up => Right
    | Right => Down
    | Down => Left
    | Left => Up
    }
}

module Guard = {
  type t = ((int, int), Direction.t)

  let walk = (((x, y), dir): t) => {
    switch dir {
    | Up => (x, y - 1)
    | Down => (x, y + 1)
    | Left => (x - 1, y)
    | Right => (x + 1, y)
    }
  }
  let turn = ((x, y), dir) => ((x, y), Direction.turnRight(dir))
}

module Map = {
  type t = {
    walls: array<((int, int), string)>,
    height: int,
    width: int,
    guard: Guard.t,
  }

  let empty = data => {
    data
    ->String.split("\n")
    ->Array.map(x => String.split(x, ""))
    ->Array.mapWithIndex((row, y) => row->Array.mapWithIndex((a, x) => ((x, y), a)))
    ->Array.flat
    ->Array.filter((((_, _), a)) => a == ".")
  }

  let fromString = data => {
    let width = data->String.split("\n")->Array.at(0)->Option.getOr("")->String.length
    let height = data->String.split("\n")->Array.length
    let coorded =
      data
      ->String.split("\n")
      ->Array.map(x => String.split(x, ""))
      ->Array.mapWithIndex((row, y) => row->Array.mapWithIndex((a, x) => ((x, y), a)))
      ->Array.flat
    let walls = Array.filter(coorded, (((_, _), a)) => a == "#")

    let guard =
      coorded
      ->Array.map((((x, y), a)) => ((x, y), Direction.fromString(a)))
      ->Array.filterMap((((x, y), a)) => {
        switch a {
        | Some(a) => Some(((x, y), a))
        | _ => None
        }
      })
      ->Array.at(0)

    Option.map(guard, guard => {walls, height, width, guard})
  }
  let updateGuard = (map, guard) => {...map, guard}
  let boundaryCheck = ({width, height}, (x, y)) => {
    x >= 0 && x < width && y >= 0 && y < height
  }

  let isWall = (map, (x, y)) =>
    map.walls->Array.find((((x_, y_), _)) => x == x_ && y == y_)->Option.isSome

  let updateWall = (map, ((x, y), a)) => {
    ...map,
    walls: [...map.walls, ((x, y), a)],
  }
}

let q1 = data => {
  let rec march = (history, map: Map.t) => {
    let ((x, y), dir) = map.guard
    let newHistory = history->List.add(map.guard)
    let nextMove = Guard.walk(map.guard)

    switch (Map.boundaryCheck(map, nextMove), Map.isWall(map, nextMove)) {
    | (false, _) => newHistory
    | (_, true) => march(newHistory, Map.updateGuard(map, ((x, y), Direction.turnRight(dir))))
    | (_, false) => march(newHistory, Map.updateGuard(map, (nextMove, dir)))
    }
  }

  data
  ->Map.fromString
  ->Option.mapOr(0, map =>
    march(list{}, map)
    ->List.toArray
    ->Array.map((((x, y), _)) => `${Int.toString(x)}-${Int.toString(y)}`)
    ->Set.fromArray
    ->Set.size
  )
}

let q2 = data => {
  let rec loopCheck = (history, map: Map.t) => {
    let ((x, y), dir) = map.guard
    let newHistory = history->List.add(map.guard)
    let nextMove = Guard.walk(map.guard)

    switch history->List.find((((x_, y_), dir_)) => x_ == x && y_ == y && dir_ == dir) {
    | Some(_) => true
    | _ =>
      switch (Map.boundaryCheck(map, nextMove), Map.isWall(map, nextMove)) {
      | (false, _) => false
      | (_, true) => loopCheck(newHistory, Map.updateGuard(map, ((x, y), Direction.turnRight(dir))))
      | (_, false) => loopCheck(newHistory, Map.updateGuard(map, (nextMove, dir)))
      }
    }
  }

  data
  ->Map.fromString
  ->Option.mapOr(0, map => {
    Map.empty(data)
    ->Array.map(x => Map.updateWall(map, x))
    ->Array.filterWithIndex((x, i) => {
      Js.log(`${i->Int.toString}\r`)
      loopCheck(list{}, x)
    })
    ->Array.length
  })
}
