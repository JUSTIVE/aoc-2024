open Utilities
module Direction = {
  type t = [#Left | #Right | #Up | #Down]
  let fromString = x =>
    switch x {
    | "^" => Some(#Up)
    | ">" => Some(#Right)
    | "<" => Some(#Left)
    | "v" => Some(#Down)
    | _ => None
    }
  let turnRight = dir =>
    switch dir {
    | #Up => #Right
    | #Right => #Down
    | #Down => #Left
    | #Left => #Up
    }
}

module Guard = {
  type t = ((int, int), Direction.t)

  let walk = (((x, y), dir): t) => {
    switch dir {
    | #Up => (x, y - 1)
    | #Down => (x, y + 1)
    | #Left => (x - 1, y)
    | #Right => (x + 1, y)
    }
  }
  let turn = (((x, y), dir)) => ((x, y), Direction.turnRight(dir))
}

module World = {
  type t = {
    // walls: Map.t<int,Map.t<int,string>>,
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
  let rec march = (history, map: World.t) => {
    let (_, dir) = map.guard
    let newHistory = history->List.add(map.guard)
    let nextMove = Guard.walk(map.guard)

    switch (World.boundaryCheck(map, nextMove), World.isWall(map, nextMove)) {
    | (false, _) => newHistory
    | (_, true) => march(newHistory, World.updateGuard(map, Guard.turn(map.guard)))
    | (_, false) => march(newHistory, World.updateGuard(map, (nextMove, dir)))
    }
  }

  data
  ->World.fromString
  ->Option.mapOr(0, map =>
    march(list{}, map)
    ->List.toArray
    ->Array.map((((x, y), _)) => `${Int.toString(x)}-${Int.toString(y)}`)
    ->Set.fromArray
    ->Set.size
  )
  ->Int.toString
}

let q2 = data => {
  let rec loopCheck = (
    history: Core__Map.t<int, Core__Map.t<int, list<Direction.t>>>,
    map: World.t,
  ) => {
    let ((x, y), dir) = map.guard
    let nextMove = Guard.walk(map.guard)
    let (nx, ny) = nextMove
    Map.get(history, nx)
    ->Option.flatMap(h => Map.get(h, ny))
    ->Option.flatMap(h => List.find(h, x => x == dir))
    ->Option.isNone
      ? World.boundaryCheck(map, nextMove)
          ? loopCheck(
              Map_.update(
                history,
                x,
                Map.get(history, x)->Option.mapOr(Map.fromArray([(y, list{dir})]), xh =>
                  Map_.update(
                    xh,
                    y,
                    Map.get(xh, y)->Option.mapOr(list{dir}, yh => List.add(yh, dir)),
                  )
                ),
              ),
              World.updateGuard(
                map,
                World.isWall(map, nextMove) ? Guard.turn(map.guard) : (nextMove, dir),
              ),
            )
          : false
      : true
  }

  data
  ->World.fromString
  ->Option.mapOr(0, map => {
    World.empty(data)
    ->Array.map(x => World.updateWall(map, x))
    ->Array.filterWithIndex((x, i) => {
      Js.log(`${i->Int.toString}\r`)
      loopCheck(Map.fromArray([]), x)
    })
    ->Array.length
  })
  ->Int.toString
}
