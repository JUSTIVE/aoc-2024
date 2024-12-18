// NodeJs.Fs.readdirSync()
open Utilities

module NameMap = {
  type t = Js.Dict.t<Js.Dict.t<string>>
  let get = async (year, day) => {
    let cachedMap = switch Fs.readFileSync("nameMap.json")->Buffer.toString->JSON.parseExn {
    | Object(o) => Some(o)
    | _ => None
    }

    switch cachedMap
    ->Option.flatMap(c =>
      switch c->Js.Dict.get(year) {
      | Some(Object(o)) => Some(o)
      | _ => None
      }
    )
    ->Option.flatMap(y =>
      switch y->Js.Dict.get(day) {
      | Some(String(s)) => Some(s)
      | _ => None
      }
    ) {
    | Some(name) => name
    | None =>
      (await (await fetch(`https://adventofcode.com/${year}/day/${day}`))
      ->Response.text)
      ->String.match(%re("/Day \d:\s(.*)\s?---/"))
      ->Option.flatMap(x => x->Array.at(1))
      ->Option.flatMap(identity)
      ->Option.getOr("")
    }
  }
}

// (await NameMap.get("2024", "6"))->Js.log

module Problem = {
  type t = {
    day: int,
    name: string,
    parts: (option<int>, option<int>),
  }
  let make = (day, name, parts) => {
    {day, name, parts}
  }

  let fromFileNameAsync = async (fileName, year) => {
    let dayNumber =
      fileName
      ->String.match(%re("/Day(\d+)_/"))
      ->Option.flatMap(x =>
        x
        ->Array.at(1)
        ->Option.flatMap(identity)
        ->Option.getOr("0")
        ->Int_.parse
      )

    let name = switch (year, dayNumber) {
    | (Some(year), Some(day)) => Some(await NameMap.get(year->Int.toString, day->Int.toString))
    | _ => None
    }

    let fileContent =
      Fs.readFileSync(`src/${year->Option.mapOr("0", x => Int.toString(x))}/${fileName}`)
      ->Buffer.toString
      ->String.split("\n")
      ->Array.mapWithIndex((x, i) => (x, i))

    let getPart = i =>
      fileContent
      ->Array.find(((x, _)) => x->String.includes(`let q${i->Int.toString} =`))
      ->Option.map(((_, i)) => i + 1)
    let part1 = getPart(1)
    let part2 = getPart(2)

    switch (dayNumber, name) {
    | (Some(day), Some(name)) => Some(make(day, name, (part1, part2)))
    | _ => None
    }
  }

  let toMarkdown = ({day, name, parts: (part1, part2)}: t, year) => {
    let partPermLink = (part, line) =>
      line->Option.mapOr("---", line =>
        `[part${part->Int.toString}](https://github.com/JUSTIVE/aoc-2024/tree/main/src/${year}/Day${day->Int.toString}_${year}.res#L${line->Int.toString})`
      )

    `| ${day->Int.toString} | [${name}](https\://adventofcode.com/${year}/day/${day->Int.toString}) | ${partPermLink(
        1,
        part1,
      )} | ${partPermLink(2, part2)} |`
  }
}

// (await Problem.fromFileNameAsync("src/2024/Day1_2024.res"))->Js.log

module Year = {
  type t = {
    year: int,
    problems: array<Problem.t>,
  }
  let make = (year, problems) => {year, problems}

  let toMarkdown = ({year, problems}: t) => {
    `## ${year->Int.toString}

| Day | title | q1 | q2 |
| --- | --- | --- | --- |
${problems
      ->Array.map(prob => prob->Problem.toMarkdown(year->Int.toString))
      ->Array.join("\n")}\n`
  }

  let fromDirNameAsync = async dirName => {
    let year = dirName->Int_.parse->monitor
    let problems =
      (await Fs.readdirSync(`src/${dirName}`)
      ->monitor
      ->Array.filter(x => x->String.endsWith(".res"))
      ->Array.map(x => Problem.fromFileNameAsync(x, year))
      ->Promise.allSettled)
      ->Array.filterMap(x =>
        switch x {
        | Fulfilled(p) => p.value
        | _ => None
        }
      )
      ->monitor

    switch year {
    | Some(year) => Some(make(year, problems))
    | _ => None
    }
  }
}

let content =
  (await Fs.readdirSync("src")
  ->Array.filterMap(Int_.parse)
  ->Array.map(Int_.toString)
  ->monitor
  ->Array.map(x => Year.fromDirNameAsync(x))
  ->monitor
  ->Promise.allSettled)
  ->Array.filterMap(x =>
    switch x {
    | Fulfilled(y) => y.value
    | _ => None
    }
  )
  ->monitor
  ->Array.map(Year.toMarkdown)
  ->Array.join("\n\n")
  ->(x => `# aoc\n\n${x}`)

(
  await RescriptBun.Bun.Write.write(
    ~destination=RescriptBun.Bun.Write.Destination.fromPath("README.md"),
    ~options={},
    ~input=RescriptBun.Bun.Write.Input.fromString(content),
  )
)->ignore
