open NodeJs.Process
open Utilities

let runner = async (year, day, problem, session) => {
  let data = await DataSupply.getData(year, day, session)
  switch year {
  | 2022 =>
    switch (day, problem) {
    | (2, 1) => Some(Day2_2022.q1)
    | (2, 2) => Some(Day2_2022.q2)
    | (3, 1) => Some(Day3_2022.q1)
    | _ => None
    }
  | 2024 =>
    switch (day, problem) {
    | (1, 1) => Some(Day1_2024.q1)
    | (1, 2) => Some(Day1_2024.q2)
    | (2, 1) => Some(Day2_2024.q1)
    | (2, 2) => Some(Day2_2024.q2)
    | (3, 1) => Some(Day3_2024.q1)
    | (3, 2) => Some(Day3_2024.q2)
    | (4, 1) => Some(Day4_2024.q1)
    | (4, 2) => Some(Day4_2024.q2)
    | (5, 1) => Some(Day5_2024.q1)
    | (5, 2) => Some(Day5_2024.q2)
    | (6, 1) => Some(Day6_2024.q1)
    | (6, 2) => Some(Day6_2024.q2)
    | (7, 1) => Some(Day7_2024.q1)
    | (7, 2) => Some(Day7_2024.q2)
    | _ => None
    }
  | _ => None
  }->Option.mapOr("Invalid day or problem", x => x(data))
}

let driver = async arg =>
  switch (String.split(arg, "-")->Array.map(Int_.parse), Bun.Env.get(Bun.env, "SESSION")) {
  | ([Some(year), Some(day), Some(problem)], Some(session)) =>
    await runner(year, day, problem, session)
  | (_, None) => "Invalid session"
  | _ => "Invalid argument"
  }

let start = performance.now()

(await Array.at(argv(process), 2)
->Option.mapOr(Promise.make((resolve, _) => resolve("No argument provided")), driver))
->Js.log

let end = performance.now()

Js.log(`Execution time: ${(end -. start)->Float.toString}ms`)
