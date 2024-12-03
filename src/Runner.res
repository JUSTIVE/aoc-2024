open NodeJs.Process
open Utilities

let runner = async (day, problem, session) => {
  (await DataSupply.getData(day, session))
  ->switch (day, problem) {
  | (1, 1) => x => Some(Day1.q1(x))
  | (1, 2) => x => Some(Day1.q2(x))
  | (2, 1) => x => Some(Day2.q1(x))
  | (2, 2) => x => Some(Day2.q2(x))
  | (3, 1) => x => Some(Day3.q1(x))
  | (3, 2) => x => Some(Day3.q2(x))
  | _ => _ => None
  }
  ->Option.mapOr("Invalid day or problem", Int_.toString)
}

let driver = async arg =>
  switch (String.split(arg, "-")->Array.map(Int_.parse), Bun.Env.get(Bun.env, "SESSION")) {
  | ([Some(day), Some(problem)], Some(session)) => await runner(day, problem, session)
  | (_, None) => "Invalid session"
  | _ => "Invalid argument"
  }

(await Array.at(argv(process), 2)
->Option.mapOr(Promise.make((resolve, _) => resolve("No argument provided")), driver))
->Js.log
