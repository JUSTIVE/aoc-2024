open NodeJs.Process
open Utilities

let runner = async (day, problem, session) => {
  (await DataSupply.getData(day, session))->switch (day, problem) {
  | (1, 1) => Day1.q1
  | (1, 2) => Day1.q2
  | (2, 1) => Day2.q1
  | (2, 2) => Day2.q2
  | _ => _ => "Invalid day or problem"
  }
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
