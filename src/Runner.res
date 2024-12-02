open NodeJs.Process

let runner = async (day, problem) => {
  (await DataSupply.getData(day))->switch (day, problem) {
  | (1, 1) => Day1.q1
  | (1, 2) => Day1.q2
  | (2, 1) => Day2.q1
  | _ => _ => "Invalid day or problem"
  }
}

(await Array.at(argv(process), 2)
->Option.mapOr(Promise.make((resolve, _reject) => resolve("No argument provided")), async arg =>
  switch String.split(arg, "-")->Array.map(x => x->Int.fromString(~radix=10)) {
  | [Some(day), Some(problem)] => await runner(day, problem)
  | _ => "Invalid argument"
  }
))
->Js.log
