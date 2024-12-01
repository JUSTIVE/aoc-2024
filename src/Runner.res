open NodeJs.Process

let runner = (day, problem) =>
  switch (day, problem) {
  | (1, 1) => Day1.q1
  | (1, 2) => Day1.q2
  | _ => "Invalid day or problem"
  }

switch argv(process)->Array.at(2) {
| Some(arg) =>
  switch String.split(arg, "-")->Array.map(x => x->Int.fromString(~radix=10)) {
  | [Some(day), Some(problem)] => runner(day, problem)
  | _ => "Invalid argument"
  }
| None => "No argument provided"
}->Js.log
