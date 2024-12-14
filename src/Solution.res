type prob = string => int
module type S = {
  let q1: prob
  let q2: prob
}
