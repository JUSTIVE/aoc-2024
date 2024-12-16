open Utilities

module Rule = {
  type t = (int, int)

  let fromString = line =>
    switch line->String.split("|")->Array.filterMap(x => x->Int_.parse)->List.fromArray {
    | list{a, b} => Some((a, b))
    | _ => None
    }
  let components = ((a, b)) => Set.fromArray([a, b])
}

module RuleSet = {
  type t = array<Rule.t>
  let has = (rules, rule) => Array.some(rules, is(rule))
}

module Case = {
  type t = array<int>
  let fromString = line => line->String.split(",")->Array.filterMap(x => x->Int_.parse)
  let selectMiddleValue = case => case->Array.get(case->Array.length / 2)
  let validate = (case, (pre, post)) => {
    switch (case->Array.findIndex(is(pre)), case->Array.findIndex(is(post))) {
    | (-1, -1)
    | (_, -1)
    | (-1, _) => true
    | (preI, postI) => preI < postI
    }
  }
  let isRelevantRule = (case, (pre, post)) =>
    switch (case->Array.findIndex(is(pre)), case->Array.findIndex(is(post))) {
    | (-1, -1)
    | (_, -1)
    | (-1, _) => false
    | _ => true
    }

  let fix = (case, rules) => {
    let rules = rules->Array.filter(x => isRelevantRule(case, x))

    let list{(a, b), ...tail} =
      rules
      ->Array.filter(((a, b)) =>
        rules
        ->Array.map(Rule.components)
        ->Array.reduce(Set.fromArray([]), Set.union)
        ->Set.toArray
        ->Array.some(c => Array.some(rules, is((a, c))) && Array.some(rules, is((c, b))))
        ->Bool.negate
      )
      ->List.fromArray

    let sortByJoinable = (state, lists) =>
      lists
      ->List.partition(((a, b)) => {
        Array.at(state, 0) == Some(b) || Array.at(state, -1) == Some(a)
      })
      ->Tuple2_.List.join

    let rec combine = (state, neighbourNumbers: list<(int, int)>) =>
      switch neighbourNumbers {
      | list{(a, b), ...tail} =>
        let newState = Array.at(state, 0) == Some(b) ? [a, ...state] : [...state, b]
        combine(newState, sortByJoinable(newState, tail))
      | _ => state
      }
    combine([a, b], sortByJoinable([a, b], tail))
  }
}

let parseRuleSet = file => file->String.split("\n")->Array.filterMap(Rule.fromString)

let parseFile = file => {
  let [a, b] = file->String.split("\n\n")
  let rules = parseRuleSet(a)
  let cases = b->String.split("\n")->Array.map(Case.fromString)

  (rules, cases)
}

let q1 = data => {
  let (rules, cases) = parseFile(data)

  cases
  ->Array.filter(case => {
    rules->Array.every(rule => Case.validate(case, rule))
  })
  ->Array.filterMap(Case.selectMiddleValue)
  ->Array_.Int.sum
}

let q2 = data => {
  let (rules, cases) = data->parseFile
  cases
  ->Array_.reject(case => rules->Array.every(rule => Case.validate(case, rule)))
  ->Array.map(x => x->Case.fix(rules))
  ->Array.filterMap(Case.selectMiddleValue)
  ->Array_.Int.sum
}
