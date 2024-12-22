open Utilities

type token =
  | M
  | U
  | L
  | LParen
  | Val(int)
  | Comma
  | RParen
  | Unknown
  | D
  | O
  | N
  | T
  | Apostrophe

type operation = Mul(int, int) | Do | Dont

type evalCtx = {
  do: bool,
  considerDo: bool,
}
let rec eval = (opl: list<operation>, (acc, ctx: evalCtx)): int =>
  switch opl {
  | list{head, ...tail} =>
    switch head {
    | Mul(a, b) => eval(tail, (ctx.do || !ctx.considerDo ? acc + a * b : acc, ctx))
    | Do => eval(tail, (acc, {...ctx, do: true}))
    | Dont => eval(tail, (acc, {...ctx, do: false}))
    }
  | _ => acc
  }

let rec concatVal = (val: string, acc: list<string>): (token, list<string>) => {
  switch acc {
  | list{head, ...tail} =>
    switch head {
    | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => concatVal(val ++ head, tail)
    | _ => (val->Int_.parse->Option.mapOr(Unknown, x => Val(x)), acc)
    }
  | _ => (val->Int_.parse->Option.mapOr(Unknown, x => Val(x)), list{})
  }
}

let rec tokenize = (str, ~acc=list{}) =>
  switch str {
  | list{ht, ...tl} => {
      let (newToken, tl) = switch ht {
      | "m" => (M, tl)
      | "u" => (U, tl)
      | "l" => (L, tl)
      | "(" => (LParen, tl)
      | ")" => (RParen, tl)
      | "," => (Comma, tl)
      | "d" => (D, tl)
      | "o" => (O, tl)
      | "n" => (N, tl)
      | "t" => (T, tl)
      | "'" => (Apostrophe, tl)
      | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => concatVal(ht, tl)
      | _ => (Unknown, tl)
      }
      tokenize(tl, ~acc=list{newToken, ...acc})
    }
  | _ => List.reverse(acc)
  }

let rec parse = (tkl, ~acc=list{}): list<operation> =>
  switch tkl {
  | list{M, U, L, LParen, Val(a), Comma, Val(b), RParen, ...tl} =>
    parse(tl, ~acc=list{Mul(a, b), ...acc})
  | list{D, O, N, Apostrophe, T, LParen, RParen, ...tl} => parse(tl, ~acc=list{Dont, ...acc})
  | list{D, O, LParen, RParen, ...tl} => parse(tl, ~acc=list{Do, ...acc})
  | list{_, ...tl} => parse(tl, ~acc)
  | _ => List.reverse(acc)
  }

let solve = (data, considerDo) =>
  data
  ->String.split("")
  ->List.fromArray
  ->tokenize
  ->parse
  ->eval((0, {do: true, considerDo}))
  ->Int.toString

let q1 = data => solve(data, false)

let q2 = data => solve(data, true)
