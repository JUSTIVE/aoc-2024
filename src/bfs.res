module Tree = {
  type rec t<'a> = Node(('a, list<t<'a>>))
  let get = t =>
    switch t {
    | Node(a) => a
    }
}
let rec levels = (ts: list<Tree.t<'a>>) =>
  switch ts {
  | list{} => list{}
  | _ => {
      let (xs, chls) =
        ts
        ->List.map(Tree.get)
        ->List.unzip
      list{xs, ...chls->List.flat->levels}
    }
  }

let tree = Tree.Node(
  1,
  list{
    Tree.Node(2, list{Tree.Node(4, list{})}),
    Tree.Node(3, list{Tree.Node(5, list{}), Tree.Node(6, list{})}),
  },
)

let join = (a, b) => a->List.toArray->Array.join(b)

list{tree}
->levels
->List.map(x => x->List.map(x => Int.toString(x))->join(" "))
->join("\n")
->Js.log
