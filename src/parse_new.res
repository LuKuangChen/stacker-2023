// Parse the new syntax


module P1 = {
  type source = string
  type target = array<string>
  let parse: source => target = src => {
    Js.String.split("\n", src)
  }
}

let rec takeWhile = (pred, ls) => {
  switch ls {
  | list{} => (list{}, list{})
  | list{x, ...xs} =>
    if pred(x) {
      let (ls1, ls2) = takeWhile(pred, ls)
      (list{x, ...ls1}, ls2)
    } else {
      (list{}, ls)
    }
  }
}

let rec stringOfList = ls => {
  switch ls {
  | list{} => ""
  | list{x, ...xs} => x ++ stringOfList(xs)
  }
}

let re_defvar = %re("/var +([a-zA-Z][a-zA-Z0-9]*) *=(.*)/")
exception SyntaxError(string)
module P2 = {
  type source = array<string>
  type symbol = string
  type rec expression = string
  and block = (list<term>, expression)
  and definition =
    | Var(symbol, expression)
    | Fun(symbol, list<symbol>, block)
  and term =
    | Def(definition)
    | Exp(expression)
  type program = list<term>
  let parse: source => program = src => {
    let src = src |> Belt.List.fromArray
    let rec loop = ls => {
      switch ls {
      | list{} => list{}
      | list{l, ...ls} =>
        if Js.String2.startsWith(l, "var ") {
          let (ls1, ls2) = takeWhile(x => Js.String2.startsWith(x, " "), ls);
          let term = l ++ (ls1 |> stringOfList);
          switch Js.String2.match_(term, re_defvar) {
          | None => raise(SyntaxError(term))
          | Some(binds) =>
            let x = Belt.Option.getExn(binds[1]);
            let e = Belt.Option.getExn(binds[2]);
            list{Def(Var(x, e)), ...loop(ls2)}
          }
        } else if Js.String2.startsWith(l, "fun ") {
          let (ls1, ls2) = takeWhile(x => Js.String2.startsWith(x, " "), ls);
          let term = l ++ (ls1 |> stringOfList);
          switch Js.String2.match_(term, re_defvar) {
          | None => raise(SyntaxError(term))
          | Some(binds) =>
            let x = Belt.Option.getExn(binds[1]);
            let e = Belt.Option.getExn(binds[2]);
            list{Def(Var(x, e)), ...loop(ls2)}
          }
        } else {
          list{Exp(l), ...loop(ls)}
        }
      }
    }
    loop(src)
  }
}
