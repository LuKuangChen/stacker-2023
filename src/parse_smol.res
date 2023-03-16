open Smol
open Utilities
open Belt.List
open Belt

exception ExpectingSymbol
let as_id = (e : annotated<S_expr.sexpr>) => {
  switch e.it {
  | S_expr.Atom(Sym(x)) => { it: x, ann: e.ann }
  | _ => raise(ExpectingSymbol)
  }
}

exception ExpectingList
let as_list = (e : annotated<S_expr.sexpr>) => {
  switch e.it {
  | S_expr.List(_b, ls) => ls
  | _ => raise(ExpectingList)
  }
}

exception ExpectingOneOrMoreGivenOther
let as_one_or_more = es => {
  switch es {
  | list{e1, ...es} => (e1, es)
  | _ => raise(ExpectingOneOrMoreGivenOther)
  }
}


let as_one_or_more_tail = es => {
  switch es {
  | list{e1, ...rest} =>
    switch reverse(rest) {
    | list{} => (list{}, e1)
    | list{x, ...xs} => (list{e1, ...reverse(xs)}, x)
    }
  | _ => raise(ExpectingOneOrMoreGivenOther)
  }
}

exception ExpectingTwoGivenOther
let as_two = es => {
  switch es {
  | list{e1, e2} => (e1, e2)
  | _ => raise(ExpectingTwoGivenOther)
  }
}
exception ExpectingTwoOrMoreGivenOther
let as_two_or_more = es => {
  switch es {
  | list{e1, e2, ...rest} =>
    switch reverse(rest) {
    | list{} => (e1, list{}, e2)
    | list{x, ...xs} => (e1, list{e2, ...reverse(xs)}, x)
    }
  | _ => raise(ExpectingTwoOrMoreGivenOther)
  }
}

exception ExpectingExpression
let as_expr = e => {
  switch e {
  | Exp(e) => e
  | _ => raise(ExpectingExpression)
  }
}

let expr_of_atom = a => {
    switch a {
    | S_expr.Str(s) => (Con(Str(s)) : expression)
    | S_expr.Sym("#t") => Con(Lgc(true))
    | S_expr.Sym("#f") => Con(Lgc(false))
    | S_expr.Sym(x) =>
        let e: expression = {
            let tryNum = x -> Float.fromString -> Option.map((n) => (Con(Num(n)) : expression))
            tryNum -> Option.getWithDefault(Ref(x))
        }
        e
    }
}

let rec term_of_sexpr = (e : annotated<S_expr.sexpr>) => {
  let ann = e.ann
  switch e.it {
  | S_expr.List(_b, list{{ it: Atom(Sym("defvar")), ann: _ }, ...rest}) => {
      let (x, e) = as_two(rest)
      let x = as_id(x)
      let e = as_expr(term_of_sexpr(e))
      Def({ ann, it: Var(x, e) })
    }

  | List(_b, list{{ it: Atom(Sym("deffun")), ann: _ }, ...rest}) => {
      let (head, terms, result) = as_two_or_more(rest)
      let (fun, args) = as_one_or_more(as_list(head))
      let fun = as_id(fun)
      let args = map(args, as_id)
      let terms = Belt.List.map(terms, term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr
      Def({ ann, it: Fun(fun, args, (terms, result))})
    }

  | List(_b, list{{ it: Atom(Sym("lambda")), ann: _ }, ...rest}) => {
      let (args, terms, result) = as_two_or_more(rest)
      let args = args->as_list->map(as_id)
      let terms = terms->map(term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr
      Exp({ ann, it: Lam(args, (terms, result))})
    }

  | List(_b, list{{ it: Atom(Sym("begin")), ann: _ }, ...rest}) => {
      let (terms, result) = as_one_or_more_tail(rest)
      let terms = terms->map(term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr
      Exp({ ann, it: Bgn(terms, result)})
    }

  | List(_b, list{{ it: Atom(Sym("set!")), ann: _ }, ...rest}) => {
      let (x, e) = as_two(rest)
      let x = as_id(x)
      let e = as_expr(term_of_sexpr(e))
      Exp({ann, it: Set(x, e)})
    }

  | List(_b, list{{ it: Atom(Sym("cond")), ann: _ }, ...branches}) => {
      let branches = branches->map(as_list)->map(as_two_or_more)
      let rec loop = (parsed, branches) => {
        switch branches {
        | list{} => Exp({ann, it: Cnd(reverse(parsed), None)})
        | list{({ it: Atom(Sym("else")), ann: _ } : annotated<S_expr.sexpr>, terms, result)} => {
            let terms = terms->map(term_of_sexpr)
            let result = result |> term_of_sexpr |> as_expr
            Exp({ann, it: Cnd(reverse(parsed), Some((terms, result)))})
          }

        | list{(case, terms, result), ...branches} => {
            let case = case->term_of_sexpr->as_expr
            let terms = terms->map(term_of_sexpr)
            let result = result |> term_of_sexpr |> as_expr
            loop(list{(case, (terms, result)), ...parsed}, branches)
          }
        }
      }
      loop(list{}, branches)
    }

  | List(_b, list{{ it: Atom(Sym("let")), ann: _ }, ...rest}) => {
    let (xes, ts, result) = as_two_or_more(rest)
    let xes = xes -> as_list -> map(as_list) -> map(as_two)
    let xes = xes -> map(((x, e)) => {
      let x = as_id(x)
      let e = term_of_sexpr(e) -> as_expr
      (x, e)
    })
    let ts = ts -> map(term_of_sexpr)
    let result = term_of_sexpr(result) -> as_expr
    Exp({ ann, it: Let(xes, (ts, result))})
  }

  | Atom(atom) => Exp({ann, it: expr_of_atom(atom)})
  | List(_b, es) => {
      let (e, es) = as_one_or_more(es)
      let e = e -> term_of_sexpr -> as_expr
      let es = es -> map(term_of_sexpr) -> map(as_expr)
      Exp({ann, it: App(e, es)})
  }
  }
}
and terms_of_sexprs = es => {
  Js.log(es)
  es -> map(term_of_sexpr)
}
