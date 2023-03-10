open Smol
open Belt.List
open Belt

exception ExpectingSymbol
let as_id = e => {
  switch e {
  | S_expr.Atom(Sym(x)) => x
  | _ => raise(ExpectingSymbol)
  }
}

exception ExpectingList
let as_list = e => {
  switch e {
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

let term_of_atom = a => {
    switch a {
    | S_expr.Str(s) => Exp(Con(Str(s)))
    | S_expr.Sym("#t") => Exp(Con(Lgc(true)))
    | S_expr.Sym("#f") => Exp(Con(Lgc(false)))
    | S_expr.Sym(x) =>
        let e = {
            let tryNum = x -> Float.fromString -> Option.map((n) => (Con(Num(n)) : expression))
            tryNum -> Option.getWithDefault(Ref(x))
        }
        Exp(e)
    }
}

let rec term_of_sexpr = e => {
  switch e {
  | S_expr.List(_b, list{Atom(Sym("defvar")), ...rest}) => {
      let (x, e) = as_two(rest)
      let x = as_id(x)
      let e = as_expr(term_of_sexpr(e))
      Def(Var(x, e))
    }

  | List(_b, list{Atom(Sym("deffun")), ...rest}) => {
      let (head, terms, result) = as_two_or_more(rest)
      let (fun, args) = as_one_or_more(as_list(head))
      let fun = as_id(fun)
      let args = map(args, as_id)
      let terms = Belt.List.map(terms, term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr
      Def(Fun(fun, args, (terms, result)))
    }

  | List(_b, list{Atom(Sym("lambda")), ...rest}) => {
      let (args, terms, result) = as_two_or_more(rest)
      let args = args->as_list->map(as_id)
      let terms = terms->map(term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr
      Exp(Lam(args, (terms, result)))
    }

  | List(_b, list{Atom(Sym("begin")), ...rest}) => {
      let (terms, result) = as_one_or_more_tail(rest)
      let terms = terms->map(term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr
      Exp(Bgn(terms, result))
    }

  | List(_b, list{Atom(Sym("set!")), ...rest}) => {
      let (x, e) = as_two(rest)
      let x = as_id(x)
      let e = as_expr(term_of_sexpr(e))
      Exp(Set(x, e))
    }

  | List(_b, list{Atom(Sym("cond")), ...branches}) => {
      let branches = branches->map(as_list)->map(as_two_or_more)
      let rec loop = (parsed, branches) => {
        switch branches {
        | list{} => Exp(Cnd(reverse(parsed), None))
        | list{(S_expr.Atom(Sym("else")), terms, result)} => {
            let terms = terms->map(term_of_sexpr)
            let result = result |> term_of_sexpr |> as_expr
            Exp(Cnd(reverse(parsed), Some((terms, result))))
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

  | List(_b, list{Atom(Sym("let")), ...rest}) => {
    let (xes, ts, result) = as_two_or_more(rest)
    let xes = xes -> as_list -> map(as_list) -> map(as_two)
    let xes = xes -> map(((x, e)) => {
      let x = as_id(x)
      let e = term_of_sexpr(e) -> as_expr
      (x, e)
    })
    let ts = ts -> map(term_of_sexpr)
    let result = term_of_sexpr(result) -> as_expr
    Exp(Let(xes, (ts, result)))
  }

  | Atom(atom) => term_of_atom(atom)
  | List(_b, es) => {
      let (e, es) = as_one_or_more(es)
      let e = e -> term_of_sexpr -> as_expr
      let es = es -> map(term_of_sexpr) -> map(as_expr)
      Exp(App(e, es))
  }
  }
}
and terms_of_sexprs = es => {
  Js.log(es)
  es -> map(term_of_sexpr)
}
