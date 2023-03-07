open Index
open S_expr
open Belt.List

exception ExpectingSymbol
let as_id = e => {
  switch e {
  | Atom(Sym(x)) => x
  | _ => raise(ExpectingSymbol)
  }
}

exception ExpectingList
let as_list = e => {
  switch e {
  | List(ls) => ls
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

let rec term_of_sexpr = e => {
  switch e {
  | List(list{Atom(Sym("defvar")), ...rest}) => {
      let (x, e) = as_two(rest)
      let x = as_id(x)
      let e = as_expr(term_of_sexpr(e))
      Def(Var(x, e))
    }

  | List(list{Atom(Sym("deffun")), ...rest}) => {
      let (head, terms, result) = as_two_or_more(rest)
      let (fun, args) = as_one_or_more(as_list(head))
      let fun = as_id(fun)
      let args = map(args, as_id)
      let terms = Belt.List.map(terms, term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr
      Def(Fun(fun, args, (terms, result)))
    }

  | List(list{Atom(Sym("lambda")), ...rest}) => {
      let (args, terms, result) = as_two_or_more(rest)
      let args = args->as_list->map(as_id)
      let terms = terms->map(term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr
      Exp(Lam(args, (terms, result)))
    }

  | List(list{Atom(Sym("set!")), ...rest}) => {
      let (x, e) = as_two(rest)
      let x = as_id(x)
      let e = as_expr(term_of_sexpr(e))
      Exp(Set(x, e))
    }

  | List(list{Atom(Sym("cond")), ...branches}) => {
      let branches = branches->map(as_list)->map(as_two_or_more)
      let rec loop = (parsed, branches) => {
        switch branches {
        | list{} => Exp(Cnd(reverse(parsed), None))
        | list{(Atom(Sym("else")), terms, result)} => {
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

  | pattern2 => expression
  }
}
and terms_of_sexprs = es => {
  es |> map(term_of_sexpr)
}
