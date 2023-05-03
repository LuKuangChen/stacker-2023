open Smol
open Utilities
open Belt.List
open Belt

type kind_expectation =
  | Symbol
  | List

type arity_expectation =
  | ExactlyOne
  | ExactlyTwo
  | ExactlyThree
  | OneThenMany
  | ManyThenOne
  | OneThenManyThenOne

type term_kind =
  | Definition
  | Expression

type parse_error =
  | SExprKindError(kind_expectation, string, annotated<S_expr.sexpr>)
  | SExprArityError(arity_expectation, string, list<annotated<S_expr.sexpr>>)
  | TermKindError(term_kind, string, term)
exception ParseError(parse_error)

let stringOfExprs = es => {
  switch es {
  | list{} => "no term"
  | list{e} => `one term: ${S_expr.stringOfSexpr(e)}`
  | es =>
    `${List.length(es)->Int.toString} terms: ${String.concat(", ", es->map(S_expr.stringOfSexpr))}`
  }
}

let stringOfParseError: parse_error => string = err => {
  switch err {
  | SExprKindError(_kind, context, sexpr) =>
    `expecting a ${context}, given ${S_expr.stringOfSexpr(sexpr)}`
  | SExprArityError(_arity_expectation, context, es) =>
    `expecting ${context}, given ${stringOfExprs(es)}`
  | TermKindError(_term_kind, context, term) =>
    `expecting ${context}, given ${Stringify_smol.string_of_term(term)}`
  }
}

let as_id = (context, e: annotated<S_expr.sexpr>) => {
  switch e.it {
  | S_expr.Atom(Sym(x)) => {it: x, ann: e.ann}
  | _ => raise(ParseError(SExprKindError(Symbol, context, e)))
  }
}

let as_list = (context, e: annotated<S_expr.sexpr>) => {
  switch e.it {
  | S_expr.List(_b, ls) => ls
  | _ => raise(ParseError(SExprKindError(List, context, e)))
  }
}

let as_one_then_many = (context, es) => {
  switch es {
  | list{e1, ...es} => (e1, es)
  | _ => raise(ParseError(SExprArityError(OneThenMany, context, es)))
  }
}

let as_many_then_one = (context, es) => {
  switch es {
  | list{e1, ...rest} =>
    switch reverse(rest) {
    | list{} => (list{}, e1)
    | list{x, ...xs} => (list{e1, ...reverse(xs)}, x)
    }
  | _ => raise(ParseError(SExprArityError(ManyThenOne, context, es)))
  }
}

let as_two = (context, es) => {
  switch es {
  | list{e1, e2} => (e1, e2)
  | _ => raise(ParseError(SExprArityError(ExactlyTwo, context, es)))
  }
}
let as_three = (context, es) => {
  switch es {
  | list{e1, e2, e3} => (e1, e2, e3)
  | _ => raise(ParseError(SExprArityError(ExactlyThree, context, es)))
  }
}
let as_one_then_many_then_one = (context, es) => {
  switch es {
  | list{e1, e2, ...rest} =>
    switch reverse(rest) {
    | list{} => (e1, list{}, e2)
    | list{x, ...xs} => (e1, list{e2, ...reverse(xs)}, x)
    }
  | _ => raise(ParseError(SExprArityError(OneThenManyThenOne, context, es)))
  }
}

exception ExpectingExpression
let as_expr = (context, e) => {
  switch e {
  | Exp(e) => e
  | _ => raise(ParseError(TermKindError(Expression, context, e)))
  }
}

let expr_of_atom = (ann, atom) => {
  switch atom {
  | S_expr.Str(s) => (ECon(Str(s)): expression)
  | S_expr.Sym("#t") => ECon(Lgc(true))
  | S_expr.Sym("#f") => ECon(Lgc(false))
  | S_expr.Sym(x) =>
    let e: expression = {
      let tryNum = x->Float.fromString->Option.map((n): expression => ECon(Num(n)))
      tryNum->Option.getWithDefault(Ref({ann, it: x}))
    }
    e
  }
}

let rec term_of_sexpr = (e: annotated<S_expr.sexpr>) => {
  let ann = e.ann
  switch e.it {
  | S_expr.List(_b, list{{it: Atom(Sym("defvar")), ann: _}, ...rest}) => {
      let (x, e) = as_two("a variable and an expression", rest)
      let x = as_id("a variable name", x)
      let e = as_expr("an expression", term_of_sexpr(e))
      Def({ann, it: Var(x, e)})
    }

  | List(_b, list{{it: Atom(Sym("deffun")), ann: _}, ...rest}) => {
      let (head, terms, result) = as_one_then_many_then_one("", rest)
      let (fun, args) = as_one_then_many(
        "function name followed by parameters",
        as_list("function name and parameters", head),
      )
      let fun = as_id("a function name", fun)
      let args = map(args, as_id("a parameter"))
      let terms = Belt.List.map(terms, term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr("an expression to be returned")
      Def({ann, it: Fun(fun, args, (terms, result))})
    }

  | List(_b, list{{it: Atom(Sym("lambda")), ann: _}, ...rest}) => {
      let (args, terms, result) = as_one_then_many_then_one(
        "the function signature followed by the function body",
        rest,
      )
      let args = as_list("function parameters", args)->map(as_id("a parameter"))
      let terms = terms->map(term_of_sexpr)
      let result = result |> term_of_sexpr |> as_expr("an expression to be returned")
      Exp({ann, it: Lam(args, (terms, result))})
    }

  | List(_b, list{{it: Atom(Sym("begin")), ann: _}, ...rest}) => {
      let (terms, result) = as_many_then_one("one or more expressions", rest)
      let terms = terms->map(term_of_sexpr)->map(as_expr("an expression"))
      let result = result->term_of_sexpr |> as_expr("an expression")
      Exp({ann, it: Bgn(terms, result)})
    }

  // | List(_b, list{{it: Atom(Sym("while")), ann: _}, ...rest}) => {
  //     let (cond, terms, result) = as_one_then_many_then_one(rest->map(term_of_sexpr))
  //     let cond = cond->as_expr
  //     let result = result |> as_expr
  //     Exp({ann, it: Whl(cond, (terms, result))})
  //   }

  // | List(_b, list{{it: Atom(Sym("for")), ann: _}, ...rest}) => {
  //     let (x, rest) = as_one_or_more(rest)
  //     let (e_from, rest) = as_one_or_more(rest)
  //     let (e_to, rest) = as_one_or_more(rest)
  //     let (terms, result) = as_one_or_more_tail(rest)
  //     let x = x->as_id
  //     let e_from = e_from->term_of_sexpr->as_expr
  //     let e_to = e_to->term_of_sexpr->as_expr
  //     let terms = terms->map(term_of_sexpr)
  //     let result = result->term_of_sexpr->as_expr
  //     Def({ann, it: For(x, e_from, e_to, (terms, result))})
  //   }

  | List(_b, list{{it: Atom(Sym("set!")), ann: _}, ...rest}) => {
      let (x, e) = as_two("a variable and an expression", rest)
      let x = as_id("a variable to be set", x)
      let e = as_expr("an expression", term_of_sexpr(e))
      Exp({ann, it: Set(x, e)})
    }

  | List(_b, list{{it: Atom(Sym("if")), ann: _}, ...rest}) => {
      let (e_cnd, e_thn, e_els) = as_three(
        "three expressions (i.e., a condition, the \"then\" branch, and the \"else\" branch)",
        rest,
      )
      let e_cnd = as_expr("a (conditional) expression", term_of_sexpr(e_cnd))
      let e_thn = as_expr("an expression", term_of_sexpr(e_thn))
      let e_els = as_expr("an expression", term_of_sexpr(e_els))
      Exp({
        ann,
        it: If(e_cnd, e_thn, e_els),
      })
    }

  | List(_b, list{{it: Atom(Sym("cond")), ann: _}, ...branches}) => {
      let branches =
        branches
        ->map(as_list("a `cond` branch"))
        ->map(as_one_then_many_then_one("the condition followed by the branch"))
      let rec loop = (parsed, branches) => {
        switch branches {
        | list{} => Exp({ann, it: Cnd(reverse(parsed), None)})
        | list{({it: Atom(Sym("else")), ann: _}: annotated<S_expr.sexpr>, terms, result)} => {
            let terms = terms->map(term_of_sexpr)
            let result = result |> term_of_sexpr |> as_expr("an expression")
            Exp({ann, it: Cnd(reverse(parsed), Some((terms, result)))})
          }

        | list{(case, terms, result), ...branches} => {
            let case = case->term_of_sexpr |> as_expr("a (conditional) expression")
            let terms = terms->map(term_of_sexpr)
            let result = result |> term_of_sexpr |> as_expr("an expression")
            loop(list{(case, (terms, result)), ...parsed}, branches)
          }
        }
      }
      loop(list{}, branches)
    }

  | List(_b, list{{it: Atom(Sym("let")), ann: _}, ...rest}) => {
      let (xes, ts, result) = as_one_then_many_then_one("the bindings followed by the body", rest)
      let xes =
        as_list("variable-expression pairs", xes)
        ->map(as_list("a variable and an expression"))
        ->map(as_two("a variable and an expression"))
      let xes = xes->map(((x, e)) => {
        let x = as_id("a variable to be bound", x)
        let e = term_of_sexpr(e) |> as_expr("an expression")
        (x, e)
      })
      let ts = ts->map(term_of_sexpr)
      let result = term_of_sexpr(result) |> as_expr("an expression to be return")
      Exp({ann, it: Let(xes, (ts, result))})
    }

  | Atom(atom) => Exp({ann, it: expr_of_atom(ann, atom)})
  | List(_b, list{{it: Atom(Sym("+")), ann: _}, ...es}) => app_prm(ann, Add, es)
  | List(_b, list{{it: Atom(Sym("-")), ann: _}, ...es}) => app_prm(ann, Sub, es)
  | List(_b, list{{it: Atom(Sym("*")), ann: _}, ...es}) => app_prm(ann, Mul, es)
  | List(_b, list{{it: Atom(Sym("/")), ann: _}, ...es}) => app_prm(ann, Div, es)
  | List(_b, list{{it: Atom(Sym("<")), ann: _}, ...es}) => app_prm(ann, Lt, es)
  | List(_b, list{{it: Atom(Sym("=")), ann: _}, ...es}) => app_prm(ann, Eq, es)
  | List(_b, list{{it: Atom(Sym(">")), ann: _}, ...es}) => app_prm(ann, Gt, es)
  | List(_b, list{{it: Atom(Sym("<=")), ann: _}, ...es}) => app_prm(ann, Le, es)
  | List(_b, list{{it: Atom(Sym(">=")), ann: _}, ...es}) => app_prm(ann, Ge, es)
  | List(_b, list{{it: Atom(Sym("!=")), ann: _}, ...es}) => app_prm(ann, Ne, es)
  | List(_b, list{{it: Atom(Sym("vec")), ann: _}, ...es}) => app_prm(ann, VecNew, es)
  | List(_b, list{{it: Atom(Sym("mvec")), ann: _}, ...es}) => app_prm(ann, VecNew, es)
  | List(_b, list{{it: Atom(Sym("vec-ref")), ann: _}, ...es}) => app_prm(ann, VecRef, es)
  | List(_b, list{{it: Atom(Sym("vref")), ann: _}, ...es}) => app_prm(ann, VecRef, es)
  | List(_b, list{{it: Atom(Sym("vec-set!")), ann: _}, ...es}) => app_prm(ann, VecSet, es)
  | List(_b, list{{it: Atom(Sym("vset!")), ann: _}, ...es}) => app_prm(ann, VecSet, es)
  | List(_b, list{{it: Atom(Sym("vec-len")), ann: _}, ...es}) => app_prm(ann, VecLen, es)
  | List(_b, list{{it: Atom(Sym("vlen")), ann: _}, ...es}) => app_prm(ann, VecLen, es)
  | List(_b, list{{it: Atom(Sym("eq?")), ann: _}, ...es}) => app_prm(ann, Eqv, es)
  | List(_b, list{{it: Atom(Sym("eqv?")), ann: _}, ...es}) => app_prm(ann, Eqv, es)
  | List(_b, list{{it: Atom(Sym("error")), ann: _}, ...es}) => app_prm(ann, OError, es)
  | List(_b, es) => {
      let (e, es) = as_one_then_many(
        "a function call/application, which includes a function and then one ore more arguments",
        es,
      )
      let e = e->term_of_sexpr |> as_expr("a function")
      let es = es->map(term_of_sexpr)->map(as_expr("an argument"))
      Exp({ann, it: App(e, es)})
    }
  }
}
and app_prm = (ann, p, es) => {
  let es = es->map(term_of_sexpr)->map(as_expr("an argument"))
  Exp({ann, it: AppPrm(p, es)})
}
and terms_of_sexprs = es => {
  es->map(term_of_sexpr)
}

let parse_terms = src => {
  src->S_expr.stringToSource->S_expr.parse_many->terms_of_sexprs
}
