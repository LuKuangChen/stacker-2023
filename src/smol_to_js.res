/*

This file convert smol programs to JavaScript

*/

open Smol
open Utilities
open Belt
open List

type js_ctx =
  | Term
  | Expr(bool) // the bool indicates whether we are in an infix operation
  | Stat
  | Return

let string_of_constant = c => {
  switch c {
  | Uni => "null"
  | Num(n) => Float.toString(n)
  | Lgc(l) =>
    if l {
      "true"
    } else {
      "false"
    }
  | Str(s) => "\"" ++ String.escaped(s) ++ "\""
  }
}

let string_of_list = ss => {
  "(" ++ String.concat(", ", ss) ++ ")"
}

let string_of_identifier = x => {
  let re = %re("/-./g")
  let matchFn = (matchPart, _offset, _wholeString) => {
    Js.String2.toUpperCase(Js.String2.substringToEnd(matchPart, ~from=1))
  }
  Js.String2.unsafeReplaceBy0(x, re, matchFn)
}

let string_of_def_var = (x, e) => {
  `let ${string_of_identifier(x.it)} = ${e};`
}

// let string_of_def_for = (_x, _e_from, _e_to, _body) => {
//   raise(TranslationNotSupportedYet("`for`-loop"))
// }

let string_of_def_fun = (f, xs, b) => {
  `function ${f}${string_of_list(xs)} {\n  ${indent(b, 2)}\n}`
}

let string_of_expr_set = (x, e) => {
  `${x} = ${e}`
}

let string_of_expr_lam = (xs, b) => {
  `function ${string_of_list(xs)} {\n  ${indent(b, 2)}\n}`
}

let string_of_expr_app_prm = (p, es) => {
  switch (p, es) {
  | (Add, es) => `${String.concat(" + ", es)}`
  | (Sub, es) => `${String.concat(" - ", es)}`
  | (Mul, es) => `${String.concat(" * ", es)}`
  | (Div, es) => `${String.concat(" / ", es)}`
  | (Lt, list{e1, e2}) => `${e1} < ${e2}`
  | (Eq, list{e1, e2}) => `${e1} === ${e2}`
  | (Gt, list{e1, e2}) => `${e1} > ${e2}`
  | (Le, list{e1, e2}) => `${e1} <= ${e2}`
  | (Ge, list{e1, e2}) => `${e1} >= ${e2}`
  | (Ne, list{e1, e2}) => `${e1} != ${e2}`
  | (VecNew, es) => `[${String.concat(", ", es)}]`
  | (VecSet, list{e1, e2, e3}) => `${e1}[${e2}] = ${e3}`
  | (VecRef, list{e1, e2}) => `${e1}[${e2}]`
  | (VecLen, list{e}) => `${e}.length`
  | (Eqv, list{e1, e2}) => `${e1} === ${e2}`
  | (OError, list{e}) => `throw ${e}`
  | _ => "/* a primitive operation not supported yet */"
  }
}

let string_of_expr_app = (e, es) => {
  `${e}${string_of_list(es)}`
}

let string_of_expr_bgn = (es, e) => {
  `(${String.concat(", ", list{...es, e})})`
}

// let string_of_expr_whl = (_e, _b) => {
//   raise(TranslationNotSupportedYet("`while`-loop"))
// }

let string_of_expr_cnd = (ebs: list<(string, string)>, ob) => {
  let ob = {
    switch ob {
    | None => ""
    | Some(b) => ` else {\n  ${indent(b, 2)}\n}`
    }
  }
  let ebs = ebs->map(((e, b)) => `if (${e}) {\n  ${indent(b, 2)}\n}`)
  let ebs = String.concat(" else ", ebs)
  ebs ++ ob
}

let string_of_expr_if = (e_cnd: string, e_thn: string, e_els: string) => {
  `(${e_cnd} ? ${e_thn} : ${e_els})`
}

let string_of_expr_let = (xes, b) => {
  `((${xes->map(((x, _e)) => x) |> String.concat(", ")})=>{${b}})(${xes->map(((_x, e)) => e)
      |> String.concat(", ")})`
}

let maybe_wrap = (ctx, code) => {
  switch ctx {
  | Expr(true) => `(${code})`
  | _ => code
  }
}

let consider_context = (ctx: js_ctx, code: string) => {
  switch ctx {
  | Term
  | Expr(_) => code
  | Stat => `${code};`
  | Return => `return ${code};`
  }
}

let rec string_of_expr = (ctx: js_ctx, e: annotated<expression>): string => {
  switch e.it {
  | ECon(c) => string_of_constant(c) |> consider_context(ctx)
  | Ref(x) => string_of_identifier(x.it) |> consider_context(ctx)
  | Set(x, e) =>
    string_of_expr_set(
      x->unann->string_of_identifier,
      string_of_expr(Expr(false), e),
    ) |> consider_context(ctx)
  | Lam(xs, b) =>
    string_of_expr_lam(
      xs->map(unann)->map(string_of_identifier),
      string_of_block(Return, b),
    ) |> consider_context(ctx)
  | AppPrm(p, es) =>
    let o = string_of_expr_app_prm(p, es->map(string_of_expr(Expr(true)))) |> maybe_wrap(ctx)
    if p != OError {
      o |> consider_context(ctx)
    } else {
      o
    }
  | App(e, es) =>
    string_of_expr_app(
      string_of_expr(Expr(false), e),
      es->map(string_of_expr(Expr(false))),
    ) |> consider_context(ctx)
  | Let(xes, b) =>
    string_of_expr_let(xes->map(string_of_xe), string_of_block(Return, b)) |> consider_context(ctx)
  | Cnd(ebs, ob) => string_of_expr_cnd(ebs->map(string_of_eb(ctx)), string_of_ob(ctx, ob))
  | If(e_cnd, e_thn, e_els) =>
    string_of_expr_if(
      string_of_expr(Expr(false), e_cnd),
      string_of_expr(Expr(false), e_thn),
      string_of_expr(Expr(false), e_els),
    ) |> consider_context(ctx)
  // | Whl(e, b) =>
  //   string_of_expr_whl(string_of_expr(Expr(false), e), string_of_block(Expr(false), b)) |> consider_context(ctx)
  | Bgn(es, e) =>
    string_of_expr_bgn(
      es->map(string_of_expr(Expr(false))),
      string_of_expr(Expr(false), e),
    ) |> consider_context(ctx)
  }
}
and string_of_def = (d: annotated<definition>): string => {
  switch d.it {
  | Var(x, e) => string_of_def_var(x, string_of_expr(Expr(false), e))
  | Fun(f, xs, b) =>
    string_of_def_fun(
      f->unann->string_of_identifier,
      xs->map(unann)->map(string_of_identifier),
      string_of_block(Return, b),
    )
  // | For(x, e_from, e_to, b) =>
  //   string_of_def_for(
  //     x,
  //     string_of_expr(Expr, e_from),
  //     string_of_expr(Expr, e_to),
  //     string_of_block(Expr, b),
  //   )
  }
}
and string_of_xe = xe => {
  let (x, e) = xe
  (string_of_identifier(x.it), string_of_expr(Expr(false), e))
}
and string_of_eb = (ctx, eb) => {
  let (e, b) = eb
  (string_of_expr(Expr(false), e), string_of_block(ctx, b))
}
and string_of_ob = (ctx, ob) => {
  ob->Option.map(string_of_block(ctx))
}
and string_of_block = (ctx, b) => {
  let (ts, e) = b
  String.concat("\n", list{...ts->map(string_of_term), string_of_expr(ctx, e)})
}
and string_of_term = t => {
  switch t {
  | Exp(e) => string_of_expr(Stat, e)
  | Def(d) => string_of_def(d)
  }
}

let string_of_top_level = ts => {
  let string_of_term = t => {
    switch t {
    | Exp(e) => `console.log(${string_of_expr(Expr(false), e)})`
    | Def(d) => string_of_def(d)
    }
  }
  ts->map(string_of_term) |> String.concat("\n")
}

let as_many_then_one = es => {
  switch es {
  | list{e1, ...rest} =>
    switch reverse(rest) {
    | list{} => (list{}, e1)
    | list{x, ...xs} => (list{e1, ...reverse(xs)}, x)
    }
  | _ => raise(Impossible)
  }
}

let smol_to_js: (js_ctx, string) => string = (ctx, smol_program) => {
  let ts = smol_program->Parse_smol.parse_terms
  switch (ctx, ts) {
  | (Term, list{t}) => string_of_term(t)
  | (Term, _) => "...expecting exactly one term..."
  | (Expr(_), list{t}) => string_of_expr(ctx, t |> Parse_smol.as_expr(""))
  | (Expr(_), _) => "...expecting exactly one expression..."
  | (Stat, ts) => string_of_top_level(ts)
  | (Return, ts) => {
      let (ts, tz) = as_many_then_one(ts)
      let e = tz |> Parse_smol.as_expr("")
      string_of_block(Return, (ts, e))
    }
  }
}
