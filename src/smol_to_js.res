/*

This file convert smol programs to JavaScript

*/

open Smol
open Utilities
open Belt
open List

type js_ctx =
  | Expr
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

let string_of_prm = (o: primitive) => {
  switch o {
  | Add => "+"
  | Sub => "-"
  | Mul => "*"
  | Div => "/"
  | Lt => "<"
  | Eq => "="
  | Gt => ">"
  | Le => "<="
  | Ge => ">="
  | Ne => "!="
  | VecNew => "vec"
  | VecRef => "vec-ref"
  | VecSet => "vec-set!"
  | VecLen => "vec-len"
  | Eqv => "eqv?"
  | OError => "error"
  }
}

let string_of_list = ss => {
  "(" ++ String.concat(" ", ss) ++ ")"
}

let string_of_def_var = (x, e) => {
  `let ${x.it} = ${e}`
}

// let string_of_def_for = (_x, _e_from, _e_to, _body) => {
//   raise(TranslationNotSupportedYet("`for`-loop"))
// }

let string_of_def_fun = (f, xs, b) => {
  `function ${f}(${string_of_list(xs)}) {\n  ${indent(b, 2)}\n}`
}

let string_of_expr_set = (x, e) => {
  `${x} = ${e}`
}

let string_of_expr_lam = (xs, b) => {
  `function (${string_of_list(xs)}) {\n  ${indent(b, 2)}\n}`
}

let string_of_expr_app = (e, es) => {
  `${e}(${string_of_list(es)})`
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
    | Some(b) => ` else {\n${indent(b, 2)}\n}`
    }
  }
  let ebs = ebs->map(((e, b)) => `if (${e}) {\n${indent(b, 2)}\n}`)
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

let consider_context = (ctx: js_ctx, code: string) => {
  switch ctx {
  | Expr => code
  | Stat => `${code};`
  | Return => `return ${code};`
  }
}

let rec string_of_expr = (ctx: js_ctx, e: annotated<expression>): string => {
  switch e.it {
  | ECon(c) => string_of_constant(c) |> consider_context(ctx)
  | EPrm(p) => string_of_prm(p) |> consider_context(ctx)
  | Ref(x) => x.it |> consider_context(ctx)
  | Set(x, e) => string_of_expr_set(x->unann, string_of_expr(Expr, e)) |> consider_context(ctx)
  | Lam(xs, b) =>
    string_of_expr_lam(xs->map(unann), string_of_block(Return, b)) |> consider_context(ctx)
  | App(e, es) =>
    string_of_expr_app(string_of_expr(Expr, e), es->map(string_of_expr(Expr))) |> consider_context(
      ctx,
    )
  | Let(xes, b) =>
    string_of_expr_let(xes->map(string_of_xe), string_of_block(Return, b)) |> consider_context(ctx)
  | Cnd(ebs, ob) => string_of_expr_cnd(ebs->map(string_of_eb(ctx)), string_of_ob(ctx, ob))
  | If(e_cnd, e_thn, e_els) =>
    string_of_expr_if(
      string_of_expr(Expr, e_cnd),
      string_of_expr(Expr, e_thn),
      string_of_expr(Expr, e_els),
    ) |> consider_context(ctx)
  // | Whl(e, b) =>
  //   string_of_expr_whl(string_of_expr(Expr, e), string_of_block(Expr, b)) |> consider_context(ctx)
  | Bgn(es, e) =>
    string_of_expr_bgn(es->map(string_of_expr(Expr)), string_of_expr(Expr, e)) |> consider_context(
      ctx,
    )
  }
}
and string_of_def = (d: annotated<definition>): string => {
  switch d.it {
  | Var(x, e) => string_of_def_var(x, string_of_expr(Expr, e))
  | Fun(f, xs, b) => string_of_def_fun(f->unann, xs->map(unann), string_of_block(Return, b))
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
  (x.it, string_of_expr(Expr, e))
}
and string_of_eb = (ctx, eb) => {
  let (e, b) = eb
  (string_of_expr(Expr, e), string_of_block(ctx, b))
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
  | Exp(e) => string_of_expr(Expr, e)
  | Def(d) => string_of_def(d)
  }
}

let smol_to_js: string => string = smol_program => {
  smol_program->Parse_smol.parse_terms->map(string_of_term) |> String.concat("\n")
}
