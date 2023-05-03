open Smol
open Utilities
open Belt
open List

let string_of_constant = c => {
  switch c {
  | Uni => "#<void>"
  | Num(n) => Float.toString(n)
  | Lgc(l) =>
    if l {
      "#t"
    } else {
      "#f"
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
  | Eqv => "eq?"
  | OError => "error"
  }
}

let string_of_function = (f: Smol.function) => {
  switch f {
  | Udf(id, _name, _ann, _xs, _body, _env) => {
      let id = id->Int.toString
      // let name = name.contents->Option.map(s => ":" ++ s)->Option.getWithDefault("")
      // I find the suffix makes the configuration very unreadable
      //   in certain cases, and only helps marginally even in
      //   the best case.
      // "@" ++ id ++ name
      "@" ++ id
    }

  | Prm(prm) => string_of_prm(prm)
  }
}

// let string_of_vector = (_v: array<value>) => {
//   "#<vector>"
// }

let string_of_value = (v: value) => {
  switch v {
  | Con(c) => string_of_constant(c)
  | VFun(f) => string_of_function(f)
  | Vec(id, _v) => "@" ++ Int.toString(id) // string_of_vector(v)
  }
}

let blank = s => {
  <code className="blank"> {React.string(s)} </code>
}

let string_of_list = ss => {
  "(" ++ String.concat(" ", ss) ++ ")"
}

let string_of_def_var = (x, e) => {
  string_of_list(list{"defvar", x.it, e})
}

let string_of_def_for = (x, e_from, e_to, body) => {
  `(for ${x.it} ${e_from} ${e_to}\n${indent(body, 2)})`
}

let string_of_def_fun = (f, xs, b) => {
  // string_of_list(list{"deffun", string_of_list(list{f, ...xs}), b})
  "(" ++ "deffun" ++ " " ++ string_of_list(list{f, ...xs}) ++ "\n  " ++ indent(b, 2) ++ ")"
}

let string_of_expr_set = (x, e) => {
  string_of_list(list{"set!", x, e})
}

let string_of_expr_lam = (xs, b) => {
  // if String.contains(b, '\n') {
  "(" ++ "lambda" ++ " " ++ string_of_list(xs) ++ "\n  " ++ indent(b, 2) ++ ")"
  // } else {
  // "(" ++ "lambda" ++ " " ++ string_of_list(xs) ++ " " ++ b ++ ")"
  // }
}

let string_of_expr_app = (e, es) => {
  string_of_list(list{e, ...es})
}

let string_of_expr_bgn = (es, e) => {
  let b = String.concat("\n", list{...es, e})
  "(begin\n  " ++ indent(b, 2) ++ ")"
}

let string_of_expr_whl = (e, b) => {
  "(while " ++ e ++ "\n  " ++ indent(b, 2) ++ ")"
}

let string_of_expr_cnd = (ebs: list<(string, string)>, ob) => {
  let ebs = {
    switch ob {
    | None => ebs
    | Some(b) => list{...ebs, ("else", b)}
    }
  }
  let ebs = ebs->map(((e, b)) => `[${e}\n ${indent(b, 1)}]`)
  let ebs = String.concat("\n", ebs)
  "(" ++ "cond\n  " ++ indent(ebs, 2) ++ ")"
}

let string_of_expr_if = (e_cnd: string, e_thn: string, e_els: string) => {
  `(if ${indent(e_cnd, 4)}\n    ${indent(e_thn, 4)}\n    ${indent(e_els, 4)})`
}

let string_of_expr_let = (xes, b) => {
  let xes = xes->map(((x, e)) => {
    let x = unann(x)
    `[${x} ${indent(e, 2 + String.length(x))}]`
  })
  let xes = String.concat("\n", xes)
  `(let ${indent(xes, 5)}\n${indent(b, 2)})`
}

let rec string_of_expr = (e: annotated<expression>): string => {
  switch e.it {
  | ECon(c) => string_of_constant(c)
  | Ref(x) => x.it
  | Set(x, e) => string_of_expr_set(x->unann, string_of_expr(e))
  | Lam(xs, b) => string_of_expr_lam(xs->map(unann), string_of_block(b))
  | AppPrm(p, es) => string_of_expr_app(string_of_prm(p), es->map(string_of_expr))
  | App(e, es) => string_of_expr_app(string_of_expr(e), es->map(string_of_expr))
  | Let(xes, b) => string_of_expr_let(xes->map(string_of_xe), string_of_block(b))
  | Cnd(ebs, ob) => string_of_expr_cnd(ebs->map(string_of_eb), string_of_ob(ob))
  | If(e_cnd, e_thn, e_els) =>
    string_of_expr_if(string_of_expr(e_cnd), string_of_expr(e_thn), string_of_expr(e_els))
  // | Whl(e, b) => string_of_expr_whl(string_of_expr(e), string_of_block(b))
  | Bgn(es, e) => string_of_expr_bgn(es->map(string_of_expr), string_of_expr(e))
  }
}
and string_of_def = (d: annotated<definition>): string => {
  switch d.it {
  | Var(x, e) => string_of_def_var(x, string_of_expr(e))
  | Fun(f, xs, b) => string_of_def_fun(f->unann, xs->map(unann), string_of_block(b))
  // | For(x, e_from, e_to, b) =>
  //   string_of_def_for(x, string_of_expr(e_from), string_of_expr(e_to), string_of_block(b))
  }
}
and string_of_xe = xe => {
  let (x, e) = xe
  (x, string_of_expr(e))
}
and string_of_eb = eb => {
  let (e, b) = eb
  (string_of_expr(e), string_of_block(b))
}
and string_of_ob = ob => {
  ob->Option.map(string_of_block)
}
and string_of_block = b => {
  let (ts, e) = b
  String.concat("\n", list{...ts->map(string_of_term), string_of_expr(e)})
}
and string_of_term = t => {
  switch t {
  | Exp(e) => string_of_expr(e)
  | Def(d) => string_of_def(d)
  }
}

let string_of_ctxFrame = frm => {
  switch frm {
  | Set1(x, ()) => xyz => string_of_expr_set(x->unann, xyz)
  | Let1(xvs, (x, ()), xes, b) =>
    xyz => {
      let xvs = xvs->List.map(((x, v)) => (x, string_of_value(v)))
      let xes = xes->List.map(((x, e)) => (x, string_of_expr(e)))
      let x_s = list{...xvs->reverse, (x, xyz), ...xes}
      string_of_expr_let(x_s, string_of_block(b))
    }
  | App1((), es) => xyz => string_of_expr_app(xyz, es->map(string_of_expr))
  | App2(v, vs, (), es) =>
    xyz => {
      let e = string_of_value(v)
      let es = list{...vs->map(string_of_value), xyz, ...es->map(string_of_expr)}
      string_of_expr_app(e, es)
    }
  | Cnd1((), b, ebs, ob) =>
    xyz => {
      let eb = (xyz, string_of_block(b))
      let ebs = list{eb, ...ebs->map(string_of_eb)}
      string_of_expr_cnd(ebs, string_of_ob(ob))
    }
  | If1((), e_thn, e_els) =>
    xyz => {
      string_of_expr_if(xyz, string_of_expr(e_thn), string_of_expr(e_els))
    }
  | Bgn1((), es, e) =>
    xyz => {
      let es = list{xyz, ...es->map(string_of_expr)}
      let e = string_of_expr(e)
      string_of_expr_bgn(es, e)
    }
  | BlkDef(x, (), b) =>
    xyz => {
      let d = string_of_def_var(x, xyz)
      d ++ "\n" ++ string_of_block(b)
    }
  | BlkExp((), b) =>
    xyz => {
      xyz ++ "\n" ++ string_of_block(b)
    }
  | PrgDef(vs, x, (), ts) =>
    xyz => {
      let vs = vs->map(string_of_value)->reverse
      let d = string_of_def_var(x, xyz)
      let ts = ts->map(string_of_term)
      String.concat("\n", list{...vs, d, ...ts})
    }
  | PrgExp(vs, (), ts) =>
    xyz => {
      let vs = vs->map(string_of_value)->reverse
      let ts = ts->map(string_of_term)
      String.concat("\n", list{...vs, xyz, ...ts})
    }
  }
}

let stringOfEntrance = entrance => {
  switch entrance {
  | App => "a function body"
  | Let => "a `let` body"
  }
}
