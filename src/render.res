/*

This file convert smol states to react elements.

*/

open Smol
open Belt
open List

let todo = React.string("TODO")

let string_of_constant = c => {
  switch c {
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
  }
}

let string_of_function = (f: Smol.function) => {
  switch f {
  | Udf(id, name, _xs, _body, _env) => {
      let id = id->Int.toString
      let name = name.contents->Option.map(s => ":" ++ s)->Option.getWithDefault("")
      "@" ++ id ++ name
    }

  | Prm(prm) => string_of_prm(prm)
  }
}

let string_of_value = v => {
  switch v {
  | Uni => "#<void>"
  | Con(c) => string_of_constant(c)
  | Fun(f) => string_of_function(f)
  }
}

let blank = s => {
  <code className="blank"> {React.string(s)} </code>
}

let string_of_list = ss => {
  "(" ++ String.concat(" ", ss) ++ ")"
}

let string_of_def_var = (x, e) => {
  string_of_list(list{"defvar", x, e})
}

let string_of_def_fun = (f, xs, b) => {
  // string_of_list(list{"deffun", string_of_list(list{f, ...xs}), b})
  let b = Js.String.replace("\n", "\n  ", b)
  "(" ++ "deffun" ++ " " ++ string_of_list(list{f, ...xs}) ++ "\n  " ++ b ++ ")"
}

let string_of_expr_set = (x, e) => {
  string_of_list(list{"set!", x, e})
}

let string_of_expr_lam = (xs, b) => {
  let b = Js.String.replace("\n", "\n  ", b)
  "(" ++ "lambda" ++ " " ++ string_of_list(xs) ++ "\n  " ++ b ++ ")"
}

let string_of_expr_app = (e, es) => {
  string_of_list(list{e, ...es})
}

let string_of_expr_cnd = (ebs, ob) => {
  let ebs = {
    switch ob {
    | None => ebs
    | Some(b) => list{...ebs, b}
    }
  }
  string_of_list(list{"cond", ...ebs})
}

let rec string_of_expr = (e: expression): string => {
  switch e {
  | Con(c) => string_of_constant(c)
  | Ref(x) => x
  | Set(x, e) => string_of_expr_set(x, string_of_expr(e))
  | Lam(xs, b) => string_of_expr_lam(xs, string_of_block(b))
  | App(e, es) => string_of_expr_app(string_of_expr(e), es->map(string_of_expr))
  | Cnd(ebs, ob) => string_of_expr_cnd(ebs->map(string_of_eb), string_of_ob(ob))
  }
}
and string_of_def = (d: definition): string => {
  switch d {
  | Var(x, e) => string_of_def_var(x, string_of_expr(e))
  | Fun(f, xs, b) => string_of_def_fun(f, xs, string_of_block(b))
  }
}
and string_of_eb = eb => {
  let (e, b) = eb
  string_of_expr(e) ++ "\n" ++ string_of_block(b)
}
and string_of_ob = ob => {
  Option.map(ob, string_of_block)
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

let label: string => React.element = s => {
  <span className="label"> {React.string(s)} </span>
}

let show_expr = e => {
  blank(string_of_expr(e))
}

let show_value = e => {
  blank(string_of_value(e))
}

let shower_of_contextFrame = frm => {
  switch frm {
  | Set1(x, ()) => xyz => string_of_expr_set(x, xyz)
  | App1((), es) => xyz => string_of_expr_app(xyz, es->map(string_of_expr))
  | App2(v, vs, (), es) =>
    xyz => {
      let e = string_of_value(v)
      let es = list{...vs->map(string_of_value), xyz, ...es->map(string_of_expr)}
      string_of_expr_app(e, es)
    }
  | Cnd1((), b, ebs, ob) =>
    xyz => {
      let eb = xyz ++ "\n" ++ string_of_block(b)
      let ebs = list{eb, ...ebs->map(string_of_eb)}
      string_of_expr_cnd(ebs, string_of_ob(ob))
    }
  | BgnDef(x, (), b) =>
    xyz => {
      let d = string_of_def_var(x, xyz)
      d ++ "\n" ++ string_of_block(b)
    }
  | BgnExp((), b) =>
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

let show_ctx = ctx => {
  let ctx = ctx->map(shower_of_contextFrame)
  let ctx = reduce(ctx, "☐", (sofar, f) => f(sofar))
  blank(ctx)
}

let show_envFrm = (frm: environmentFrame) => {
  if Array.length(frm.content) == 0 {
    React.string(" nothing")
  } else {
    React.array(
      Array.map(frm.content, xv => {
        let (x, v) = xv
        let ov = v.contents
        let v = switch ov {
        | None => "💣"
        | Some(v) => string_of_value(v)
        }
        <div className="bind">
          {blank(x)}
          {React.string("↦")}
          {blank(v)}
        </div>
      }),
    )
  }
}

let show_env = (env: environment) => {
  switch env {
  | list{} => raise(Impossible("An environment must have at least one frame."))
  | list{frm, ..._rest} => blank(frm.id)
  }
}

let show_one_env = (env: environment): React.element => {
  switch env {
  | list{} => raise(Impossible("An environment must have at least one frame."))
  | list{frm, ...rest} => {
      let {id, content} = frm
      <div className="env-frame box">
        <p>
          {label("@ ")}
          {blank(id)}
        </p>
        <p> {label("binds")} </p>
        {show_envFrm(frm)}
        <p>
          {label("rest @ ")}
          {show_env(rest)}
        </p>
      </div>
    }
  }
}

let show_all_envs = () => {
  React.array(all_envs.contents->reverse->map(show_one_env)->List.toArray)
}

exception Impossible
let show_one_hav = (val: value): React.element => {
  switch val {
  | Fun(Udf(id, name, xs, body, env)) => {
      let id = id->Int.toString
      let name = name.contents->Option.map(s => ":" ++ s)->Option.getWithDefault("")
      let id = id ++ name
      <div className="fun box">
        <p>
          {label("@ ")}
          {blank(id)}
        </p>
        <p>
          {label("environment @ ")}
          {show_env(env)}
        </p>
        <p>
          {label("Code ")}
          {show_expr(Lam(xs->List.fromArray, body))}
        </p>
      </div>
    }

  | _ => raise(Impossible)
  }
}

let show_all_havs = () => {
  React.array(all_havs.contents->reverse->map(show_one_hav)->List.toArray)
}

let string_of_error = err => {
  switch err {
  | UnboundIdentifier(symbol) => `The variable ${symbol} hasn't be declared.`
  | UsedBeforeInitialization(symbol) => `The variable ${symbol} hasn't be assigned a value.`
  | ExpectButGiven(string, _value) => `Expecting a ${string}.`
  | ArityMismatch(_arity, int) => `Expecting a function that accept ${Int.toString(int)} arguments`
  }
}

let show_stkFrm = (frm: stackFrame) => {
  let (ctx, env) = frm
  <div className="stack-frame box">
    {label("Waiting for a value")}
    <p>
      {label("in context ")}
      {show_ctx(ctx)}
    </p>
    <p>
      {label("in environment @ ")}
      {show_env(env)}
    </p>
  </div>
}

let show_stack = (frms: list<React.element>) => {
  <div> {React.array(frms->List.toArray)} </div>
}

let show_state = (stack, now, envs, heap) => {
  <div id="smol-state">
    <div id="stack-and-now" className="column">
      <p> {label("Stack Frames & Program Counter")} </p>
      <div> {stack} </div>
      <div className="now"> {now} </div>
    </div>
    <div className="column">
      <p> {label("Environments")} </p>
      {envs}
    </div>
    <div className="column">
      <p> {label("Heap-allocated Values")} </p>
      {heap}
    </div>
  </div>
}

let render: Smol.state => React.element = s => {
  switch s {
  | Terminated(Err(err)) => {
      let now =
        <div className="box errored">
          <p> {label("Errored")} </p>
          {blank(string_of_error(err))}
        </div>
      show_state(show_stack(list{}), now, show_all_envs(), show_all_havs())
    }

  | Terminated(Tm(vs)) => {
      let now =
        <div className="box terminated">
          <p> {label("Terminated")} </p>
          {blank(String.concat("\n", vs->reverse->map(string_of_value)))}
        </div>
      show_state(show_stack(list{}), now, show_all_envs(), show_all_havs())
    }

  | Continuing(App(f, vs, stt)) => {
      let {ctx, env, stk} = stt
      let stk = show_stack(stk->map(show_stkFrm))
      let now =
        <div className="box calling">
          <p>
            {label("Calling ")}
            {blank(string_of_list(list{string_of_value(f), ...vs->map(string_of_value)}))}
          </p>
          <p>
            {label("in context ")}
            {show_ctx(ctx)}
          </p>
          <p>
            {label("in environment @ ")}
            {show_env(env)}
          </p>
        </div>
      show_state(stk, now, show_all_envs(), show_all_havs())
    }

  | Continuing(Setting(x, v, stt)) => {
      let {ctx, env, stk} = stt
      let stk = show_stack(stk->map(show_stkFrm))
      let now =
        <div className="box replacing">
          <p>
            {label("Replacing the value of ")}
            {blank(x)}
            {label(" with ")}
            {show_value(v)}
          </p>
          <p>
            {label("in context ")}
            {show_ctx(ctx)}
          </p>
          <p>
            {label("in environment @ ")}
            {show_env(env)}
          </p>
        </div>
      show_state(stk, now, show_all_envs(), show_all_havs())
    }

  | Continuing(Returning(v, stk)) => {
      let stk = show_stack(stk->map(show_stkFrm))
      let now =
        <div className="box returning">
          <p>
            {label("Returning ")}
            {show_value(v)}
          </p>
        </div>
      show_state(stk, now, show_all_envs(), show_all_havs())
    }
  }
}
