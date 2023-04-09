/*

This file convert smol states to react elements.

*/

open Utilities
open Belt
open List
open Smol

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
  | Eqv => "eqv?"
  | Error => "error"
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

let string_of_expr_bgn = b => {
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
  | EPrm(p) => string_of_prm(p)
  | Ref(x) => x.it
  | Set(x, e) => string_of_expr_set(x->unann, string_of_expr(e))
  | Lam(xs, b) => string_of_expr_lam(xs->map(unann), string_of_block(b))
  | App(e, es) => string_of_expr_app(string_of_expr(e), es->map(string_of_expr))
  | Let(xes, b) => string_of_expr_let(xes->map(string_of_xe), string_of_block(b))
  | Cnd(ebs, ob) => string_of_expr_cnd(ebs->map(string_of_eb), string_of_ob(ob))
  | If(e_cnd, e_thn, e_els) =>
    string_of_expr_if(string_of_expr(e_cnd), string_of_expr(e_thn), string_of_expr(e_els))
  | Whl(e, b) => string_of_expr_whl(string_of_expr(e), string_of_block(b))
  // | Bgn(b) => string_of_expr_bgn(string_of_block(b))
  }
}
and string_of_def = (d: annotated<definition>): string => {
  switch d.it {
  | Var(x, e) => string_of_def_var(x, string_of_expr(e))
  | Fun(f, xs, b) => string_of_def_fun(f->unann, xs->map(unann), string_of_block(b))
  | For(x, e_from, e_to, b) =>
    string_of_def_for(x, string_of_expr(e_from), string_of_expr(e_to), string_of_block(b))
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

let label = React.string

let show_expr = e => {
  blank(string_of_expr(e))
}

let show_value = e => {
  blank(string_of_value(e))
}

let shower_of_contextFrame = frm => {
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
  let ctx = reduce(ctx, "❓", (sofar, f) => f(sofar))
  blank(ctx)
}

let show_envFrm = (frm: environmentFrame) => {
  if Array.length(frm.content) == 0 {
    React.string("(nothing)")
  } else {
    <span className="binds">
      {React.array(
        Array.mapWithIndex(frm.content, (key, xv) => {
          let key = Int.toString(key)
          let (x, v) = xv
          let ov = v.contents
          let v = switch ov {
          | None => "💣"
          | Some(v) => string_of_value(v)
          }
          <span key className="bind">
            {blank(x)}
            <span ariaHidden={true}> {React.string(" ↦ ")} </span>
            <span className="sr-only"> {React.string("to")} </span>
            {blank(v)}
          </span>
        }),
      )}
    </span>
  }
}

let show_env = (env: environment) => {
  switch env {
  | list{} => raise(Impossible("An environment must have at least one frame."))
  | list{frm, ..._rest} => blank(`@${frm.id}`)
  }
}

let show_one_env = (key: int, env: environment): React.element => {
  switch env {
  | list{} => raise(Impossible("An environment must have at least one frame."))
  | list{frm, ...rest} => {
      let {id, content: _} = frm
      <li key={Int.toString(key)} className="env-frame box">
        <span>
          {blank(`@${id}`)}
          <br />
          {label("binds ")}
          {show_envFrm(frm)}
          <br />
          {label("extending ")}
          {show_env(rest)}
        </span>
      </li>
    }
  }
}

let show_all_envs = () => {
  if all_envs.contents === list{} {
    <p> {label("(No environments)")} </p>
  } else {
    <ol className="box-list" ariaLabel="a list of all environments">
      {React.array(all_envs.contents->reverse->mapWithIndex(show_one_env)->List.toArray)}
    </ol>
  }
}

exception Impossible
let show_one_hav = (key: int, val: value): React.element => {
  let key = Int.toString(key)
  switch val {
  | VFun(Udf(id, _name, ann, xs, body, env)) => {
      let id = id->Int.toString
      // let name = name.contents->Option.map(s => ":" ++ s)->Option.getWithDefault("")
      // let id = id ++ name
      <li key className="fun box">
        {blank(`@${id}`)}
        {label(", a function")}
        <br />
        <details>
          <summary>
            {React.string(`at line ${(ann.begin.ln + 1)->Int.toString}`)}
            <small> {React.string(`:${(ann.begin.ch + 1)->Int.toString}`)} </small>
            {React.string(` to ${(ann.end.ln + 1)->Int.toString}`)}
            <small> {React.string(`:${(ann.end.ch + 1)->Int.toString}`)} </small>
          </summary>
          <p>
            {blank(string_of_expr_lam(xs->List.fromArray->map(unann), string_of_block(body)))}
          </p>
        </details>
        {label("with environment ")}
        {show_env(env)}
      </li>
    }

  | Vec(id, vs) => {
      let id = id->Int.toString
      <li key className="vec box">
        {blank(`@${id}`)}
        {label(", a vector")}
        <br />
        {label("with contents")}
        {React.array(
          vs
          ->Array.map(string_of_value)
          ->Array.map(blank)
          ->Array.mapWithIndex((i, e) =>
            <span key={Int.toString(i)}>
              {React.string(" ")}
              {e}
            </span>
          ),
        )}
      </li>
    }

  | _ => raise(Impossible)
  }
}

let show_all_havs = () => {
  if all_havs.contents === list{} {
    <p> {label("(No heap-allocated values)")} </p>
  } else {
    <ol className="box-list" ariaLabel="a list of all heap-allocated values">
      {React.array(all_havs.contents->reverse->mapWithIndex(show_one_hav)->List.toArray)}
    </ol>
  }
}

let string_of_error = err => {
  switch err {
  | UnboundIdentifier(symbol) => `The variable ${symbol} hasn't been defined.`
  | UsedBeforeInitialization(symbol) => `The variable ${symbol} hasn't been assigned a value.`
  | ExpectButGiven(string, _value) => `Expecting a ${string}.`
  | ArityMismatch(_arity, int) => `Expecting a function that accept ${Int.toString(int)} arguments.`
  | OutOfBound(length, index) =>
    `Expecting an index less than the length of the vector (${Int.toString(
        length,
      )}), found ${Int.toString(index)}.`
  | UserRaised(message) => message
  }
}

let show_stkFrm = (key: int, frm: stackFrame) => {
  let key = Int.toString(key)
  let (ctx, env) = frm
  <li key className="stack-frame box">
    {label("Waiting for a value")}
    <br />
    {label("in context ")}
    {show_ctx(ctx)}
    <br />
    {label("in environment ")}
    {show_env(env)}
  </li>
}

let show_stack = (frms: list<React.element>) => {
  if frms == list{} {
    <p> {React.string("(No stack frames)")} </p>
  } else {
    <ol className="box-list" ariaLabel="stack frames, with the oldest at the top">
      {React.array(frms->reverse->List.toArray)}
    </ol>
  }
}

let show_state = (stack, now, envs, heap) => {
  <article id="stacker-configuration" ariaLabel="the current stacker configuration">
    <section id="stack-and-now">
      <h1> {label("Stack Frames & The Program Counter")} </h1>
      {stack}
      <hr />
      {now}
    </section>
    <section id="environments">
      <h1> {label("Environments")} </h1>
      {envs}
    </section>
    <section id="heap">
      <h1> {label("Heap-allocated Values")} </h1>
      {heap}
    </section>
  </article>
}

let render: Smol.state => React.element = s => {
  switch s {
  | Terminated(Err(err)) => {
      let now =
        <div className="now box errored">
          <p> {label("Errored")} </p>
          {blank(string_of_error(err))}
        </div>
      show_state(show_stack(list{}), now, show_all_envs(), show_all_havs())
    }

  | Terminated(Tm(vs)) => {
      let now =
        <div className="now box terminated">
          <p> {label("Terminated")} </p>
          {blank(String.concat("\n", vs->reverse->map(string_of_value)))}
        </div>
      show_state(show_stack(list{}), now, show_all_envs(), show_all_havs())
    }

  | Continuing(Applying(f, vs, stt)) => {
      let {ctx, env, stk} = stt
      let stk = show_stack(stk->mapWithIndex(show_stkFrm))
      let now =
        <p className="now box calling">
          {label("Calling ")}
          {blank(string_of_list(list{string_of_value(f), ...vs->map(string_of_value)}))}
          <br />
          {label("in context ")}
          {show_ctx(ctx)}
          <br />
          {label("in environment ")}
          {show_env(env)}
        </p>
      show_state(stk, now, show_all_envs(), show_all_havs())
    }

  | Continuing(Applied(b, stt)) => {
      let {ctx, env, stk} = stt
      let stk = show_stack(stk->mapWithIndex(show_stkFrm))
      let now =
        <p className="now box called">
          {label("Evaluating the function body")}
          <br />
          {blank(string_of_block(b))}
          <br />
          {label("in context ")}
          {show_ctx(ctx)}
          <br />
          {label("in environment ")}
          {show_env(env)}
        </p>
      show_state(stk, now, show_all_envs(), show_all_havs())
    }

  | Continuing(Looping(_e, _b, exp, stt)) => {
      let {ctx, env, stk} = stt
      let stk = show_stack(stk->mapWithIndex(show_stkFrm))
      let now =
        <div className="now box looping">
          <p> {label("Met a loop")} </p>
          {show_expr(exp)}
          <p>
            {label("in context ")}
            {show_ctx(ctx)}
          </p>
          <p>
            {label("in environment ")}
            {show_env(env)}
          </p>
        </div>
      show_state(stk, now, show_all_envs(), show_all_havs())
    }

  | Continuing(Setting(x, v, stt)) => {
      let {ctx, env, stk} = stt
      let stk = show_stack(stk->mapWithIndex(show_stkFrm))
      let now =
        <div className="now box replacing">
          <p>
            {label("Replacing the value of ")}
            {blank(unann(x))}
            {label(" with ")}
            {show_value(v)}
          </p>
          <p>
            {label("in context ")}
            {show_ctx(ctx)}
          </p>
          <p>
            {label("in environment ")}
            {show_env(env)}
          </p>
        </div>
      show_state(stk, now, show_all_envs(), show_all_havs())
    }

  | Continuing(VecSetting((id, _vs), i, v_val, stt)) => {
      let {ctx, env, stk} = stt
      let stk = show_stack(stk->mapWithIndex(show_stkFrm))
      let now =
        <div className="now box replacing">
          <p>
            {label("Replacing ")}
            {blank(`@${Int.toString(id)}`)}
            {label("’s ")}
            {blank(i->Int.toString)}
            {label("-th element with ")}
            {show_value(v_val)}
          </p>
          <p>
            {label("in context ")}
            {show_ctx(ctx)}
          </p>
          <p>
            {label("in environment ")}
            {show_env(env)}
          </p>
        </div>
      show_state(stk, now, show_all_envs(), show_all_havs())
    }

  | Continuing(Returning(v, stk)) => {
      let stk = show_stack(stk->mapWithIndex(show_stkFrm))
      let now =
        <div className="now box returning">
          <p>
            {label("Returning ")}
            {show_value(v)}
          </p>
        </div>
      show_state(stk, now, show_all_envs(), show_all_havs())
    }
  }
}
