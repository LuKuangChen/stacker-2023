/*

This file convert smol states to react elements.

*/

open Smol
open Utilities
open Belt
open List
open Stringify_smol

type syntax_kind =
  | Lisp
  | JavaScript
  | Python

let id = x => x

type translator = {
  tr_term: string => string,
  tr_expr: string => string,
  tr_top_level: string => string,
  tr_fun_body: string => string,
}

let adjust_syntax = sk => {
  switch sk {
  | Lisp => {
      tr_term: id,
      tr_expr: id,
      tr_top_level: id,
      tr_fun_body: id,
    }
  | JavaScript => {
      tr_term: Smol_to_js.smol_to_js(Term),
      tr_expr: Smol_to_js.smol_to_js(Expr(false)),
      tr_top_level: Smol_to_js.smol_to_js(Stat),
      tr_fun_body: Smol_to_js.smol_to_js(Return),
    }
  | Python => {
      tr_term: Smol_to_py.smol_to_py(Term),
      tr_expr: Smol_to_py.smol_to_py(Expr(false)),
      tr_top_level: Smol_to_py.smol_to_py(Stat),
      tr_fun_body: Smol_to_py.smol_to_py(Return),
    }
  }
}

let label = React.string

exception Impossible(string)
let render: (syntax_kind, Smol.state) => React.element = (sk, s) => {
  let {tr_term, tr_expr, tr_fun_body, tr_top_level} = adjust_syntax(sk)

  let show_value = e => {
    blank(string_of_value(e) |> tr_expr)
  }

  let show_ctx = ctx => {
    let ctx = ctx->map(string_of_ctxFrame)
    let ctx = reduce(ctx, "❓", (sofar, f) => f(sofar))
    blank(ctx |> tr_fun_body)
  }

  let show_top_level_ctx = ctx => {
    let ctx = ctx->map(string_of_ctxFrame)
    let ctx = reduce(ctx, "❓", (sofar, f) => f(sofar))
    blank(ctx |> tr_top_level)
  }

  let show_envFrm = (frm: environmentFrame) => {
    if Array.length(frm.content) == 0 {
      React.string("nothing")
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
              {blank(x->tr_expr)}
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
    if allEnvs.contents === list{} {
      <p> {label("(No environments)")} </p>
    } else {
      <ol className="box-list" ariaLabel="a list of all environments">
        {React.array(allEnvs.contents->reverse->mapWithIndex(show_one_env)->List.toArray)}
      </ol>
    }
  }

  let show_one_hav = (key: int, val: value): React.element => {
    let key = Int.toString(key)
    switch val {
    | VFun(Udf(id, name, ann, xs, body, env)) => {
        let string_of_udf = (name, xs, body) => {
          switch name.contents {
          | None => string_of_expr_lam(xs, body)
          | Some(f) => string_of_def_fun(f, xs, body)
          }
        }
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
              {blank(
                string_of_udf(
                  name,
                  xs->List.fromArray->map(unann),
                  string_of_block(body),
                ) |> tr_term,
              )}
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

    | _ => raise(Impossible("Not supported heap-allocated value"))
    }
  }

  let show_all_havs = () => {
    if allHavs.contents === list{} {
      <p> {label("(No heap-allocated values)")} </p>
    } else {
      <ol className="box-list" ariaLabel="a list of all heap-allocated values">
        {React.array(allHavs.contents->reverse->mapWithIndex(show_one_hav)->List.toArray)}
      </ol>
    }
  }

  let string_of_error = err => {
    switch err {
    | UnboundIdentifier(symbol) => `The variable \`${symbol}\` is not defined.`
    | UsedBeforeInitialization(symbol) => `The variable \`${symbol}\` hasn't been assigned a value.`
    | ExpectButGiven(string, _value) => `Expecting a ${string}.`
    | ArityMismatch(_arity, int) =>
      `Expecting a function that accept ${Int.toString(int)} arguments.`
    | OutOfBound(length, index) =>
      `Expecting an index less than the length of the vector (${Int.toString(
          length,
        )}), found ${Int.toString(index)}.`
    | UserRaised(message) => message
    }
  }

  let show_stackFrm = (key: int, frm: stackFrame) => {
    let (ctx, env) = frm
    <li key={Int.toString(key)} className="stack-frame box">
      {label("Waiting for a value")}
      <br />
      {label("in context ")}
      {if key == 0 {
        show_top_level_ctx(ctx)
      } else {
        show_ctx(ctx)
      }}
      <br />
      {label("in environment ")}
      {show_env(env)}
    </li>
  }

  let show_stack = frms => {
    if frms == list{} {
      <p> {React.string("(No stack frames)")} </p>
    } else {
      <ol className="box-list" ariaLabel="stack frames, with the oldest at the top">
        {React.array(frms->reverse->mapWithIndex(show_stackFrm)->List.toArray)}
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
  let nowOfTerminatedState = s => {
    switch s {
    | Err(err) =>
      <div className="now box errored">
        <p> {label("Errored")} </p>
        {blank(string_of_error(err))}
      </div>

    | Tm(vs) =>
      <div className="now box terminated">
        <p> {label("Terminated")} </p>
        {blank(String.concat("\n", vs->reverse->map(string_of_value)))}
      </div>
    }
  }
  let nowOfRedex = (redex, ctx, env) => {
    switch redex {
    | Applying(f, vs) =>
      <p className="now box calling">
        {label("Calling ")}
        {blank(string_of_list(list{string_of_value(f), ...vs->map(string_of_value)})->tr_expr)}
        <br />
        {label("in context ")}
        {ctx}
        <br />
        {label("in environment ")}
        {env}
      </p>

    // | Looping(_e, _b, exp) => {
    //     let {ctx, env, stk} = stt
    //     let stk = show_stack(stk)
    //       <div className="now box looping">
    //         <p> {label("Met a loop")} </p>
    //         {blank(string_of_expr(exp)->adjust)}
    //         <p>
    //           {label("in context ")}
    //           {ctx}
    //         </p>
    //         <p>
    //           {label("in environment ")}
    //           {env}
    //         </p>
    //       </div>
    //   }

    | Setting(x, v) =>
      <p className="now box replacing">
        {label("Rebinding a variable ")}
        <br />
        {blank(string_of_expr_set(unann(x), string_of_value(v))->tr_expr)}
        <br />
        {label("in context ")}
        {ctx}
        <br />
        {label("in environment ")}
        {env}
      </p>

    | VecSetting((id, _vs), i, v_val) =>
      <p className="now box replacing ">
        {label("Replacing a vector element ")}
        <br />
        {blank(
          string_of_expr_app(
            string_of_prm(VecSet),
            list{`@${Int.toString(id)}`, Int.toString(i), string_of_value(v_val)},
          )->tr_expr,
        )}
        <br />
        {label("in context ")}
        {ctx}
        <br />
        {label("in environment ")}
        {env}
      </p>
    }
  }
  let show_continuing_state = state => {
    switch state {
    | Returning(v, stk) => {
        let stk = show_stack(stk)
        let now =
          <div className="now box returning">
            <p>
              {label("Returning ")}
              {show_value(v)}
            </p>
          </div>
        show_state(stk, now, show_all_envs(), show_all_havs())
      }

    | Entering(entrance, b, env, stk) => {
        let stk = show_stack(stk)
        let now =
          <p className="now box called">
            {label(`Evaluating ${stringOfEntrance(entrance)}`)}
            <br />
            {blank(string_of_block(b)->tr_fun_body)}
            <br />
            {label("in environment ")}
            {show_env(env)}
          </p>
        show_state(stk, now, show_all_envs(), show_all_havs())
      }

    | Reducing(redex, stt) => {
        let {ctx, env, stk} = stt
        let now = nowOfRedex(
          redex,
          ctx->if stk == list{} {
            show_top_level_ctx
          } else {
            show_ctx
          },
          env->show_env,
        )
        let stk = show_stack(stk)
        show_state(stk, now, show_all_envs(), show_all_havs())
      }
    }
  }
  switch s {
  | Terminated(state) =>
    show_state(show_stack(list{}), nowOfTerminatedState(state), show_all_envs(), show_all_havs())
  | Continuing(state) => show_continuing_state(state)
  }
}
