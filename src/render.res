/*

This file convert smol states to react elements.

*/

open Belt
open List
open SMoL
open SExpression
open Runtime

type syntax_kind =
  | Lisp
  | JavaScript
  | Python

let id = x => x

let blank = s => {
  <code className="blank"> {React.string(s)} </code>
}

let adjust_syntax = (sk): stringifier => {
  switch sk {
  | Lisp => stringify
  | JavaScript => stringifyAsJS
  | Python => stringifyAsPY
  }
}

let stringify_context = (stringify: stringifier) => {
  let {
    string_of_result,
    string_of_expr,
    string_of_def,
    string_of_block,
    string_of_program,
  } = stringify

  let interp_ctx = (ctx, any: annotated<expression>): annotated<expression> =>
    dummy_ann(
      switch ctx {
      | Set1(x, ()) => Set(x, any)
      | App1((), es) => App(any, es)
      | App2(v, vs, (), es) => App(observe(v), list{...vs->List.map(observe), any, ...es})
      | AppPrm1(p, vs, (), es) => AppPrm(p, list{...vs->List.map(observe), any, ...es})
      | Let1(xvs, (x, ()), xes, block) =>
        Let(list{...xvs->List.map(((x, v)) => (x, observe(v))), (x, any), ...xes}, block)
      | If1((), e_thn, e_els) => If(any, e_thn, e_els)
      | Cnd1((), block, ebs, ob) => Cnd(list{(any, block), ...ebs}, ob)
      | Bgn1((), es, e) => Bgn(list{any, ...es}, e)
      },
    )

  let interp_body_base = (base: bodyBase, any: annotated<expression>): block => {
    switch base {
    | BdyDef(x, (), (ts, e)) => (list{Def(dummy_ann(Var(x, any))), ...ts}, e)
    | BdyExp((), (ts, e)) => (list{Exp(any), ...ts}, e)
    | BdyRet => (list{}, any)
    }
  }

  let string_of_value = v => string_of_result(result_of_value(v))

  let expr_of_value = (v: value): annotated<expression> => {
    dummy_ann(Ref(dummy_ann(string_of_value(v))))
  }

  let interp_program_base = (base: programBase, any: annotated<expression>): program => {
    let (redex, ts) = base
    let redex: term = switch redex {
    | Def(x) => Def(dummy_ann(Var(x, any)))
    | Exp => Exp(any)
    }
    list{redex, ...ts}
  }

  let block_of_body_context = (any: annotated<expression>, ctx: pile<contextFrame, bodyBase>) => {
    let embed_in_topping: annotated<expression> => annotated<expression> = any =>
      List.reduce(ctx.topping->List.map(interp_ctx), any, (any, embed) => embed(any))
    let embed_in_base: annotated<expression> => block = interp_body_base(ctx.base)
    any |> embed_in_topping |> embed_in_base
  }

  let block_of_program_context = (
    any: annotated<expression>,
    ctx: pile<contextFrame, programBase>,
  ) => {
    let embed_in_topping: annotated<expression> => annotated<expression> = any =>
      List.reduce(ctx.topping->List.map(interp_ctx), any, (any, embed) => embed(any))
    let embed_in_base: annotated<expression> => program = interp_program_base(ctx.base)
    any |> embed_in_topping |> embed_in_base
  }

  let placeholder = dummy_ann(Ref(dummy_ann("❓")))

  let string_of_body_context = ctx => string_of_block(block_of_body_context(placeholder, ctx))
  let string_of_program_context = ctx =>
    String.concat("\n",
      list{
        ...allVals->List.fromArray->List.map(string_of_value),
        string_of_program(block_of_program_context(placeholder, ctx))})
  let string_of_fun = (f, xs, body) => {
    switch f {
    | None => string_of_expr(dummy_ann(Lam(xs, body)))
    | Some(f) => string_of_def(dummy_ann((Fun(dummy_ann(f), xs, body): definition)))
    }
  }

  (expr_of_value, string_of_value, string_of_fun, string_of_body_context, string_of_program_context)
}

exception Impossible(string)
let render: (syntax_kind, state) => React.element = (sk, s) => {
  let stringify = adjust_syntax(sk)
  let {string_of_expr, string_of_block, string_of_term} = stringify
  let (
    expr_of_value,
    string_of_value,
    string_of_fun,
    string_of_body_context,
    string_of_program_context,
  ) = stringify_context(stringify)

  let show_envFrm = (frm: environmentFrame) => {
    if Array.length(frm.content) == 0 {
      React.string("nothing")
    } else {
      <span className="binds">
        {React.array(
          Array.mapWithIndex(frm.content, (key, xv) => {
            let key = Int.toString(key)
            let (x, v) = xv
            <span key className="bind">
              {blank(string_of_expr(dummy_ann(Ref(dummy_ann(x)))))}
              <span ariaHidden={true}> {React.string(" ↦ ")} </span>
              <span className="sr-only"> {React.string("to")} </span>
              {blank(v.contents->Option.map(string_of_value)->Option.getWithDefault("💣"))}
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
            {React.string("binds ")}
            {show_envFrm(frm)}
            <br />
            {React.string("extending ")}
            {show_env(rest)}
          </span>
        </li>
      }
    }
  }

  let show_all_envs = () => {
    if allEnvs.contents === list{} {
      <p> {React.string("(No environments)")} </p>
    } else {
      <ol className="box-list" ariaLabel="a list of all environments">
        {React.array(allEnvs.contents->reverse->List.mapWithIndex(show_one_env)->List.toArray)}
      </ol>
    }
  }

  let show_one_hav = (key: int, val: value): React.element => {
    let key = Int.toString(key)
    switch val {
    | VFun(Udf(id, name, ann, xs, body, env)) => {
        let id = id->Int.toString
        // let name = name.contents->Option.map(s => ":" ++ s)->Option.getWithDefault("")
        // let id = id ++ name
        <li key className="fun box">
          {blank(`@${id}`)}
          {React.string(", a function")}
          <br />
          <details>
            <summary>
              {React.string(`at line ${(ann.begin.ln + 1)->Int.toString}`)}
              <small> {React.string(`:${(ann.begin.ch + 1)->Int.toString}`)} </small>
              {React.string(` to ${(ann.end.ln + 1)->Int.toString}`)}
              <small> {React.string(`:${(ann.end.ch + 1)->Int.toString}`)} </small>
            </summary>
            <p> {blank(string_of_fun(name.contents, xs |> List.fromArray, body))} </p>
          </details>
          {React.string("with environment ")}
          {show_env(env)}
        </li>
      }

    | Vec(id, vs) => {
        let id = id->Int.toString
        <li key className="vec box">
          {blank(`@${id}`)}
          {React.string(", a vector")}
          <br />
          {React.string("with contents")}
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
      <p> {React.string("(No heap-allocated values)")} </p>
    } else {
      <ol className="box-list" ariaLabel="a list of all heap-allocated values">
        {React.array(allHavs.contents->reverse->List.mapWithIndex(show_one_hav)->List.toArray)}
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

  let show_body_frame = (key: int, frm: frame<bodyBase>) => {
    let {ctx, env} = frm
    <li key={Int.toString(key)} className="stack-frame box">
      {React.string("Waiting for a value")}
      <br />
      {React.string("in context ")}
      {blank(ctx->string_of_body_context)}
      <br />
      {React.string("in environment ")}
      {show_env(env)}
    </li>
  }

  let show_program_frame = (key: int, frm: frame<programBase>) => {
    let {ctx, env} = frm
    <li key={Int.toString(key)} className="stack-frame box">
      {React.string("Waiting for a value")}
      <br />
      {React.string("in context ")}
      {blank(ctx->string_of_program_context)}
      <br />
      {React.string("in environment ")}
      {show_env(env)}
    </li>
  }

  let show_stack = (stk: stack) => {
    <ol className="box-list" ariaLabel="stack frames, with the oldest at the top">
      {show_program_frame(-1, stk.base)}
      {React.array(stk.topping->reverse->List.mapWithIndex(show_body_frame)->List.toArray)}
    </ol>
  }

  let show_state = (stack, now, envs, heap) => {
    <article id="stacker-configuration" ariaLabel="the current stacker configuration">
      <section id="stack-and-now">
        <h1> {React.string("Stack Frames & The Program Counter")} </h1>
        {stack}
        <hr />
        {now}
      </section>
      <section id="environments">
        <h1> {React.string("Environments")} </h1>
        {envs}
      </section>
      <section id="heap">
        <h1> {React.string("Heap-allocated Values")} </h1>
        {heap}
      </section>
    </article>
  }
  let nowOfTerminatedState = s => {
    switch s {
    | Err(err) =>
      <div className="now box errored">
        <p> {React.string("Errored")} </p>
        {blank(string_of_error(err))}
      </div>

    | Tm =>
      <div className="now box terminated">
        <p> {React.string("Terminated")} </p>
        {blank(String.concat("\n", allVals->List.fromArray->List.map(v => v |> string_of_value)))}
      </div>
    }
  }
  let nowOfRedex = (redex, ctx, env) => {
    switch redex {
    | AppPrming(f, vs) =>
      <p className="now box calling">
        {React.string("Calling ")}
        {blank(
          dummy_ann(
            (AppPrm(f, vs->List.map(expr_of_value)): expression),
          )->string_of_expr,
        )}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>
    | Applying(f, vs) =>
      <p className="now box calling">
        {React.string("Calling ")}
        {blank(
          dummy_ann(
            (App(expr_of_value(f), vs->List.map(expr_of_value)): expression),
          )->string_of_expr,
        )}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | Setting(x, v) =>
      <p className="now box replacing">
        {React.string("Rebinding a variable ")}
        <br />
        {blank(Set(x, expr_of_value(v))->dummy_ann->(x=>(Exp(x) : term))->string_of_term)}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | VecSetting(v_vec, i, v_val) =>
      <p className="now box replacing ">
        {React.string("Replacing a vector element ")}
        <br />
        {blank(
          AppPrm(
            VecSet,
            list{
              expr_of_value(Vec(v_vec)),
              expr_of_value(Con(Num(i |> Int.toFloat))),
              expr_of_value(v_val),
            },
          )
          ->dummy_ann
          ->(x=>(Exp(x) : term))->string_of_term,
        )}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>
    }
  }
  let show_continuing_state = state => {
    switch state {
    | Returning(v, stk: stack) => {
        let stk = show_stack(stk)
        let now =
          <div className="now box returning">
            <p>
              {React.string("Returning ")}
              {blank(v->string_of_value)}
            </p>
          </div>
        show_state(stk, now, show_all_envs(), show_all_havs())
      }

    | Entering(entrance, b, env, stk: stack) => {
        let stk = show_stack(stk)
        let now =
          <p className="now box called">
            {React.string(`Evaluating ${string_of_entrace(entrance)}`)}
            <br />
            {blank(string_of_block(b))}
            <br />
            {React.string("in environment ")}
            {show_env(env)}
          </p>
        show_state(stk, now, show_all_envs(), show_all_havs())
      }

    | Reducing(redex, stk) => {
        let (ctx, env, stk) = switch stk {
        | {topping: list{}, base: {ctx, env}} => (
            blank(ctx->string_of_program_context),
            env->show_env,
            <p> {React.string("(No stack frames)")} </p>,
          )
        | {topping: list{{ctx, env}, ...topping}, base} => (
            blank(ctx->string_of_body_context),
            env->show_env,
            show_stack({topping, base}),
          )
        }
        let now = nowOfRedex(redex, ctx, env)
        show_state(stk, now, show_all_envs(), show_all_havs())
      }
    }
  }
  switch s {
  | Terminated(state) =>
    show_state(
      <p> {React.string("(No stack frames)")} </p>,
      nowOfTerminatedState(state),
      show_all_envs(),
      show_all_havs(),
    )
  | Continuing(state) => show_continuing_state(state)
  }
}
