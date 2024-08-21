/*

This file convert smol states to react elements.

*/

open Belt
open List
open SMoL
open SExpression
open Runtime

type syntax_kind =
  | Lispy
  | JavaScript
  | Python
  | Common

let id = x => x

@module("./highlight") external highlight: string => unit = "highlight"
@module("./highlight") external lowlight: string => unit = "lowlight"

let array_interleave = (arr, sep) => {
  if Array.length(arr) == 1 {
    arr
  } else {
    let prefix = [Array.getExn(arr, 0)]
    let suffix = Array.flatMap(Array.slice(arr, ~offset=1, ~len=Array.length(arr) - 1), e => [
      sep,
      e,
    ])
    Array.concat(prefix, suffix)
  }
}

let re_split = (s, re) => {
  // Js.Console.log3("Before split", s, Js.String.match_(re, s))
  switch Js.String.match_(re, s) {
  | None => [s]
  | Some(fragments) =>
    Array.reduce(fragments, [s], (ss, frag) => {
      let frag = Option.getExn(frag)
      Array.flatMap(ss, s => {
        // Js.Console.log(`Spliting "${s}" by fragment "${frag}"`)
        array_interleave(Js.String.split(frag, s), frag)
      })
    })
  }
}

let blank = s => {
  let s = re_split(s, %re("/@[-0-9a-zA-Z]+/"))
  <code className="blank">
    // {React.string(String.concat("<address>", s->List.fromArray))}
    {React.array(
      s->Array.mapWithIndex((i, s) => {
        if mod(i, 2) == 0 {
          React.string(s)
        } else {
          // must be an address
          let address = String.sub(s, 1, String.length(s) - 1)
          <span
            className={`ref ref-${address}`}
            onMouseEnter={_ => {
              highlight(`#def-${address}`)
            }}
            onMouseLeave={_ => {
              lowlight(`#def-${address}`)
            }}>
            {React.string(s)}
          </span>
        }
      }),
    )}
  </code>
}

let defblank = s => {
  <code className="blank def"> {React.string(s)} </code>
}

let safe_f = (string_of, safe_string_of, src) => {
  switch string_of(src) {
  | dst => dst
  | exception SMoL.SMoLParseError(err) => {
      let parseFeedback = SMoL.ParseError.toString(err)
      `;; ${parseFeedback}\n${safe_string_of(src)}`
    }
  | exception SMoL.SMoLPrintError(err) => `;; ${err}\n${safe_string_of(src)}`
  }
}

exception Impossible(string)
let render: (syntax_kind, state) => React.element = (sk, s) => {
  let outputletOfValue = (v: value): SMoL.outputlet => {
    let rec outputletOfValue = (v: value): SMoL.val => {
      switch v {
      // Constants
      | Con(constant) => Con(constant)
      // Functions
      | VFun(function) => Con(Sym(function.id |> Int.toString))
      | VGen(generator) => Con(Sym(generator.id |> Int.toString))
      // Vectors
      | Vec(vector) => Vec(vector.contents->Array.map(outputletOfValue)->List.fromArray)
      }
    }
    SMoL.OVal(outputletOfValue(v))
  }

  let printName = switch sk {
  | Lispy => SMoLPrinter.printName
  | JavaScript => JSPrinter.printName
  | Python => PYPrinter.printName
  | Common => PCPrinter.printName
  }

  let printOutput = switch sk {
  | Lispy => SMoLPrinter.printOutput
  | JavaScript => JSPrinter.printOutput
  | Python => PYPrinter.printOutput
  | Common => PCPrinter.printOutput
  }

  let printOutputlet = (sk, ol) => {
    printOutput(list{ol})
  }

  let string_of_value = v => {
    v |> outputletOfValue |> printOutputlet(sk)
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
            <span key className="bind">
              {blank(x.ann.print |> Print.toString)}
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
    | list{frm, ..._rest} => blank(`@${frm.id |> EnvironmentID.toString |> printName}`)
    }
  }

  let show_one_env = (key: int, env: environment): React.element => {
    switch env {
    | list{} =>
      // blank("An environment must have at least one frame.")
      raise(Impossible("An environment must have at least one frame."))
    | list{frm, ...rest} => {
        let {id, content: _} = frm
        <li
          id={`def-${id |> EnvironmentID.toString}`}
          key={Int.toString(key)}
          className="env-frame box">
          <span>
            {defblank(`@${id |> EnvironmentID.toString}`)}
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
    | VGen({id, status: gen}) => {
        let id = id->Int.toString
        let status = switch gen.contents {
        | Fresh(_b, _env) => "fresh"
        | Suspended(_ctx, _env) => "suspended"
        | Running => "running"
        | Done => "done"
        }
        <li key id={`def-${id}`} className="fun box">
          {defblank(`@${id}`)}
          {React.string(", a ")}
          {blank(status)}
          {React.string(" generator")}
          {switch gen.contents {
          | Fresh(b, env) =>
            <>
              <br />
              {blank(b.ann.print |> Print.toString)}
              <br />
              <span>
                {React.string("with environment ")}
                {show_env(env)}
              </span>
            </>
          | Suspended({topping, base}, env) =>
            <>
              <br />
              {React.string("with context ")}
              {{topping, base: {isGen: None, base}}->renderBodyContext}
              <br />
              <span>
                {React.string("with environment ")}
                {show_env(env)}
              </span>
            </>
          | Running | Done => <> </>
          }}
        </li>
      }
    | VFun({id, isGen, name, sourceLocation: ann, xs, body, env}) => {
        let id = id->Int.toString
        // let name = name.contents->Option.map(s => ":" ++ s)->Option.getWithDefault("")
        // let id = id ++ name
        <li key id={`def-${id}`} className="fun box">
          {defblank(`@${id}`)}
          {React.string(isGen ? ", a generator function" : ", a function")}
          <br />
          <details>
            <summary>
              {React.string(`at line ${(ann.begin.ln + 1)->Int.toString}`)}
              <small> {React.string(`:${(ann.begin.ch + 1)->Int.toString}`)} </small>
              {React.string(` to ${(ann.end.ln + 1)->Int.toString}`)}
              <small> {React.string(`:${(ann.end.ch + 1)->Int.toString}`)} </small>
            </summary>
            {blank(string_of_fun(name.contents, xs |> List.fromArray, body))}
            <br />
          </details>
          <span>
            {React.string("with environment ")}
            {show_env(env)}
          </span>
        </li>
      }

    | Vec({ id, contents: vs}) => {
        let id = id->Int.toString
        <li key id={`def-${id}`} className="vec box">
          {defblank(`@${id}`)}
          <br />
          {React.string("vec")}
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

    | Con(_) => raise(Impossible("Not supported heap-allocated value"))
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
    | RedefinedIdentifier(symbol, env_id) =>
      `The variable \`${symbol}\` is defined more than once in the \`@${env_id}\` environment.`
    | UsedBeforeInitialization(symbol) => `The variable \`${symbol}\` hasn't been assigned a value.`
    | ExpectButGiven(string, value) => `Expecting a ${string}, given ${string_of_value(value)}.`
    | ArityMismatch(arity, int) =>
      `Expecting a function that accept ${Int.toString(
          int,
        )} arguments, given a function that takes ${RTArity.toString(arity)} arguments.`
    | OutOfBound(length, index) =>
      `Expecting an index less than the length of the vector (${Int.toString(
          length,
        )}), found ${Int.toString(index)}.`
    | DivisionByZero => `Division by zero`
    | AnyError(message) => message
    }
  }

  let show_body_frame = (key: int, frm: frame<bodyBase>) => {
    let {ctx, env} = frm
    <li key={Int.toString(key)} className="stack-frame box">
      {React.string("Waiting for a value")}
      <br />
      {React.string("in context ")}
      {ctx->renderBodyContext}
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
    <ol className="box-list the-stack" ariaLabel="stack frames, with the oldest at the top">
      {show_program_frame(-1, stk.base)}
      {React.array(stk.topping->reverse->List.mapWithIndex(show_body_frame)->List.toArray)}
    </ol>
  }

  let show_state = (stack, now, envs, heap, stdout) => {
    <article id="stacker-configuration" ariaLabel="the current stacker configuration">
      <section id="stack-and-now">
        <h1> {React.string("Stack & Current Task")} </h1>
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
      <section id="stdout">
        <hr />
        {if stdout == list{} {
          <span> {React.string("(No output yet)")} </span>
        } else {
          <>
            <h1> {React.string("Output: ")} </h1>
            <pre className="stdout">
              {React.string(
                String.concat("\n", List.reverse(stdout)->List.map(string_of_printing)),
              )}
            </pre>
          </>
        }}
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
        // {blank(String.concat("\n", allVals->List.fromArray->List.map(v => v |> string_of_value)))}
      </div>
    }
  }
  let nowOfRedex = (redex, ctx, env) => {
    switch redex {
    | Yielding(v) =>
      <p className="now box returning">
        {React.string("Yielding ")}
        {blank(v->expr_of_value->string_of_expr)}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | Printing(v) =>
      <p className="now box printing">
        {React.string("Printing ")}
        {blank(v->expr_of_value->string_of_expr)}
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

    | Nexting(id, _status) =>
      <p className="now box calling">
        {React.string("Advancing ")}
        {blank(`@${id |> Int.toString}`)}
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
        {blank(Set(x, expr_of_value(v))->dummy_ann->((x): term => Exp(x))->string_of_term)}
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
          ->((x): term => Exp(x))
          ->string_of_term,
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
        show_state(stk, now)
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
        show_state(stk, now)
      }

    | Reducing(redex, stk) => {
        let (ctx, env, stk) = switch stk {
        | {topping: list{}, base: {ctx, env}} => (
            blank(ctx->string_of_program_context),
            env->show_env,
            <p> {React.string("(No stack frames)")} </p>,
          )
        | {topping: list{{ctx, env}, ...topping}, base} => (
            ctx->renderBodyContext,
            env->show_env,
            show_stack({topping, base}),
          )
        }
        let now = nowOfRedex(redex, ctx, env)
        show_state(stk, now)
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
      stdout.contents,
    )
  | Continuing(state) =>
    show_continuing_state(state, show_all_envs(), show_all_havs(), stdout.contents)
  }
}
