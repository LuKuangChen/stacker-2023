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

type stringifier = {
  string_of_term: term => string,
  string_of_block: block => string,
  string_of_program: program => string,
  string_of_printing: printing => string,
}
type safe_stringifier = {
  string_of_value: value => string,
  string_of_expr: annotated<expression> => string,
  string_of_term: term => string,
  string_of_block: block => string,
  string_of_program: program => string,
  string_of_printing: printing => string,
  unsafe_string_of_program: program => string,
}
let make_safe_stringifier = (stringifier: stringifier) => {
  let {string_of_term, string_of_block, string_of_program, string_of_printing} = stringifier
  {
    string_of_value: printValue,
    string_of_expr: safe_f(x => string_of_term(Exp(x)), x => SMoL.SMoLPrinter.printTerm(Exp(x))),
    string_of_term: safe_f(string_of_term, SMoL.SMoLPrinter.printTerm),
    string_of_block: safe_f(string_of_block, SMoL.SMoLPrinter.printBlock),
    string_of_program: safe_f(string_of_program, SMoL.SMoLPrinter.printProgram(true)),
    string_of_printing,
    unsafe_string_of_program: string_of_program,
  }
}

let adjust_syntax = (sk): safe_stringifier => {
  switch sk {
  | Lispy =>
    let rec string_of_printing = p => {
      switch p {
      | PCon(s) => s
      | PRef(r) => `#${Int.toString(r)}#`
      | PVec(o, es) =>
        `${switch o {
          | None => ""
          | Some(r) => `#${Int.toString(r)}#=`
          }}(${String.concat(" ", list{"mvec", ...es->List.map(string_of_printing)})})`
      }
    }
    make_safe_stringifier({
      string_of_term: SMoLPrinter.printTerm,
      string_of_block: SMoLPrinter.printBlock,
      string_of_program: SMoLPrinter.printProgram(true),
      string_of_printing,
    })
  | JavaScript =>
    let rec string_of_printing = p => {
      switch p {
      | PCon(s) => s
      | PRef(r) => `#${Int.toString(r)}#`
      | PVec(o, es) =>
        `${switch o {
          | None => ""
          | Some(r) => `#${Int.toString(r)}#=`
          }}[${String.concat(", ", es->List.map(string_of_printing))}]`
      }
    }
    make_safe_stringifier({
      string_of_term: JSPrinter.printTerm,
      string_of_block: JSPrinter.printBlock,
      string_of_program: JSPrinter.printProgram(true),
      string_of_printing,
    })
  | Python =>
    let rec string_of_printing = p => {
      switch p {
      | PCon(s) => s
      | PRef(r) => `#${Int.toString(r)}#`
      | PVec(o, es) =>
        `${switch o {
          | None => ""
          | Some(r) => `#${Int.toString(r)}#=`
          }}[${String.concat(", ", es->List.map(string_of_printing))}]`
      }
    }
    make_safe_stringifier({
      string_of_term: PYPrinter.printTerm,
      string_of_block: PYPrinter.printBlock,
      string_of_program: PYPrinter.printProgram(true),
      string_of_printing,
    })
  | Common =>
    let rec string_of_printing = p => {
      switch p {
      | PCon(s) => s
      | PRef(r) => `#${Int.toString(r)}#`
      | PVec(o, es) =>
        `${switch o {
          | None => ""
          | Some(r) => `#${Int.toString(r)}#=`
          }}vec[${String.concat(", ", es->List.map(string_of_printing))}]`
      }
    }
    make_safe_stringifier({
      string_of_term: CommonPrinter.printTerm,
      string_of_block: CommonPrinter.printBlock,
      string_of_program: CommonPrinter.printProgram(true),
      string_of_printing,
    })
  }
}

let stringify_context = (stringify: safe_stringifier) => {
  let {string_of_value, string_of_expr, string_of_block, string_of_program} = stringify

  let observe = (v: value): annotated<expression> => {
    dummy_ann(Ref(dummy_ann(string_of_value(v))))
  }

  let interp_ctx = (ctx, any: annotated<expression>): annotated<expression> =>
    dummy_ann(
      switch ctx {
      | Set1(x, ()) => Set(x, any)
      | App1((), es) => App(any, es)
      | App2(v, vs, (), es) =>
        App(observe(v), list{...vs->List.map(observe)->List.reverse, any, ...es})
      | AppPrm1(p, vs, (), es) =>
        AppPrm(p, list{...vs->List.map(observe)->List.reverse, any, ...es})
      | Let1(xvs, (x, ()), xes, block) =>
        Let(list{...xvs->List.map(((x, v)) => (x, observe(v))), (x, any), ...xes}, block)
      | If1((), e_thn, e_els) => If(any, e_thn, e_els)
      | Cnd1((), block, ebs, ob) => Cnd(list{(any, block), ...ebs}, ob)
      | Bgn1((), es, e) => Bgn(list{any, ...es}, e)
      | Yield1() => Yield(any)
      },
    )

  let interp_body_base = ({isGen: _, base}: bodyBase, any: annotated<expression>): block => {
    switch base {
    | BdyDef(x, (), (ts, e)) => (list{Def(dummy_ann(Var(x, any))), ...ts}, e)
    | BdyExp((), (ts, e)) => (list{Exp(any), ...ts}, e)
    | BdyRet => (list{}, any)
    }
  }

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

  let placeholder = "◌"
  // ❓ 🤔 🕳 👀 ⌛ ⏲ 🚀 🪧 ⭕ ⬚ ◌

  let placeholder = dummy_ann(Ref(dummy_ann(placeholder)))

  let renderBodyContext = (ctx: pile<contextFrame, bodyBase>) => {
    switch ctx.base.isGen {
    | None => blank(string_of_block(block_of_body_context(placeholder, ctx)))
    | Some((id, _status)) =>
      <table style={{display: "inline-table"}}>
        <tr>
          <td> {blank(string_of_block(block_of_body_context(placeholder, ctx)))} </td>
        </tr>
        <tr>
          <td>
            {React.string("(Generator ")}
            {blank(`@${id->Int.toString}`)}
            {React.string(")")}
          </td>
        </tr>
      </table>
    }
  }
  let string_of_program_context = ctx =>
    String.concat(
      "\n",
      list{
        // ...allVals->List.fromArray->List.map(string_of_value),
        string_of_program(block_of_program_context(placeholder, ctx)),
      },
    )
  let string_of_fun = (_f, xs, body) => {
    string_of_expr(dummy_ann(Lam(xs, body)))
  }

  (expr_of_value, string_of_value, string_of_fun, renderBodyContext, string_of_program_context)
}

exception Impossible(string)
let render: (syntax_kind, state) => React.element = (sk, s) => {
  let stringify = adjust_syntax(sk)
  let {string_of_expr, string_of_block, string_of_term, string_of_printing} = stringify
  let (
    expr_of_value,
    string_of_value,
    string_of_fun,
    renderBodyContext,
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
    | list{} =>
      // blank("An environment must have at least one frame.")
      raise(Impossible("An environment must have at least one frame."))
    | list{frm, ...rest} => {
        let {id, content: _} = frm
        <li id={`def-${id}`} key={Int.toString(key)} className="env-frame box">
          <span>
            {defblank(`@${id}`)}
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
    | VGen(id, gen) => {
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
              {blank(string_of_block(b))}
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
    | VFun(Udf(id, isGen, name, ann, xs, body, env)) => {
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

    | Vec(id, vs) => {
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
