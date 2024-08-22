/*

This file convert smol states to react elements.

*/

open Belt
open List
open SMoL
open SExpression
open Runtime

let reactOfPrint = (p: SMoL.print<sourceLocation>): React.element => {
  let rec reactOfAnnotatedPrint = ({it, ann}: SMoL.print<_>) => {
    let ann = switch ann {
    | None => it => it
    | Some(ann) =>
      it => {
        let className = SExpression.SourceLocation.toString(ann)
        <span title={ann->SExpression.SourceLocation.toString} className> {it} </span>
      }
    }
    switch it {
    | Plain("") => <> </>
    | Group(list{}) => <> </>
    | Plain(s) => ann(React.string(s))
    | Group(es) => ann(React.array(es->List.toArray->Array.map(reactOfAnnotatedPrint)))
    }
  }
  Js.Console.log(p)
  reactOfAnnotatedPrint(p)
}

let substituteById = (p: print<'id>, id: 'id, q: Print.t<'id>): print<'id> => {
  let rec sub = ({ann, it}: print<'id>): print<'id> => {
    if ann == Some(id) {
      {ann, it: q}
    } else {
      let it = switch it {
      | Plain(s) => Plain(s)
      | Group(es) => Group(List.map(es, sub))
      }
      {ann, it}
    }
  }
  sub(p)
}

module Syntax = {
  type t =
    | Lispy
    | Python
    | JavaScript
    | Pseudo
  let toString = t => {
    switch t {
    | Lispy => "Lispy"
    | Python => "Python"
    | JavaScript => "JavaScript"
    | Pseudo => "Pseudo"
    }
  }
  let fromString = s => {
    switch s {
    | "Lispy" => Some(Lispy)
    | "PY" => Some(Python)
    | "Python" => Some(Python)
    | "JS" => Some(JavaScript)
    | "JavaScript" => Some(JavaScript)
    | "Pseudo" => Some(Pseudo)
    | _ => None
    }
  }
  let all = [Lispy, Python, JavaScript, Pseudo]
}

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

let blankElem = e => {
  <code className="blank"> {e} </code>
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
let render: (Syntax.t, state) => React.element = (sk, s) => {
  let printName = switch sk {
  | Lispy => SMoLPrinter.printName
  | JavaScript => JSPrinter.printName
  | Python => PYPrinter.printName
  | Pseudo => PCPrinter.printName
  }

  let printTerm = switch sk {
  | Lispy => SMoLPrinter.printStandAloneTerm
  | JavaScript => JSPrinter.printStandAloneTerm
  | Python => PYPrinter.printStandAloneTerm
  | Pseudo => PCPrinter.printStandAloneTerm
  }

  let printOutput = switch sk {
  | Lispy => SMoLPrinter.printOutput
  | JavaScript => JSPrinter.printOutput
  | Python => PYPrinter.printOutput
  | Pseudo => PCPrinter.printOutput
  }

  let dummyAnn = it => {
    {
      ann: {begin: {ln: 0, ch: 0}, end: {ln: 0, ch: 0}},
      it,
    }
  }

  let printTerm = (t: termNode<_>): string => {
    printTerm(dummyAnn(t))
  }

  let printExp = (e: expressionNode<sourceLocation>): string => {
    printTerm(Exp(e))
  }

  let expr_of_value = (v: value): expressionNode<_> => {
    switch v {
    // Constants
    | Con(constant) => Con(constant)
    // Functions
    | VFun(function) => Con(Sym(`@${function.id |> Int.toString}`))
    | VGen(generator) => Con(Sym(`@${generator.id |> Int.toString}`))
    // Vectors
    | Vec(vector) => Con(Sym(`@${vector.id |> Int.toString}`))
    }
  }

  let printVal = (v: value) => {
    printExp(v->expr_of_value)
  }

  let printOfValue = (v: value): Print.t<'id> => {
    Plain(printVal(v))
  }

  let renderBodyContext = (ctx: pile<contextFrame, bodyBase>): React.element => {
    // Js.Console.log("body context")
    // Js.Console.log(ctx)
    let {topping, base} = ctx
    let print = ref(getPrint(base.base))
    // plug values in
    topping->List.forEach(f => {
      valuesOfFrame(f)->List.forEach(((v, id)) => {
        print := substituteById(print.contents, id, printOfValue(v))
      })
    })
    // plug hole in
    let hole = Option.getWithDefault(
      topping->List.head->Option.map(holeOfFrame),
      holeOfBodyBase(base.base.it),
    )
    print := substituteById(print.contents, hole, Plain("◌"))
    // convert to React.element
    blankElem(reactOfPrint(print.contents))
  }

  let renderProgramContext = (ctx: pile<contextFrame, programBase>): React.element => {
    // Js.Console.log("program context")
    // Js.Console.log(ctx)
    let {topping, base} = ctx
    let print = ref(getPrint(base))
    topping->List.forEach(f => {
      valuesOfFrame(f)->List.forEach(((v, id)) => {
        print := substituteById(print.contents, id, printOfValue(v))
      })
    })
    // plug hole in
    let hole = Option.getWithDefault(
      topping->List.head->Option.map(holeOfFrame),
      holeOfProgramBase(base.it),
    )
    print := substituteById(print.contents, hole, Plain("◌"))
    // convert to React.element
    blankElem(reactOfPrint(print.contents))
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
              {blank(v.contents->Option.map(printVal)->Option.getWithDefault("💣"))}
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
          | Suspended(ctx, env) =>
            <>
              <br />
              {React.string("with context ")}
              {{...ctx, base: {isGen: None, base: ctx.base}}->renderBodyContext}
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
    | VFun({id, isGen, sourceLocation: ann, print, env}) => {
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
            {blank(print.it |> Print.toString)}
            <br />
          </details>
          <span>
            {React.string("with environment ")}
            {show_env(env)}
          </span>
        </li>
      }

    | Vec({id, contents: vs}) => {
        let id = id->Int.toString
        <li key id={`def-${id}`} className="vec box">
          {defblank(`@${id}`)}
          <br />
          {React.string("vec")}
          {React.array(
            vs
            ->Array.map(printVal)
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
    | ExpectButGiven(string, value) => `Expecting a ${string}, given ${printVal(value)}.`
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
      {ctx->renderProgramContext}
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

  let show_state = (stack, now, envs, heap, stdout: output) => {
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
            <pre className="stdout"> {printOutput(List.reverse(stdout)) |> React.string} </pre>
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
      </div>
    }
  }
  let nowOfRedex = (redex, ctx, env) => {
    switch redex {
    | Yielding(v) =>
      <p className="now box returning">
        {React.string("Yielding ")}
        {blank(printExp(v->expr_of_value))}
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
        {blank(printExp(v->expr_of_value))}
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
          printExp(
            App(expr_of_value(f) |> dummyAnn, vs->List.map(v => v |> expr_of_value |> dummyAnn)),
          ),
        )}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | Nexting({id}) =>
      <p className="now box calling">
        {React.string("Resuming ")}
        {blank(`@${id |> Int.toString}`)}
        <br />
        {React.string("in context ")}
        {ctx}
        <br />
        {React.string("in environment ")}
        {env}
      </p>

    | Setting(x, v) =>
      let x = {
        let {ann: {sourceLocation}, it} = x
        {ann: sourceLocation, it}
      }
      <p className="now box replacing">
        {React.string("Rebinding a variable ")}
        <br />
        {blank(Set(x, expr_of_value(v)->dummyAnn) |> printExp)}
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
              expr_of_value(Vec(v_vec)) |> dummyAnn,
              expr_of_value(Con(Num(i |> Int.toFloat))) |> dummyAnn,
              expr_of_value(v_val) |> dummyAnn,
            },
          )->printExp,
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
              {blank(v->printVal)}
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
            {blank(b.ann.print |> Print.toString)}
            <br />
            {React.string("in environment ")}
            {show_env(env)}
          </p>
        show_state(stk, now)
      }

    | Reducing(redex, stk) => {
        let (ctx, env, stk) = switch stk {
        | {topping: list{}, base: {ctx, env}} => (
            ctx->renderProgramContext,
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
