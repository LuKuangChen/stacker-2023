open Belt
open SMoL
open Render

@module("./url_parameters") external syntaxAtURL: string = "syntaxAtURL"
@module("./url_parameters") external printTopLevelAtURL: bool = "printTopLevelAtURL"
@module("./url_parameters") external randomSeedAtURL: string = "randomSeedAtURL"
@module("./url_parameters") external nNextAtURL: int = "nNextAtURL"
@module("./url_parameters") external programAtURL: string = "programAtURL"
@module("./url_parameters") external readOnlyMode: bool = "readOnlyMode"
@module("./url_parameters")
external make_url: (string, string, int, string, bool, bool) => string = "make_url"
@scope("window") @val external openPopUp: string => unit = "openPopUp"

exception Impossible

module FontSize = {
  type t =
    | XXS
    | XS
    | S
    | M
    | L
    | XL
    | XXL
  let default = M
  let toString = fontSize => {
    switch fontSize {
    | XXS => "xx-small"
    | XS => "x-small"
    | S => "small"
    | M => "medium"
    | XXL => "xx-large"
    | XL => "x-large"
    | L => "large"
    }
  }
  let fromString = s => {
    switch s {
    | "xx-small" => XXS
    | "x-small" => XS
    | "small" => S
    | "xx-large" => XXL
    | "x-large" => XL
    | "large" => L
    | _ => M
    }
  }
}

type running_state = {
  prevs: list<React.element>,
  now: React.element,
  nexts: list<React.element>,
  latestState: Runtime.state,
  srcMap: kindedSourceLocation => option<SExpression.sourceLocation>
}

let pool_of_randomSeed = [
  Js.Math._PI->Float.toString,
  Js.Math._E->Float.toString,
  "smol",
  "defvar",
  "deffun",
  "cond",
  "lambda",
  "2023",
]
let new_randomSeed = () => {
  let index = Js.Math.random_int(0, 1 + Array.length(pool_of_randomSeed))
  pool_of_randomSeed->Array.get(index)->Option.getWithDefault(Js.Math.random()->Float.toString)
}

type state = {
  running: option<running_state>,
  previewSyntax: option<Render.Syntax.t>,
}

type randomSeedConfig = {isSet: bool, randomSeed: string}

let translateProgram = (sk, printTopLevel, p) => {
  open Render.Syntax
  switch sk {
  | Lispy => SMoL.SMoLTranslator.translateProgram(printTopLevel, p)
  | Python => SMoL.PYTranslator.translateProgram(printTopLevel, p)
  | JavaScript => SMoL.JSTranslator.translateProgram(printTopLevel, p)
  | Pseudo => SMoL.PCTranslator.translateProgram(printTopLevel, p)
  }
}

let translateProgramFull = (sk, printTopLevel, p) => {
  open Render.Syntax
  switch sk {
  | Lispy => SMoL.SMoLTranslator.translateProgramFull(printTopLevel, p)
  | Python => SMoL.PYTranslator.translateProgramFull(printTopLevel, p)
  | JavaScript => SMoL.JSTranslator.translateProgramFull(printTopLevel, p)
  | Pseudo => SMoL.PCTranslator.translateProgramFull(printTopLevel, p)
  }
}

let make_preview = (sk: Syntax.t, printTopLevel, program) => {
  switch translateProgram(sk, printTopLevel, program) {
  | program =>
    <>
      <span>
        {React.string(`(Showing the `)}
        <u> {React.string(sk |> Syntax.toString)} </u>
        {React.string(` translation)`)}
      </span>
      <CodeEditor syntax={sk} program={program} readOnly={true} setProgram={_ => ()} />
    </>
  | exception SMoLTranslateError(err) => {
      let parseFeedback = TranslateError.toString(err)
      <span className="parse-feedback"> {React.string(parseFeedback)} </span>
    }
  }
}

let parseSMoL = (program: string) => {
  Parser.parseProgram(program)
}

let remove_lang_line = (program: string) => {
  // Js.Console.log2("Before replacement", program)
  let re = %re("/^#lang[^\n]*[\n]*/g")
  let program = Js.String.replaceByRe(re, "", program)

  // Js.Console.log2("After replacement", program)
  program
}

module SourceLocationCmp = Belt.Id.MakeComparable({
  type t = kindedSourceLocation
  let cmp = (a, b) => Pervasives.compare(a, b)
})

@react.component
let make = () => {
  let (program, rawSetProgram) = React.useState(_ => "")
  let (parseFeedback, setParseFeedback) = React.useState(_ => "")
  let setProgram = (setter: string => string) => {
    setParseFeedback(_ => "")
    rawSetProgram(v => {
      remove_lang_line(setter(v))
    })
  }
  let (nNext, setNNext) = React.useState(_ => 0)
  let (runtimeSyntax, setRuntimeSyntax) = React.useState(_ => {
    Syntax.fromString(syntaxAtURL)
  })
  let (previewSyntax: option<Render.Syntax.t>, setPreviewSyntax) = React.useState(_ => None)
  let actualRuntimeSyntax = Option.orElse(runtimeSyntax, previewSyntax)->Option.getWithDefault(Render.Syntax.Lispy)
  let (printTopLevel, setPrintTopLevel) = React.useState(_ => printTopLevelAtURL)
  let (randomSeed: randomSeedConfig, setRandomSeed) = React.useState(_ => {
    if randomSeedAtURL == "" {
      {isSet: false, randomSeed: new_randomSeed()}
    } else {
      {isSet: true, randomSeed: randomSeedAtURL}
    }
  })
  let forward = s => {
    switch s {
    | None => raise(Impossible)
    | Some({prevs: _, now: _, nexts: list{}, latestState: Terminated(_)}) => raise(Impossible)
    | Some({prevs, now, nexts: list{}, latestState: Continuing(latestState), srcMap}) => {
        let latestState = Runtime.transition(latestState)
        Some({
          prevs: list{now, ...prevs},
          now: Render.render(actualRuntimeSyntax, latestState, srcMap),
          nexts: list{},
          latestState,
          srcMap
        })
      }

    | Some({prevs, now, nexts: list{e, ...nexts}, latestState, srcMap}) =>
      Some({
        prevs: list{now, ...prevs},
        now: e,
        nexts,
        latestState,
        srcMap
      })
    }
  }
  let loadProgram = program => {
    switch translateProgramFull(Option.getWithDefault(runtimeSyntax, Lispy), printTopLevel, program) {
    | exception SMoLTranslateError(err) => {
        setParseFeedback(_ => TranslateError.toString(err))
        None
      }

    | program => {
        open SExpression
        let s: Runtime.state = Runtime.load(program, randomSeed.randomSeed, printTopLevel)
        let srcMap: kindedSourceLocation => option<sourceLocation> = {
          let map = getProgramPrint(program) -> Print.toSourceMap(module(SourceLocationCmp))
          (srcLoc) => {
            Map.get(map, srcLoc)
            // failwith("todo")
          }
        }
        Some({
          prevs: list{},
          nexts: list{},
          now: Render.render(actualRuntimeSyntax, s, srcMap),
          latestState: s,
          srcMap
        })
      }
    }
  }
  let (editorFontSize, setEditorFontSize) = React.useState(_ => FontSize.default)
  let (state, setState) = React.useState(_ => {
    let programAtURL = remove_lang_line(programAtURL)
    setProgram(_ => programAtURL)
    if nNextAtURL < 0 {
      None
    } else {
      setNNext(_ => nNextAtURL)
      let s = ref(loadProgram(programAtURL))
      for _ in 1 to nNextAtURL {
        s.contents = forward(s.contents)
      }
      s.contents
    }
  })
  let onRunClick = _evt => {
    setState(_ => loadProgram(program))
    setNNext(_ => 0)
  }
  let onStopClick = _evt => {
    setState(_ => None)
  }
  let prevable = switch state {
  | None => false
  | Some({prevs, now: _, nexts: _, latestState: _}) =>
    switch prevs {
    | list{} => false
    | list{_e, ..._prevs} => true
    }
  }
  let onPrevClick = _evt => {
    setNNext(nNext => nNext - 1)
    setState(state =>
      switch state {
      | None => raise(Impossible)
      | Some({prevs, now, nexts, latestState, srcMap}) =>
        switch prevs {
        | list{} => raise(Impossible)
        | list{e, ...prevs} => Some({prevs, now: e, nexts: list{now, ...nexts}, latestState, srcMap})
        }
      }
    )
  }
  let onNextClick = _evt => {
    setNNext(nNext => nNext + 1)
    setState(forward)
  }
  let nextable = switch state {
  | None => false
  | Some({prevs: _, now: _, nexts: list{}, latestState: Terminated(_)}) => false
  | Some({prevs: _, now: _, nexts: list{}, latestState: Continuing(_)}) => true
  | Some({prevs: _, now: _, nexts: list{_e, ..._nexts}, latestState: _}) => true
  }
  let onShare = (readOnlyMode, _evt) => {
    openPopUp(
      make_url(
        actualRuntimeSyntax->Syntax.toString,
        randomSeed.randomSeed,
        nNext,
        program,
        readOnlyMode,
        printTopLevel,
      ),
    )
  }
  let onKeyDown = evt => {
    let key = ReactEvent.Keyboard.key(evt)
    Js.log(`Key pressed (${key})`)
    if key == "j" && prevable {
      onPrevClick(evt)
    } else if key == "k" && nextable {
      onNextClick(evt)
    }
  }
  let is_running = state != None
  let runButton =
    <button onClick=onRunClick disabled={is_running}>
      <span ariaHidden={true}> {React.string("▶ ")} </span>
      {React.string("Run")}
    </button>
  let stopButton =
    <button onClick=onStopClick disabled={!is_running}>
      <span ariaHidden={true}> {React.string("⏹ ")} </span>
      {React.string("Stop")}
    </button>
  let prevButton =
    <button onClick=onPrevClick disabled={!prevable}>
      <span ariaHidden={true}> {React.string("⏮ ")} </span>
      {React.string("Previous")}
      // <kbd> {React.string("j")} </kbd>
    </button>
  let nextButton =
    <button onClick=onNextClick disabled={!nextable}>
      <span ariaHidden={true}> {React.string("⏭ ")} </span>
      {React.string("Next")}
      // <kbd> {React.string("k")} </kbd>
    </button>
  let previewProgram = previewSyntax => {
    _event => {
      setPreviewSyntax(_ => previewSyntax)
    }
  }
  let editorConfig = if readOnlyMode {
    <> </>
  } else {
    <span>
      <label> {React.string("Font size = ")} </label>
      <select
        onChange={evt => {
          let fs: string = ReactEvent.Form.currentTarget(evt)["value"]
          let fs = FontSize.fromString(fs)
          setEditorFontSize(_ => fs)
        }}>
        <option selected={FontSize.XXS == editorFontSize} value={FontSize.toString(XXS)}>
          {React.string(FontSize.toString(XXS))}
        </option>
        <option selected={FontSize.XS == editorFontSize} value={FontSize.toString(XS)}>
          {React.string(FontSize.toString(XS))}
        </option>
        <option selected={FontSize.S == editorFontSize} value={FontSize.toString(S)}>
          {React.string(FontSize.toString(S))}
        </option>
        <option selected={FontSize.M == editorFontSize} value={FontSize.toString(M)}>
          {React.string(FontSize.toString(M))}
        </option>
        <option selected={FontSize.L == editorFontSize} value={FontSize.toString(L)}>
          {React.string(FontSize.toString(L))}
        </option>
        <option selected={FontSize.XL == editorFontSize} value={FontSize.toString(XL)}>
          {React.string(FontSize.toString(XL))}
        </option>
        <option selected={FontSize.XXL == editorFontSize} value={FontSize.toString(XXL)}>
          {React.string(FontSize.toString(XXL))}
        </option>
      </select>
    </span>
  }
  let exampleProgramsAndStopButtonShortcut = if readOnlyMode {
    <> </>
  } else {
    <>
      <details>
        <summary>
          {React.string("Example programs (")}
          <a
            href="https://docs.google.com/document/d/e/2PACX-1vTMVCrUYliicrunyxftDwv6HVmBeKaRW9-VF9Xh1GUFoHMmomOczz_RRIZXPJoH8WB66x-d4GlRvwuy/pub">
            {React.string("The SMoL Language Reference")}
          </a>
          {React.string("):")}
        </summary>
        <menu ariaLabel="a list of example programs">
          <li>
            <button
              disabled={is_running}
              value="Fibonacci"
              onClick={_evt => setProgram(_ => Programs.program_fib)}>
              {React.string("Fibonacci")}
            </button>
          </li>
          <li>
            <button
              disabled={is_running}
              value="Scope"
              onClick={_evt => setProgram(_ => Programs.program_dynscope)}>
              {React.string("Scope")}
            </button>
          </li>
          <li>
            <button
              disabled={is_running}
              value="Counter1"
              onClick={_evt => setProgram(_ => Programs.program_ctr1)}>
              {React.string("Counter1")}
            </button>
          </li>
          <li>
            <button
              disabled={is_running}
              value="Counter2"
              onClick={_evt => setProgram(_ => Programs.program_ctr2)}>
              {React.string("Counter2")}
            </button>
          </li>
          <li>
            <button
              disabled={is_running}
              value="Aliasing"
              onClick={_evt => setProgram(_ => Programs.program_aliasing)}>
              {React.string("Aliasing")}
            </button>
          </li>
          <li>
            <button
              disabled={is_running}
              value="Object"
              onClick={_evt => setProgram(_ => Programs.program_object)}>
              {React.string("Object")}
            </button>
          </li>
        </menu>
      </details>
      {if is_running {
        <p>
          <mark>
            <button onClick=onStopClick disabled={!is_running}>
              <span ariaHidden={true}> {React.string("⏹ ")} </span>
              {React.string("Stop")}
            </button>
            {React.string(" before making any change!")}
          </mark>
        </p>
      } else {
        React.array([])
      }}
      <span className="parse-feedback"> {React.string(parseFeedback)} </span>
      <span>
        {React.string("Show translation at right ➡️:")}
        {React.array(
          Syntax.all
          ->Array.keep(s => s != Lispy)
          ->Array.map(s => {
            <label>
              <input
                disabled={is_running}
                type_="radio"
                name="previewSyntax"
                value={s->Syntax.toString}
                onClick={previewProgram(Some(s))}
              />
              <span> {React.string({s->Syntax.toString})} </span>
            </label>
          }),
        )}
        <label>
          <input
            disabled={is_running}
            type_="radio"
            name="previewSyntax"
            value="off"
            onClick={previewProgram(None)}
            checked={previewSyntax == None}
          />
          <span> {React.string("off")} </span>
        </label>
      </span>
    </>
  }
  let advancedConfiguration = if readOnlyMode {
    <> </>
  } else {
    <details>
      <summary>
        <span ariaHidden={true}> {React.string("⚙️ ")} </span>
        {React.string("Advanced configuration:")}
      </summary>
      <label>
        {React.string("Syntax-flavor = ")}
        {
          let onChange = evt => {
            let newValue: string = ReactEvent.Form.currentTarget(evt)["value"]
            setRuntimeSyntax(_ => Syntax.fromString(newValue))
          }
          <select onChange disabled={is_running}>
            <option selected={None == runtimeSyntax} value="auto">
              {React.string(`Auto (${actualRuntimeSyntax |> Syntax.toString})`)}
            </option>
            {React.array(
              Syntax.all->Array.map(s => {
                <option selected={Some(s) == runtimeSyntax} value={Syntax.toString(s)}>
                  {React.string({Syntax.toString(s)})}
                </option>
              }),
            )}
          </select>
        }
      </label>
      <br />
      <label>
        {React.string("Random seed = ")}
        {
          let onChange = evt => {
            let newValue: string = ReactEvent.Form.currentTarget(evt)["value"]
            setRandomSeed(_ => {isSet: true, randomSeed: newValue})
          }
          if randomSeed.isSet {
            <input disabled={is_running} type_="text" value={randomSeed.randomSeed} onChange />
          } else {
            <input
              disabled={is_running} type_="text" placeholder={randomSeed.randomSeed} onChange
            />
          }
        }
      </label>
      <br />
      <label>
        {React.string("Print the values of top-level expressions")}
        <input
          type_="checkbox"
          disabled={is_running}
          checked={printTopLevel}
          onChange={_ => setPrintTopLevel(v => !v)}
        />
      </label>
    </details>
  }

  let (dragging, setDragging) = React.useState(() => false)
  let (editorWidth, setEditorWidth) = React.useState(() => None)
  <main
    onKeyDown
    onMouseMove={event => {
      if dragging {
        let x = ReactEvent.Mouse.clientX(event)
        setEditorWidth(_ => Some(x))
      }
    }}
    onMouseUp={event => {
      setDragging(_ => false)
    }}>
    <section
      id="program-source"
      style={switch editorWidth {
      | None => {}
      | Some(editorWidth) =>
        ReactDOM.Style.make(~width=`calc(${Belt.Int.toString(editorWidth)}px - 0.5ex)`, ())
      }}>
      {exampleProgramsAndStopButtonShortcut}
      {editorConfig}
      <div
        ariaLabel="the code editor, press Esc then Tab to escape!"
        style={{
          fontSize: FontSize.toString(editorFontSize),
        }}>
        <CodeEditor
          syntax={if is_running {
            actualRuntimeSyntax
          } else {
            Lispy
          }}
          program={if (is_running && actualRuntimeSyntax != Lispy) {
            translateProgram(actualRuntimeSyntax, printTopLevel, program)
          } else {
            program
          }}
          readOnly={is_running}
          setProgram
        />
      </div>
    </section>
    <div
      id="split"
      onMouseDown={event => {
        setDragging(_ => true)
      }}
    />
    <section id="stacker">
      {advancedConfiguration}
      <menu id="nav-trace" ariaLabel="toolbar">
        {if readOnlyMode {
          <> </>
        } else {
          <>
            <li> {runButton} </li>
            <li> {stopButton} </li>
          </>
        }}
        <li> {prevButton} </li>
        <li> {nextButton} </li>
        <li>
          <button onClick={onShare(readOnlyMode)} disabled={!is_running}>
            <span ariaHidden={true}> {React.string("🔗 ")} </span>
            {React.string("Share This Configuration")}
          </button>
        </li>
        {if readOnlyMode {
          <li>
            <a
              href={make_url(
                actualRuntimeSyntax->Syntax.toString,
                "",
                -1,
                program,
                false,
                printTopLevel,
              )}>
              {React.string("✎ edit")}
            </a>
          </li>
        } else {
          <li>
            <button onClick={onShare(true)} disabled={!is_running}>
              <span ariaHidden={true}> {React.string("🔗 ")} </span>
              {React.string("Share Read-only Version")}
            </button>
          </li>
        }}
      </menu>
      {switch state {
      | None =>
        <>
          <p>
            {React.string("To start tracing, click ")}
            <button onClick=onRunClick disabled={is_running}>
              <span ariaHidden={true}> {React.string("▶ ")} </span>
              {React.string("Run")}
            </button>
            {React.string(".")}
          </p>
          {switch previewSyntax {
          | None => <> </>
          | Some(sk) => make_preview(sk, printTopLevel, program)
          }}
        </>
      | Some(s) => s.now
      }}
    </section>
  </main>
}
