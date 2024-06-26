open Belt
open SMoL

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

let parseSyntax = newValue =>
  switch newValue {
  | "SMoL" => Some(Render.Lispy)
  | "Lispy" => Some(Render.Lispy)
  | "JS" => Some(JavaScript)
  | "JavaScript" => Some(JavaScript)
  | "PY" => Some(Python)
  | "Python" => Some(Python)
  | "CM" => Some(Common)
  | "Common" => Some(Common)
  | _ => None
  }
let syntax_as_string = sk =>
  switch sk {
  | Render.Lispy => "Lispy"
  | JavaScript => "JavaScript"
  | Python => "Python"
  | Common => "Common"
  }

type running_state = {
  prevs: list<React.element>,
  now: React.element,
  nexts: list<React.element>,
  latestState: Runtime.state,
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
  preview: option<Render.syntax_kind>,
}

type randomSeedConfig = {isSet: bool, randomSeed: string}

let make_preview = (sk, program) => {
  switch program->SMoL.Parser.parseProgram |> Render.adjust_syntax(sk).unsafe_string_of_program {
  | program =>
    <>
      <span>
        {React.string(`(Showing the `)}
        <u> {React.string(sk |> syntax_as_string)} </u>
        {React.string(` translation)`)}
      </span>
      <CodeEditor syntax={sk} program={program} readOnly={true} setProgram={_ => ()} />
    </>
  | exception SMoLParseError(err) => {
      let parseFeedback = ParseError.toString(err)
      <span className="parse-feedback"> {React.string(parseFeedback)} </span>
    }
  | exception SMoLPrintError(err) => <span className="parse-feedback"> {React.string(err)} </span>
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
  let (syntax, setSyntax) = React.useState(_ => {
    if syntaxAtURL == "" {
      None
    } else {
      parseSyntax(syntaxAtURL)
    }
  })
  let (printTopLevel, setPrintTopLevel) = React.useState(_ => printTopLevelAtURL)
  let (randomSeed: randomSeedConfig, setRandomSeed) = React.useState(_ => {
    if randomSeedAtURL == "" {
      {isSet: false, randomSeed: new_randomSeed()}
    } else {
      {isSet: true, randomSeed: randomSeedAtURL}
    }
  })
  let (preview: option<Render.syntax_kind>, setPreview) = React.useState(_ => None)
  let runtime_syntax = Option.orElse(syntax, preview)->Option.getWithDefault(Render.Lispy)
  let forward = s => {
    switch s {
    | None => raise(Impossible)
    | Some({prevs: _, now: _, nexts: list{}, latestState: Terminated(_)}) => raise(Impossible)
    | Some({prevs, now, nexts: list{}, latestState: Continuing(latestState)}) => {
        let latestState = Runtime.transition(latestState)
        Some({
          prevs: list{now, ...prevs},
          now: Render.render(runtime_syntax, latestState),
          nexts: list{},
          latestState,
        })
      }

    | Some({prevs, now, nexts: list{e, ...nexts}, latestState}) =>
      Some({
        prevs: list{now, ...prevs},
        now: e,
        nexts,
        latestState,
      })
    }
  }
  let loadProgram = program => {
    switch parseSMoL(program) {
    | exception SMoLParseError(err) => {
        setParseFeedback(_ => ParseError.toString(err))
        None
      }

    | program => {
        let s: Runtime.state = Runtime.load(program, randomSeed.randomSeed, printTopLevel)
        Some({
          prevs: list{},
          nexts: list{},
          now: Render.render(runtime_syntax, s),
          latestState: s,
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
      | Some({prevs, now, nexts, latestState}) =>
        switch prevs {
        | list{} => raise(Impossible)
        | list{e, ...prevs} => Some({prevs, now: e, nexts: list{now, ...nexts}, latestState})
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
        runtime_syntax->syntax_as_string,
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
  let previewProgram = preview => {
    _event => {
      setPreview(_ => preview)
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
          let fs = FontSize.fromString(fs);
          setEditorFontSize(_ => fs);
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
        <label>
          <input
            disabled={is_running}
            type_="radio"
            name="preview"
            value="JavaScript"
            onClick={previewProgram(Some(JavaScript))}
          />
          <span> {React.string("JavaScript")} </span>
        </label>
        <label>
          <input
            disabled={is_running}
            type_="radio"
            name="preview"
            value="Python"
            onClick={previewProgram(Some(Python))}
          />
          <span> {React.string("Python")} </span>
        </label>
        <label>
          <input
            disabled={is_running}
            type_="radio"
            name="preview"
            value="Common"
            onClick={previewProgram(Some(Common))}
          />
          <span> {React.string("Common")} </span>
        </label>
        <label>
          <input
            disabled={is_running}
            type_="radio"
            name="preview"
            value="off"
            onClick={previewProgram(None)}
            checked={preview == None}
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
            setSyntax(_ => parseSyntax(newValue))
          }
          <select onChange disabled={is_running}>
            <option selected={None == syntax} value="auto">
              {React.string(`Auto (${runtime_syntax |> syntax_as_string})`)}
            </option>
            <option selected={Some(Render.Lispy) == syntax} value="Lispy">
              {React.string("Lispy")}
            </option>
            <option selected={Some(Render.JavaScript) == syntax} value="JavaScript">
              {React.string("JavaScript")}
            </option>
            <option selected={Some(Render.Python) == syntax} value="Python">
              {React.string("Python")}
            </option>
            <option selected={Some(Render.Common) == syntax} value="Common">
              {React.string("Common")}
            </option>
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
          checked={
            // Js.Console.log2("printing top-level?", printTopLevel)
            printTopLevel
          }
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
      <div ariaLabel="the code editor, press Esc then Tab to escape!" style={{
        fontSize: FontSize.toString(editorFontSize)
      }}>
        <CodeEditor
          syntax={if is_running {
            runtime_syntax
          } else {
            Lispy
          }}
          program={if is_running && runtime_syntax != Lispy {
            program->Parser.parseProgram->Render.adjust_syntax(runtime_syntax).string_of_program
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
                runtime_syntax->syntax_as_string,
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
          {switch preview {
          | None => <> </>
          | Some(sk) => make_preview(sk, program)
          }}
        </>
      | Some(s) => s.now
      }}
    </section>
  </main>
}
