open Belt
open SMoL

@module("./url_parameters") external syntaxAtURL: string = "syntaxAtURL"
@module("./url_parameters") external randomSeedAtURL: string = "randomSeedAtURL"
@module("./url_parameters") external nNextAtURL: int = "nNextAtURL"
@module("./url_parameters") external programAtURL: string = "programAtURL"
@module("./url_parameters") external readOnlyMode: bool = "readOnlyMode"
@module("./url_parameters")
external make_url: (string, string, int, string, bool) => string = "make_url"
@scope("window") @val external openPopUp: string => unit = "openPopUp"

exception Impossible

let parseSyntax = newValue =>
  switch newValue {
  | "SMoL" => Some(Render.Lisp)
  | "JavaScript" => Some(JavaScript)
  | "Python" => Some(Python)
  | _ => None
  }
let syntax_as_string = sk =>
  switch sk {
  | Render.Lisp => "SMoL"
  | JavaScript => "JavaScript"
  | Python => "Python"
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
  switch program->terms_of_string {
  | program =>
    <>
      <span>
        {React.string(`(Showing the `)}
        <u> {React.string(sk |> syntax_as_string)} </u>
        {React.string(` translation)`)}
      </span>
      <CodeEditor
        syntax={sk}
        program={program->Render.adjust_syntax(sk).string_of_program}
        readOnly={true}
        setProgram={_ => ()}
      />
    </>
  | exception SMoL.ParseError(err) => {
      let parseFeedback = stringOfParseError(err)
      <span className="parse-feedback"> {React.string(parseFeedback)} </span>
    }
  }
}

let parseSMoL = (program: string) => {
  terms_of_string(program)
}

@react.component
let make = () => {
  let (program, rawSetProgram) = React.useState(_ => "")
  let (parseFeedback, setParseFeedback) = React.useState(_ => "")
  let setProgram = (setter: string => string) => {
    setParseFeedback(_ => "")
    rawSetProgram(setter)
  }
  let (nNext, setNNext) = React.useState(_ => 0)
  let (syntax, setSyntax) = React.useState(_ => {
    if syntaxAtURL == "" {
      None
    } else {
      parseSyntax(syntaxAtURL)
    }
  })
  let (randomSeed: randomSeedConfig, setRandomSeed) = React.useState(_ => {
    if randomSeedAtURL == "" {
      {isSet: false, randomSeed: new_randomSeed()}
    } else {
      {isSet: true, randomSeed: randomSeedAtURL}
    }
  })
  let (preview: option<Render.syntax_kind>, setPreview) = React.useState(_ => None)
  let runtime_syntax = Option.orElse(syntax, preview)->Option.getWithDefault(Render.Lisp)
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
    | exception SMoL.ParseError(err) => {
        setParseFeedback(_ => stringOfParseError(err))
        None
      }

    | program => {
        let s: Runtime.state = Runtime.load(program, randomSeed.randomSeed)
        Some({
          prevs: list{},
          nexts: list{},
          now: Render.render(runtime_syntax, s),
          latestState: s,
        })
      }
    }
  }
  let (state, setState) = React.useState(_ => {
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
      <span ariaHidden={true}> {React.string("⏵ ")} </span>
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
  let exampleProgramsAndStopButtonShortcut = if readOnlyMode {
    <> </>
  } else {
    <>
      <details>
        <summary> {React.string("Example programs:")} </summary>
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
      <summary> {React.string("Advanced configuration:")} </summary>
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
            <option selected={Some(Render.Lisp) == syntax} value="SMoL">
              {React.string("SMoL")}
            </option>
            <option selected={Some(Render.JavaScript) == syntax} value="JavaScript">
              {React.string("JavaScript")}
            </option>
            <option selected={Some(Render.Python) == syntax} value="Python">
              {React.string("Python")}
            </option>
          </select>
        }
      </label>
      <br />
      <label>
        {React.string("Random Seed = ")}
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
    </details>
  }
  <main onKeyDown>
    <section id="program-source">
      {exampleProgramsAndStopButtonShortcut}
      <div ariaLabel="the code editor, press Esc then Tab to escape!">
        <CodeEditor
          syntax={if is_running {
            runtime_syntax
          } else {
            Lisp
          }}
          program={if (is_running && runtime_syntax != Lisp) {
            program->terms_of_string->Render.adjust_syntax(runtime_syntax).string_of_program
          } else {
            program
          }}
          readOnly={is_running}
          setProgram
        />
      </div>
    </section>
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
              <span ariaHidden={true}> {React.string("⏵ ")} </span>
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
