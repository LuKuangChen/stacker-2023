open Belt
open SMoL

@module("./url_parameters") external syntaxAtURL: string = "syntaxAtURL"
@module("./url_parameters") external randomSeedAtURL: string = "randomSeedAtURL"
@module("./url_parameters") external nNextAtURL: int = "nNextAtURL"
@module("./url_parameters") external programAtURL: string = "programAtURL"
@module("./url_parameters") external make_url: (string, string, int, string) => string = "make_url"
@scope("window") @val external openPopUp: string => unit = "openPopUp"

exception Impossible

let parseSyntax = newValue =>
  switch newValue {
  | "Lisp" => Render.Lisp
  | "JavaScript" => JavaScript
  | "Python" => Python
  | _ => Lisp
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

type state =
  | Editing
  | Running(running_state)

type randomSeedConfig = {isSet: bool, randomSeed: string}

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
      "Lisp"
    } else {
      syntaxAtURL
    }
  })
  let (randomSeed: randomSeedConfig, setRandomSeed) = React.useState(_ => {
    if randomSeedAtURL == "" {
      {isSet: false, randomSeed: new_randomSeed()}
    } else {
      {isSet: true, randomSeed: randomSeedAtURL}
    }
  })
  let parseSMoL = (program: string) => {
    terms_of_string(program)
  }
  let loadProgram = program => {
    switch parseSMoL(program) {
    | exception ParseError(err) => {
        setParseFeedback(_ => stringOfParseError(err))
        Editing
      }

    | program => {
        let s: Runtime.state = Runtime.load(program, randomSeed.randomSeed)
        Running({
          prevs: list{},
          nexts: list{},
          now: Render.render(parseSyntax(syntax), s),
          latestState: s,
        })
      }
    }
  }
  let forward = s => {
    switch s {
    | Editing => raise(Impossible)
    | Running({prevs: _, now: _, nexts: list{}, latestState: Terminated(_)}) => raise(Impossible)
    | Running({prevs, now, nexts: list{}, latestState: Continuing(latestState)}) => {
        let latestState = Runtime.transition(latestState)
        Running({
          prevs: list{now, ...prevs},
          now: Render.render(parseSyntax(syntax), latestState),
          nexts: list{},
          latestState,
        })
      }

    | Running({prevs, now, nexts: list{e, ...nexts}, latestState}) =>
      Running({
        prevs: list{now, ...prevs},
        now: e,
        nexts,
        latestState,
      })
    }
  }
  let (state, setState) = React.useState(_ => {
    if programAtURL == "" {
      Editing
    } else {
      setProgram(_ => programAtURL)
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
    setState(_ => Editing)
  }
  let prevable = switch state {
  | Editing => false
  | Running({prevs, now: _, nexts: _, latestState: _}) =>
    switch prevs {
    | list{} => false
    | list{_e, ..._prevs} => true
    }
  }
  let onPrevClick = _evt => {
    setNNext(nNext => nNext - 1)
    setState(state =>
      switch state {
      | Editing => raise(Impossible)
      | Running({prevs, now, nexts, latestState}) =>
        switch prevs {
        | list{} => raise(Impossible)
        | list{e, ...prevs} => Running({prevs, now: e, nexts: list{now, ...nexts}, latestState})
        }
      }
    )
  }
  let onNextClick = _evt => {
    setNNext(nNext => nNext + 1)
    setState(forward)
  }
  let nextable = switch state {
  | Editing => false
  | Running({prevs: _, now: _, nexts: list{}, latestState: Terminated(_)}) => false
  | Running({prevs: _, now: _, nexts: list{}, latestState: Continuing(_)}) => true
  | Running({prevs: _, now: _, nexts: list{_e, ..._nexts}, latestState: _}) => true
  }
  let onShare = _evt => {
    openPopUp(make_url(syntax, randomSeed.randomSeed, nNext, program))
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
  let readOnly = !(state == Editing)
  <main onKeyDown>
    <section id="program-source">
      <details>
        <summary> {React.string("We provided some example programs.")} </summary>
        <menu ariaLabel="a list of example programs">
          <li>
            <button
              disabled={readOnly}
              value="Fibonacci"
              onClick={_evt => setProgram(_ => Programs.program_fib)}>
              {React.string("Fibonacci")}
            </button>
          </li>
          <li>
            <button
              disabled={readOnly}
              value="Scope"
              onClick={_evt => setProgram(_ => Programs.program_dynscope)}>
              {React.string("Scope")}
            </button>
          </li>
          <li>
            <button
              disabled={readOnly}
              value="Counter1"
              onClick={_evt => setProgram(_ => Programs.program_ctr1)}>
              {React.string("Counter1")}
            </button>
          </li>
          <li>
            <button
              disabled={readOnly}
              value="Counter2"
              onClick={_evt => setProgram(_ => Programs.program_ctr2)}>
              {React.string("Counter2")}
            </button>
          </li>
          <li>
            <button
              disabled={readOnly}
              value="Aliasing"
              onClick={_evt => setProgram(_ => Programs.program_aliasing)}>
              {React.string("Aliasing")}
            </button>
          </li>
          <li>
            <button
              disabled={readOnly}
              value="Object"
              onClick={_evt => setProgram(_ => Programs.program_object)}>
              {React.string("Object")}
            </button>
          </li>
        </menu>
      </details>
      {if readOnly {
        <p>
          <mark>
            <button onClick=onStopClick disabled={state == Editing}>
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
      <div ariaLabel="the code editor, press Esc then Tab to escape!">
        <CodeEditor
          syntax={if readOnly {
            parseSyntax(syntax)
          } else {
            Lisp
          }}
          program={if readOnly {
            program->terms_of_string->Render.adjust_syntax(parseSyntax(syntax)).string_of_program
          } else {
            program
          }}
          readOnly
          setProgram
        />
      </div>
      <details open_={syntaxAtURL != "" || randomSeedAtURL != ""}>
        <summary> {React.string("Advanced configurations.")} </summary>
        <label>
          {React.string("Syntax-flavor =")}
          {
            let onChange = evt => {
              let newValue: string = ReactEvent.Form.currentTarget(evt)["value"]
              setSyntax(_ => newValue)
            }
            <select onChange disabled={readOnly}>
              <option selected={"Lisp" == syntax} value="Lisp"> {React.string("Lisp-like")} </option>
              <option selected={"JavaScript" == syntax} value="JavaScript">
                {React.string("JavaScript-like")}
              </option>
              <option selected={"Python" == syntax} value="Python"> {React.string("Python-like")} </option>
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
              <input disabled={readOnly} type_="text" value={randomSeed.randomSeed} onChange />
            } else {
              <input
                disabled={readOnly} type_="text" placeholder={randomSeed.randomSeed} onChange
              />
            }
          }
        </label>
      </details>
    </section>
    <section id="stacker">
      <menu id="nav-trace" ariaLabel="toolbar">
        <li>
          <button onClick=onRunClick disabled={state != Editing}>
            <span ariaHidden={true}> {React.string("⏵ ")} </span>
            {React.string("Run")}
          </button>
        </li>
        <li>
          <button onClick=onStopClick disabled={state == Editing}>
            <span ariaHidden={true}> {React.string("⏹ ")} </span>
            {React.string("Stop")}
          </button>
        </li>
        <li>
          <button onClick=onPrevClick disabled={!prevable}>
            <span ariaHidden={true}> {React.string("⏮ ")} </span>
            {React.string("Previous")}
            // <kbd> {React.string("j")} </kbd>
          </button>
        </li>
        <li>
          <button onClick=onNextClick disabled={!nextable}>
            <span ariaHidden={true}> {React.string("⏭ ")} </span>
            {React.string("Next")}
            // <kbd> {React.string("k")} </kbd>
          </button>
        </li>
        <li>
          <button onClick=onShare disabled={state == Editing}>
            <span ariaHidden={true}> {React.string("🔗 ")} </span>
            {React.string("Share This Configuration")}
          </button>
        </li>
      </menu>
      {switch state {
      | Editing =>
        <p>
          {React.string("To start tracing, click ")}
          <button onClick=onRunClick disabled={state != Editing}>
            <span ariaHidden={true}> {React.string("⏵ ")} </span>
            {React.string("Run")}
          </button>
          {React.string(".")}
        </p>

      | Running(s) => s.now
      }}
    </section>
  </main>
}
