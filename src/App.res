open Belt
open Utilities

@module("./url_parameters") external randomSeedAtURL: string = "randomSeedAtURL"
@module("./url_parameters") external nNextAtURL: int = "nNextAtURL"
@module("./url_parameters") external programAtURL: string = "programAtURL"
@module("./url_parameters") external shareLink: (string, int, string) => unit = "shareLink"

type running_state = {
  prevs: list<React.element>,
  now: React.element,
  nexts: list<React.element>,
  latestState: Smol.state,
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

exception Impossible

type randomSeedConfig = {isSet: bool, randomSeed: string}

@react.component
let make = () => {
  let (program, setProgram) = React.useState(_ => "")
  let (nNext, setNNext) = React.useState(_ => 0)
  let (randomSeed: randomSeedConfig, setRandomSeed) = React.useState(_ => {
    Js.log2("randomSeedAtURL", randomSeedAtURL)
    if randomSeedAtURL == "" {
      {isSet: false, randomSeed: new_randomSeed()}
    } else {
      {isSet: true, randomSeed: randomSeedAtURL}
    }
  })
  let loadProgram = program => {
    let s: Smol.state = Smol.load(
      program->S_expr.stringToSource->S_expr.parse_many->Parse_smol.terms_of_sexprs,
      randomSeed.randomSeed,
    )
    Running({
      prevs: list{},
      nexts: list{},
      now: Render.render(s),
      latestState: s,
    })
  }
  let forward = s => {
    switch s {
    | Editing => raise(Impossible)
    | Running({prevs: _, now: _, nexts: list{}, latestState: Terminated(_)}) => raise(Impossible)
    | Running({prevs, now, nexts: list{}, latestState: Continuing(latestState)}) => {
        let latestState = Smol.transition(latestState)
        Running({
          prevs: list{now, ...prevs},
          now: Render.render(latestState),
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
    shareLink(randomSeed.randomSeed, nNext, program)
  }
  let onKeyPress = evt => {
    let key = ReactEvent.Keyboard.key(evt)
    if key == "j" && prevable {
      onPrevClick(evt)
    } else if key == "k" && nextable {
      onNextClick(evt)
    }
  }
  <main>
    <section id="program-source">
      <details>
        <summary> {text("We provided some example programs.")} </summary>
        <menu ariaLabel="a list of example programs">
          <li>
            <button value="Fibonacci" onClick={_evt => setProgram(_ => Programs.program_fib)}>
              {text("Fibonacci")}
            </button>
          </li>
          <li>
            <button value="Scope" onClick={_evt => setProgram(_ => Programs.program_dynscope)}>
              {text("Scope")}
            </button>
          </li>
          <li>
            <button value="Counter" onClick={_evt => setProgram(_ => Programs.program_ctr)}>
              {text("Counter")}
            </button>
          </li>
          <li>
            <button value="Aliasing" onClick={_evt => setProgram(_ => Programs.program_aliasing)}>
              {text("Aliasing")}
            </button>
          </li>
        </menu>
      </details>
      <div ariaLabel="the code editor, press Esc then Tab to escape!">
        <CodeEditor program setProgram />
      </div>
      <details>
        <summary> {text("Advanced configurations.")} </summary>
        <label>
          {React.string("Random Seed = ")}
          {
            let onChange = evt => {
              let newValue: string = ReactEvent.Form.currentTarget(evt)["value"]
              setRandomSeed(_ => {isSet: true, randomSeed: newValue})
            }
            if randomSeed.isSet {
              <input type_="text" value={randomSeed.randomSeed} onChange />
            } else {
              <input type_="text" placeholder={randomSeed.randomSeed} onChange />
            }
          }
        </label>
      </details>
    </section>
    <section id="stacker" onKeyPress>
      <menu id="nav-trace" ariaLabel="toolbar">
        <li>
          <button onClick=onRunClick disabled={state != Editing}>
            <span ariaHidden={true}> {text("⏵ ")} </span>
            {text("Run")}
          </button>
        </li>
        <li>
          <button onClick=onStopClick disabled={state == Editing}>
            <span ariaHidden={true}> {text("⏹ ")} </span>
            {React.string("Stop")}
          </button>
        </li>
        <li>
          <button onClick=onPrevClick disabled={!prevable}>
            <span ariaHidden={true}> {text("⏮ ")} </span>
            {React.string("Previous")}
          </button>
        </li>
        <li>
          <button onClick=onNextClick disabled={!nextable}>
            <span ariaHidden={true}> {text("⏭ ")} </span>
            {React.string("Next")}
          </button>
        </li>
        <li>
          <button onClick=onShare disabled={state == Editing}>
            <span ariaHidden={true}> {text("🔗 ")} </span>
            {React.string("Share This Configuration")}
          </button>
        </li>
      </menu>
      {switch state {
      | Editing =>
        <p>
          {text("To start tracing, click ")}
          <button onClick=onRunClick disabled={state != Editing}>
            <span ariaHidden={true}> {text("⏵ ")} </span>
            {React.string("Run")}
          </button>
          {text(".")}
        </p>

      | Running(s) => s.now
      }}
    </section>
  </main>
}
