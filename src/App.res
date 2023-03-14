open Belt

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
        setNNext(nNext => (nNext + 1))
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
      let s = ref(loadProgram(programAtURL))
      for _ in 1 to nNextAtURL {
        s.contents = forward(s.contents)
      }
      s.contents
    }
  })
  let onRunClick = _evt => {
    setState(_ => loadProgram(program))
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
  <main>
    <div id="control-panel">
      <button onClick=onRunClick disabled={state != Editing}> {React.string("Run")} </button>
      <button onClick=onStopClick disabled={state == Editing}> {React.string("Stop")} </button>
      <label>
        {React.string("Random Seed =")}
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
      <button onClick=onPrevClick disabled={!prevable}> {React.string("Previous")} </button>
      <button onClick=onNextClick disabled={!nextable}> {React.string("Next")} </button>
      <button onClick=onShare> {React.string(`Create a sharable link`)} </button>
    </div>
    <div id="row">
      <section id="program-source">
        <CodeEditor program setProgram />
      </section>
      <section id="stacker">
        {switch state {
        | Editing => React.string("Click run to start tracing")
        | Running(s) => s.now
        }}
      </section>
    </div>
  </main>
}
