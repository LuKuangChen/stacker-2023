open Belt

type running_state = {
  prevs: list<React.element>,
  now: React.element,
  nexts: list<React.element>,
  latestState: Smol.state,
}

type state =
  | Editing
  | Running(running_state)

exception Impossible

@react.component
let make = () => {
  let (program, setProgram) = React.useState(_ => "")
  let (randomSeed, setRandomSeed) = React.useState(_ => None)
  let (state, setState) = React.useState(_ => Editing)
  let onRunClick = _evt => {
    let s: Smol.state = Smol.load(
      program->S_expr.stringToSource->S_expr.parse_many->Parse_smol.terms_of_sexprs,
      randomSeed -> Option.getWithDefault(Js.Math.random() -> Float.toString)
    )
    setState(_ => Running({
      prevs: list{},
      nexts: list{},
      now: Render.render(s),
      latestState: s,
    }))
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
    setState(s => {
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
    })
  }
  let nextable = switch state {
  | Editing => false
  | Running({prevs: _, now: _, nexts: list{}, latestState: Terminated(_)}) => false
  | Running({prevs: _, now: _, nexts: list{}, latestState: Continuing(_)}) => true
  | Running({prevs: _, now: _, nexts: list{_e, ..._nexts}, latestState: _}) => true
  }
  <main>
    <div id="control-panel">
      <button onClick=onRunClick disabled={state != Editing}> {React.string("Run")} </button>
      <button onClick=onStopClick disabled={state == Editing}> {React.string("Stop")} </button>
      <label>
        {React.string("Random Seed =")}
        <input
          type_="text"
          value={randomSeed->Option.getWithDefault("")}
          onChange={evt => {
            let newValue: string = ReactEvent.Form.currentTarget(evt)["value"]
            setRandomSeed(_ => Some(newValue))
          }}
        />
      </label>
      <button onClick=onPrevClick disabled={!prevable}> {React.string("Previous")} </button>
      <button onClick=onNextClick disabled={!nextable}> {React.string("Next")} </button>
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
