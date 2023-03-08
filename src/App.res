type state =
  | Editing
  | Running(Smol.state)

exception Impossible

@react.component
let make = () => {
  let (program, setProgram) = React.useState(_ => "")
  let (state, setState) = React.useState(_ => Editing)
  let onRunClick = _evt => {
    setState(_ => Running(
      Smol.load(program->S_expr.stringToSource->S_expr.parse_many->Parse_smol.terms_of_sexprs),
    ))
  }
  let onStopClick = _evt => {
    setState(_ => Editing)
  }
  let onPrevClick = _evt => {
    ()
  }
  let onNextClick = _evt => {
    setState(s => {
      switch s {
      | Editing => raise(Impossible)
      | Running(Terminated(_s)) => raise(Impossible)
      | Running(Continuing(s)) => Running(Smol.transition(s))
      }
    })
  }
  let runningButNotTerminated =
    switch state {
    | Running(Continuing(_)) => false
    | _ => true
    }
  <div id="main">
    <div id="control-panel">
      <button onClick=onRunClick disabled={state != Editing}> {React.string("Run")} </button>
      <button onClick=onStopClick disabled={state == Editing}> {React.string("Stop")} </button>
      <button onClick=onPrevClick disabled={state == Editing}> {React.string("Prev")} </button>
      <button onClick=onNextClick disabled={runningButNotTerminated}> {React.string("Next")} </button>
    </div>
    <div id="row">
      <div id="program-source">
        <CodeEditor program setProgram editable={state == Editing} />
      </div>
      <div id="stacker">
        {switch state {
        | Editing => React.string("Click run to start tracing")
        | Running(s) => Render.render(s)
        }}
      </div>
    </div>
  </div>
}
