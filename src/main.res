type state =
  | Editing
  | Running

@react.component
let make = () => {
  let (program, setProgram) = React.useState(_ => "");
  let (state, setState) = React.useState(_ => Editing);
  <div>
    <div id="program-source">
      <CodeEditor program setProgram editable = (state == Editing) />
    </div>
    <div id="stacker"><Stacker program /></div>
  </div>
}
