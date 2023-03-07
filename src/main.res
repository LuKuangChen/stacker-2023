@react.component
let make = () => {
  let (program, setProgram) = React.useState(_ => "");
  <div>
    <div id="program-source"><CodeEditor program setProgram /></div>
    <div id="stacker"><Stacker program /></div>
  </div>
}
