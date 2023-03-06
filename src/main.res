@react.component
let make = () => {
  let (program, setProgram) = React.useState(_ => "");
  <div>
    <div id="program-source"> {React.string("Hello World")} </div>
    <div id="stacker"> {React.string("Hello World")} </div>
  </div>
}
