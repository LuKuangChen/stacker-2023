@react.component
let make = (~program, ~setProgram) => {
  let onChange = evt => {
    ReactEvent.Form.preventDefault(evt)
    setProgram(_ => ReactEvent.Form.target(evt)["value"])
  }
  <textarea value=program onChange />
}
