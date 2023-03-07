@react.component
let make = (~program, ~setProgram) => {
  let output = switch S_expr.parse_many(program |> S_expr.stringToSource) {
  | parsed => S_expr.stringOfManySexprs(parsed)
  | exception S_expr.WantSExprFoundEOF => "Unexpected EOF while looking for an S-expression."
  | exception S_expr.WantStringFoundEOF => "Unexpected EOF while processing a string."
  | exception S_expr.WantEscapableCharFound(string) =>
    `This character (${string}) can't be escaped.`
  | exception S_expr.WantSExprFoundRP(_source) => "Unexpected right parenthesis."
  }
  let onChange = evt => {
    ReactEvent.Form.preventDefault(evt)
    setProgram(_ => ReactEvent.Form.target(evt)["value"])
  }
  <div>
    <textarea value=program onChange />
    <p> {React.string(output)} </p>
  </div>
}
