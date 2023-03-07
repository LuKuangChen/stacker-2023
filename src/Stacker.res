@react.component
let make = (~program) => {
  let output = switch S_expr.parse_one(program |> S_expr.stringToSource) {
  | (parsed, _src) =>
    S_expr.stringOfSexpr(parsed)
  | exception S_expr.WantSExprFoundEOF =>
    "Unexpected EOF while looking for an S-expression."
  | exception S_expr.WantStringFoundEOF =>
    "Unexpected EOF while processing a string."
  | exception S_expr.WantEscapableCharFound(string) =>
    `This character (${string}) can't be escaped.`
  | exception S_expr.WantSExprFoundRP(_source) =>
    "Unexpected right parenthesis."
  }
  <pre> {React.string(output)} </pre>
}
