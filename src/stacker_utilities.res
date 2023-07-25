type dec<'a, 'b> =
  | Yes('a)
  | No('b)

let text = s => React.string(s)

let indent = (s, i) => {
  let pad = Js.String.repeat(i, " ")
  Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s)
}

exception Impossible
