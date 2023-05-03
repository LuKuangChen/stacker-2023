type dec<'a, 'b> =
  | Yes('a)
  | No('b)

type srcloc = {ln: int, ch: int}
type srcrange = {begin: srcloc, end: srcloc}
type annotated<'t> = {it: 't, ann: srcrange}
let annotate = (it, begin, end) => {
  {it, ann: {begin, end}}
}
let unann = (x: annotated<'t>) => x.it
let dummyAnn: 'a => annotated<'a> = x => {
  let loc = {ln: 0, ch: 0}
  annotate(x, loc, loc)
}

let text = s => React.string(s)

let indent = (s, i) => {
  let pad = Js.String.repeat(i, " ")
  Js.String.replaceByRe(%re("/\n/g"), "\n" ++ pad, s)
}

exception Impossible
