type srcloc = {ln: int, ch: int}
type srcrange = {begin: srcloc, end: srcloc}
type annotated<'t> = {it: 't, ann: srcrange}
let annotate = (it, begin, end) => {
  {it, ann: {begin, end}}
}
let unann = (x: annotated<'t>) => x.it

let text = (s) => React.string(s)