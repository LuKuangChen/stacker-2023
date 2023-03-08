type srcloc = {ln: int, ch: int}

type annotated<'t> = 't
// { data: 't, from: srcloc, to: srcloc }

type atom =
  | Str(string)
  | Sym(string)

type rec pre_sexpr =
  | Atom(atom)
  | List(list<sexpr>)
and sexpr = annotated<pre_sexpr>

type source = {srcloc: srcloc, i: int, content: string}

let stringToSource = s => {
  {srcloc: {ln: 0, ch: 0}, i: 0, content: s}
}

let advance = (srcloc, char) => {
  let {ln, ch} = srcloc
  switch char {
  | "\n" => {ln: ln + 1, ch: 0}
  | _ => {ln, ch: ch + 1}
  }
}

let case_source = source => {
  let {srcloc, i, content} = source
  if i < Js.String.length(content) {
    let ch = Js.String.get(content, i)
    let srcloc = advance(srcloc, ch)
    Some((ch, {srcloc, i: i + 1, content}))
  } else {
    None
  }
}

exception WantSExprFoundEOF
exception WantStringFoundEOF
exception WantEscapableCharFound(string)
exception WantSExprFoundRP(source)

let rec stringOfList = xs => {
  switch xs {
  | list{} => ""
  | list{x, ...xs} => x ++ stringOfList(xs)
  }
}

let parse_sym = (chr, src: source): (sexpr, source) => {
  let rec loop = (cs, src) => {
    let end = () => (Atom(Sym(stringOfList(Belt.List.reverse(cs)))), src)
    switch case_source(src) {
    | None => end()
    | Some(("(", _src)) => end()
    | Some((")", _src)) => end()
    | Some(("\"", _src)) => end()
    | Some((chr, src1)) =>
      if Js.Re.test_(%re("/\s+/ig"), chr) {
        end()
      } else {
        let src = src1
        loop(list{chr, ...cs}, src)
      }
    }
  }
  loop(list{chr}, src)
}

let parse_str = (src: source): (sexpr, source) => {
  let rec loop = (cs, src) => {
    switch case_source(src) {
    | None => raise(WantStringFoundEOF)
    | Some(("\"", src)) => (Atom(Sym(stringOfList(Belt.List.reverse(cs)))), src)
    | Some(("\\", src)) => escapting(cs, src)
    | Some((chr, src)) => loop(list{chr, ...cs}, src)
    }
  }
  and escapting = (cs, src) => {
    switch case_source(src) {
    | None => raise(WantStringFoundEOF)
    | Some(("\"", src)) => loop(list{"\"", ...cs}, src)
    | Some(("\\", src)) => loop(list{"\\", ...cs}, src)
    | Some(("t", src)) => loop(list{"\t", ...cs}, src)
    | Some(("n", src)) => loop(list{"\n", ...cs}, src)
    | Some((chr, src)) => raise(WantEscapableCharFound(chr))
    }
  }
  loop(list{}, src)
}

let rec parse_one = (src: source): (sexpr, source) => {
  switch case_source(src) {
  | None => raise(WantSExprFoundEOF)
  | Some(("(", src)) => start_parse_list(src.srcloc, src)
  | Some((")", src)) => raise(WantSExprFoundRP(src))
  | Some(("\"", src)) => parse_str(src)
  | Some((chr, src)) =>
    if Js.Re.test_(%re("/\s+/ig"), chr) {
      parse_one(src)
    } else {
      parse_sym(chr, src)
    }
  }
}
and start_parse_list = (start, src) => {
  let rec parse_list = (elms, src) => {
    switch parse_one(src) {
    | (elm, src) => parse_list(list{elm, ...elms}, src)
    | exception WantSExprFoundRP(src) => (List(Belt.List.reverse(elms)), src)
    }
  }
  parse_list(list{}, src)
}

let parse_many = (src: source) => {
  let rec loop = (elms, src) => {
    switch parse_one(src) {
    | (elm, src) => loop(list{elm, ...elms}, src)
    | exception WantSExprFoundEOF => Belt.List.reverse(elms)
    }
  }
  loop(list{}, src)
}

let rec stringOfSexpr = e =>
  switch e {
  | Atom(Sym(s)) => s
  | Atom(Str(s)) => "str:" ++ s
  | List(list{}) => "()"
  | List(list{x, ...xs}) => {
      let stringOfXs = stringOfList(Belt.List.map(xs, x => " " ++ stringOfSexpr(x)))
      "(" ++ stringOfSexpr(x) ++ stringOfXs ++ ")"
    }
  }

let rec stringOfManySexprs = es => {
  List.map(e => {stringOfSexpr(e) ++ "\n"}, es) |> stringOfList
}
