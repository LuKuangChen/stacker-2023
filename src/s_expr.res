open Utilities

/*
Okay, I find this file gets really confusing. I am using exceptions for two purposes:

1. to backtrack
2. to raise real parsing error

Let's leave the first kind of exceptions as their own kind, and group all second kind
exception in one data type.
*/

type atom =
  | Str(string)
  | Sym(string)

type bracket =
  | Round
  | Square
let stringOfBracket = bracket => {
  switch bracket {
  | Square => "square"
  | Round => "round"
  }
}

type rec sexpr =
  | Atom(atom)
  | List(bracket, list<annotated<sexpr>>)

type source = {srcloc: srcloc, i: int, content: string}

let stringToSource = s => {
  {srcloc: {ln: 0, ch: 0}, i: 0, content: s}
}

let advance = (srcloc, char) => {
  let {ln, ch} = srcloc
  if char === "\n" {
    {ln: ln + 1, ch: 0}
  } else {
    {ln, ch: ch + 1}
  }
}

let case_source = (source): option<(string, source)> => {
  let {srcloc, i, content} = source
  if i < Js.String.length(content) {
    let ch = Js.String.get(content, i)
    let srcloc = advance(srcloc, ch)
    Some((ch, {srcloc, i: i + 1, content}))
  } else {
    None
  }
}

type parseError =
  | WantListFoundEOF
  | WantStringFoundEOF
  | WantEscapableCharFound(string)
  | MismatchedBracket(bracket, bracket)
exception ParseError(parseError)

let stringOfParseError: parseError => string = err => {
  switch err {
  | WantListFoundEOF => "Reached the end of the file while processing a list."
  | WantStringFoundEOF => "Reached the end of the file while processing a string."
  | WantEscapableCharFound(string) => `Found an unexpected escape sequence (\\${string}).`
  | MismatchedBracket(start, end) =>
    `Found a closing ${stringOfBracket(end)} bracket but this list starts with a ${stringOfBracket(
        start,
      )} bracket.`
  }
}

let rec stringOfList = xs => {
  switch xs {
  | list{} => ""
  | list{x, ...xs} => x ++ stringOfList(xs)
  }
}

let parse_sym = (start, first_chr, src: source): (annotated<sexpr>, source) => {
  let rec loop = (cs, src: source): (annotated<sexpr>, source) => {
    let end = () => {
      let e = Atom(Sym(stringOfList(Belt.List.reverse(cs))))
      (annotate(e, start, src.srcloc), src)
    }
    switch case_source(src) {
    | None => end()
    | Some(("(", _src)) => end()
    | Some((")", _src)) => end()
    | Some(("[", _src)) => end()
    | Some(("]", _src)) => end()
    | Some((`"`, _src)) => end()
    | Some((chr, src1)) =>
      if Js.Re.test_(%re("/\s+/ig"), chr) {
        end()
      } else {
        let src = src1
        loop(list{chr, ...cs}, src)
      }
    }
  }
  loop(list{first_chr}, src)
}

let parse_str = (start: srcloc, src: source): (annotated<sexpr>, source) => {
  let rec loop = (cs, src): (annotated<sexpr>, source) => {
    switch case_source(src) {
    | None => raise(ParseError(WantStringFoundEOF))
    | Some((`"`, src)) => {
        let e = Atom(Str(stringOfList(Belt.List.reverse(cs))))
        (annotate(e, start, src.srcloc), src)
      }

    | Some((chr, src)) =>
      if chr == "\\" {
        escapting(cs, src)
      } else {
        loop(list{chr, ...cs}, src)
      }
    }
  }
  and escapting = (cs, src): (annotated<sexpr>, source) => {
    switch case_source(src) {
    | None => raise(ParseError(WantStringFoundEOF))
    | Some((chr, src)) =>
      switch chr {
      | `"` => loop(list{`"`, ...cs}, src)
      | "r" => loop(list{"\r", ...cs}, src)
      | "t" => loop(list{"\t", ...cs}, src)
      | "n" => loop(list{"\n", ...cs}, src)
      | chr =>
        if chr == "\\" {
          loop(list{"\\", ...cs}, src)
        } else {
          raise(ParseError(WantEscapableCharFound(chr)))
        }
      }
    }
  }
  loop(list{}, src)
}

exception EOF
exception WantSExprFoundRP(bracket, source)
let rec parse_one = (src: source): (annotated<sexpr>, source) => {
  let start = src.srcloc
  switch case_source(src) {
  | None => raise(EOF)
  | Some(("(", src)) => start_parse_list(Round, start, src)
  | Some(("[", src)) => start_parse_list(Square, start, src)
  | Some((")", src)) => raise(WantSExprFoundRP(Round, src))
  | Some(("]", src)) => raise(WantSExprFoundRP(Square, src))
  | Some((`"`, src)) => parse_str(start, src)
  | Some((chr, src)) =>
    // Js.log(`This one character is: "${chr}".`)
    if Js.Re.test_(%re("/\s+/ig"), chr) {
      parse_one(src)
    } else {
      parse_sym(start, chr, src)
    }
  }
}
and start_parse_list = (bracket1, start, src): (annotated<sexpr>, source) => {
  let rec parse_list = (elms, src): (annotated<sexpr>, source) => {
    switch parse_one(src) {
    | (elm, src) => parse_list(list{elm, ...elms}, src)
    | exception EOF => raise(ParseError(WantListFoundEOF))
    | exception WantSExprFoundRP(bracket2, src) =>
      if bracket1 == bracket2 {
        let e = List(bracket1, Belt.List.reverse(elms))
        (annotate(e, start, src.srcloc), src)
      } else {
        raise(ParseError(MismatchedBracket(bracket1, bracket2)))
      }
    }
  }
  parse_list(list{}, src)
}

let parse_many = (src: source) => {
  let rec loop = (elms, src) => {
    switch parse_one(src) {
    | (elm, src) => loop(list{elm, ...elms}, src)
    | exception EOF => Belt.List.reverse(elms)
    }
  }
  loop(list{}, src)
}

let rec stringOfSexpr = (e: annotated<sexpr>) =>
  switch e.it {
  | Atom(Sym(s)) => s
  | Atom(Str(s)) => "str:" ++ s
  | List(_b, list{}) => "()"
  | List(_b, list{x, ...xs}) => {
      let stringOfXs = stringOfList(Belt.List.map(xs, x => " " ++ stringOfSexpr(x)))
      "(" ++ stringOfSexpr(x) ++ stringOfXs ++ ")"
    }
  }
// let rec stringOfManySexprs = es => {
//   List.map(e => {stringOfSexpr(e) ++ "\n"}, es) |> stringOfList
// }
