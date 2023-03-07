/*

This file convert smol states to react elements.

*/

open Smol
open Belt
open List

let string_of_constant = c => {
    switch c {
    | Num(n) => Float.toString(n)
    | Lgc(l) => if l { "#t" } else { "#f" }
    | Str(s) => "\"" ++ String.escaped(s) ++ "\""
    }
}

let string_of_function = f => {
    "#<procedure>"
}

let string_of_value = v => {
    switch v {
    | Uni => "#<void>"
    | Con(c) => string_of_constant(c)
    | Fun(f) => string_of_function(f)
    }
}

let render_error = err => {
  switch err {
  | UnboundIdentifier(symbol) => `The variable ${symbol} is not bound.`
  | UsedBeforeInitialization(symbol) => `The variable ${symbol} hasn't be declared.`
  | ExpectButGiven(string, _value) => `Expecting a ${string}.`
  | ArityMismatch(_arity, int) => `Expecting a function that accept ${Int.toString(int)} arguments`
  }
}

let render: state => React.element = s => {
  switch s {
  | Terminated(Err(err)) => React.string(render_error(err))
  | Terminated(Tm(vs)) => React.string("terminated\n" ++ String.concat("\n", vs -> map(string_of_value)))
  | Continuing(Ev(_e, _about)) => {
      Js.log(_e)
      Js.log(_about)
      React.string("evaluating")
  }
  | Continuing(Setting(_x, _v, _about)) => React.string("replacing")
  | Continuing(Returning(_v, _about)) => React.string("returning")
  }
}
