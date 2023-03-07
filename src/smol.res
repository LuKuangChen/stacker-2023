// type definition = Defvar(string, expression)

type symbol = string
type constant =
  | Num(float)
  | Lgc(bool)
  | Str(string)
type rec expression =
  | Con(constant)
  | Ref(symbol)
  | Set(symbol, expression)
  | Lam(list<symbol>, block)
  | App(expression, list<expression>)
  | Cnd(list<(expression, block)>, option<block>)
and block = (list<term>, expression)
and definition =
  | Var(symbol, expression)
  | Fun(symbol, list<symbol>, block)
and term =
  | Def(definition)
  | Exp(expression)
type program = list<term>

let xsOfBlock = (b: block) => {
  let (ts, _e) = b
  open Js.List
  ts
  |> map((. trm) =>
    switch trm {
    | Def(Var(x, _e)) => list{x}
    | Def(Fun(x, _ys, _b)) => list{x}
    | Exp(_e) => list{}
    }
  )
  |> flatten
  |> toVector
}

type primitive =
  | Add
  | Sub
  | Mul
  | Div
  | Lt
  | Eq
  | Gt
  | Le
  | Ge
  | Ne

type rec environmentFrame = array<(symbol, ref<option<value>>)>
and environment = list<environmentFrame>
and function =
  // primitives
  | Prm(primitive)
  // User-Defined Functions
  | Udf(array<symbol>, block, environment)
and value =
  // the Unit value
  | Uni
  // Constants
  | Con(constant)
  // Functions
  | Fun(function)

let initialEnv: environment = list{
  [
    ("+", ref(Some(Fun(Prm(Add))))),
    ("-", ref(Some(Fun(Prm(Sub))))),
    ("*", ref(Some(Fun(Prm(Mul))))),
    ("/", ref(Some(Fun(Prm(Div))))),
    ("<", ref(Some(Fun(Prm(Lt))))),
    ("=", ref(Some(Fun(Prm(Eq))))),
    (">", ref(Some(Fun(Prm(Gt))))),
    ("<=", ref(Some(Fun(Prm(Le))))),
    (">=", ref(Some(Fun(Prm(Ge))))),
    ("!=", ref(Some(Fun(Prm(Ne))))),
  ],
}

type result =
  | Err
  | Val(value)

let stdout = ref(list{})
let print = v => {
  stdout.contents = list{Val(v), ...stdout.contents}
}

type arity =
  | AtLeast(int)
  | Exactly(int)

type runtime_error =
  | UnboundIdentifier(symbol)
  | UsedBeforeInitialization(symbol)
  | ExpectButGiven(string, value)
  | ArityMismatch(arity, int)
exception RuntimeError(runtime_error)

let extend = (env, xs): environment => list{xs |> Js.Array.map(x => (x, ref(None))), ...env}

let ifNone = (thunk : () => 'X, o : option<'X>): 'X => {
    switch o {
    | None => thunk()
    | Some(x) => x
    }
}

let rec lookup = (env, x) => {
  switch env {
  | list{} => raise(RuntimeError(UnboundIdentifier(x)))
  | list{bs, ...env} =>
    bs
    |> Js.Array.find(((y, _v)) => x == y)
    |> Js.Option.map((. (_x, v)) => v)
    |> ifNone(() => lookup(env, x))
  }
}

let doRef = (env, x) => {
  switch lookup(env, x).contents {
  | None => raise(RuntimeError(UsedBeforeInitialization(x)))
  | Some(v) => v
  }
}
let doSet = (env, x, v) => {
  lookup(env, x).contents = Some(v)
}

type contextFrame =
  | Set1(symbol, unit)
  | App1(unit, list<expression>)
  | App2(value, list<value>, unit, list<expression>)
  | Cnd1(unit, block, list<(expression, block)>, option<block>)
  | BgnDef(symbol, unit, block)
  | BgnExp(unit, block)
  | PrgDef(list<value>, symbol, unit, list<term>)
  | PrgExp(list<value>, unit, list<term>)
type context = list<contextFrame>

type stackFrame = (context, environment)
type stack = list<stackFrame>

type commomState = {ctx: context, env: environment, stk: stack}

type dec<'a, 'b> =
  | Yes('a)
  | No('b)

let consCtx = (ctxFrame, stt) => {
  ...stt,
  ctx: list{ctxFrame, ...stt.ctx},
}
let caseCtx = stt => {
  switch stt.ctx {
  | list{} => No(stt.stk)
  | list{ctxFrame, ...ctx} => Yes(ctxFrame, {...stt, ctx})
  }
}

type terminated_state =
  | Err(runtime_error)
  | Tm(list<value>)
type continuing_state =
  | Ev(expression, commomState)
  | Setting(symbol, value, commomState)
  | Returning(value, stack)
type state =
  | Terminated(terminated_state)
  | Continuing(continuing_state)

let evaluate = (exp: expression) => {
  let ctx = list{}
  and env = list{}
  and stk = list{}
  Ev(exp, {ctx, env, stk})
}

let asNum = v =>
  switch v {
  | Con(Num(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("number", v)))
  }
let asLgc = v =>
  switch v {
  | Con(Lgc(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("boolean", v)))
  }
and asFun = v =>
  switch v {
  | Fun(f) => f
  | _else => raise(RuntimeError(ExpectButGiven("function", v)))
  }

let doAdd = (u, v) => Con(Num(asNum(u) +. asNum(v)))

let transitionBgn = ((ts, e), stt) => {
  switch ts {
  | list{} =>
    let exp = e
    Continuing(Ev(exp, stt))
  | list{Def(Var(x, e0)), ...ts} =>
    let exp = e0
    Continuing(Ev(exp, consCtx(BgnDef(x, (), (ts, e)), stt)))
  | list{Def(Fun(f, xs, b)), ...ts} =>
    let exp = Lam(xs, b)
    Continuing(Ev(exp, consCtx(BgnDef(f, (), (ts, e)), stt)))
  | list{Exp(e0), ...ts} =>
    let exp = e0
    Continuing(Ev(exp, consCtx(BgnExp((), (ts, e)), stt)))
  }
}

let transitionPrg = (vs, ts, stt) => {
  switch ts {
  | list{} => Terminated(Tm(vs))
  | list{Def(Var(x, e0)), ...ts} =>
    let exp = e0
    Continuing(Ev(exp, consCtx(PrgDef(vs, x, (), ts), stt)))
  | list{Def(Fun(f, xs, b)), ...ts} =>
    let exp = Lam(xs, b)
    Continuing(Ev(exp, consCtx(PrgDef(vs, f, (), ts), stt)))
  | list{Exp(e0), ...ts} =>
    let exp = e0
    Continuing(Ev(exp, consCtx(PrgExp(vs, (), ts), stt)))
  }
}

let load = (program: program) => {
  transitionPrg(list{}, program, {ctx: list{}, env: initialEnv, stk: list{}})
}

let pushStk = (env, stt) => {
  {ctx: list{}, env, stk: list{(stt.ctx, stt.env), ...stt.stk}}
}

let doBlk = (b, stt): state => {
  transitionBgn(b, pushStk(extend(stt.env, xsOfBlock(b)), stt))
}

let deltaNum1 = (f, v, vs) => {
  open Js.List
  let v = asNum(v)
  and vs = map((. v) => asNum(v), vs)
  Con(Num(Js.List.foldLeft(f, v, vs)))
}
let deltaNum2 = (f, v1, v2, vs) => {
  open Js.List
  let v1 = asNum(v1)
  and v2 = asNum(v2)
  and vs = map((. v) => asNum(v), vs)
  Con(Num(Js.List.foldLeft(f, f(. v1, v2), vs)))
}

type acc = {
  v1: float,
  vvs: (float, float),
}
let deltaCmp = (cmp, v, vs): value => {
  open Js.List
  let v = asNum(v)
  and vs = map((. v) => asNum(v), vs)
  let rec loop = (v1, vs) => {
    switch vs {
    | list{} => true
    | list{v2, ...vs} => cmp(v1, v2) && loop(v2, vs)
    }
  }
  Con(Lgc(loop(v, vs)))
}

let arityOf = p =>
  switch p {
  | Add => AtLeast(1)
  | Sub => AtLeast(2)
  | Mul => AtLeast(1)
  | Div => AtLeast(2)
  | Lt => AtLeast(2)
  | Eq => AtLeast(2)
  | Gt => AtLeast(2)
  | Le => AtLeast(2)
  | Ge => AtLeast(2)
  | Ne => AtLeast(2)
  }

let delta = (p, vs) =>
  switch (p, vs) {
  | (Add, list{v, ...vs}) => deltaNum1((. a, b) => a +. b, v, vs)
  | (Sub, list{v1, v2, ...vs}) => deltaNum2((. a, b) => a -. b, v1, v2, vs)
  | (Mul, list{v, ...vs}) => deltaNum1((. a, b) => a *. b, v, vs)
  | (Div, list{v1, v2, ...vs}) => deltaNum2((. a, b) => a /. b, v1, v2, vs)
  | (Lt, list{v, ...vs}) => deltaCmp((a, b) => a < b, v, vs)
  | (Eq, list{v, ...vs}) => deltaCmp((a, b) => a == b, v, vs)
  | (Gt, list{v, ...vs}) => deltaCmp((a, b) => a > b, v, vs)
  | (Le, list{v, ...vs}) => deltaCmp((a, b) => a <= b, v, vs)
  | (Ge, list{v, ...vs}) => deltaCmp((a, b) => a >= b, v, vs)
  | (Ne, list{v, ...vs}) => deltaCmp((a, b) => a != b, v, vs)
  | _otherwise => raise(RuntimeError(ArityMismatch(arityOf(p), List.length(vs))))
  }

exception Impossible(string)
let rec return = (v: value, stk): state => {
  switch stk {
  | list{} => raise(Impossible("Should have been handled by the top-level"))
  | list{(ctx, env), ...stk} => continue(v, {ctx, env, stk})
  }
}
and setting = (x, v, stt) => {
  doSet(stt.env, x, v)
  continue(Uni, stt)
}
and continue = (v: value, stt): state => {
  switch caseCtx(stt) {
  | No(stk) => Continuing(Returning(v, stk))
  | Yes(ctxFrame, stt) =>
    switch ctxFrame {
    | Set1(x, ()) => Continuing(Setting(x, v, stt))
    | App1((), exps) =>
      let fun = v
      and vals = list{}
      transitionApp(fun, vals, exps, stt)
    | App2(fun, vals, (), exps) =>
      let vals = list{v, ...vals}
      transitionApp(fun, vals, exps, stt)
    | Cnd1((), b, ebs, ob) =>
      switch asLgc(v) {
      | true => doBlk(b, stt)
      | false => transitionCnd(ebs, ob, stt)
      }
    | BgnDef(x, (), b) =>
      doSet(stt.env, x, v)
      transitionBgn(b, stt)
    | BgnExp((), b) => transitionBgn(b, stt)
    | PrgDef(vs, x, (), ts) =>
      doSet(stt.env, x, v)
      transitionPrg(vs, ts, stt)
    | PrgExp(vs, (), ts) =>
      print(v)
      transitionPrg(list{v, ...vs}, ts, stt)
    }
  }
}
and transitionCnd = (ebs, ob, stt) => {
  switch ebs {
  | list{} =>
    switch ob {
    | None =>
      let val = Uni
      continue(val, stt)
    | Some(b) => doBlk(b, stt)
    }
  | list{(e, b), ...ebs} => {
      let exp = e
      Continuing(Ev(exp, consCtx(Cnd1((), b, ebs, ob), stt)))
    }
  }
}
and doApp = (v, vs, stt): state => {
  switch asFun(v) {
  | Prm(p) => continue(delta(p, vs), stt)
  | Udf(xs, b, env) =>
    if Js.Array.length(xs) == Js.List.length(vs) {
      let env = extend(env, Js.Array.concat(xs, xsOfBlock(b)))

      {
        xs |> Js.Array.forEachi((x, i) => {
          doSet(env, x, Js.List.nth(vs, i) |> Js.Option.getExn)
        })
      }

      transitionBgn(b, pushStk(env, stt))
    } else {
      raise(RuntimeError(ArityMismatch(Exactly(Js.Array.length(xs)), Js.List.length(vs))))
    }
  }
}
and transitionApp = (f: value, vs: list<value>, es: list<expression>, stt) => {
  switch es {
  | list{} => doApp(f, vs, stt)
  | list{e, ...es} =>
    let exp = e
    Continuing(Ev(exp, consCtx(App2(f, vs, (), es), stt)))
  }
}

exception HasTerminated
let transition = (state: continuing_state): state => {
  try {
    switch state {
    | Returning(v, stk) => return(v, stk)
    | Setting(x, v, stt) => setting(x, v, stt)
    | Ev(exp, stt) =>
      switch exp {
      | Con(c) =>
        let val = Con(c)
        continue(val, stt)
      | Ref(x) =>
        let val = doRef(stt.env, x)
        continue(val, stt)
      | Set(x, e) =>
        let exp = e
        Continuing(Ev(exp, consCtx(Set1(x, ()), stt)))
      | Lam(xs, b) =>
        let v = Fun(Udf(xs |> Js.List.toVector, b, stt.env))
        continue(v, stt)
      | App(e, es) =>
        let exp = e
        Continuing(Ev(exp, consCtx(App1((), es), stt)))
      | Cnd(ebs, ob) => transitionCnd(ebs, ob, stt)
      }
    }
  } catch {
  | RuntimeError(err) => Terminated(Err(err))
  }
}
