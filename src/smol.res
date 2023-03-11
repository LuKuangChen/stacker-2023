// type definition = Defvar(string, expression)

open Belt

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
  | Let(list<(symbol, expression)>, block)
  | App(expression, list<expression>)
  | Cnd(list<(expression, block)>, option<block>)
  | Bgn(block)
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
  | VecNew
  | VecRef
  | VecSet
  | VecLen
  | Eqv
  | Error

type rec environmentFrame = {
  id: string,
  content: array<(symbol, ref<option<value>>)>,
}
and environment = list<environmentFrame>
and vector = (int, array<value>)
and function =
  // primitives
  | Prm(primitive)
  // User-Defined Functions
  | Udf(int, ref<option<string>>, array<symbol>, block, environment)
and value =
  // the Unit value
  | Uni
  // Constants
  | Con(constant)
  // Functions
  | Fun(function)
  // Vectors
  | Vec(vector)

let initialEnv: environment = list{
  {
    id: "primordial-env",
    content: [
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
      ("eqv?", ref(Some(Fun(Prm(Eqv))))),
      ("vec", ref(Some(Fun(Prm(VecNew))))),
      ("mvec", ref(Some(Fun(Prm(VecNew))))),
      ("vec-ref", ref(Some(Fun(Prm(VecRef))))),
      ("vec-set!", ref(Some(Fun(Prm(VecSet))))),
      ("vec-len", ref(Some(Fun(Prm(VecLen))))),
      ("error", ref(Some(Fun(Prm(Error))))),
    ],
  },
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
  | OutOfBound(int, int)
  | UserRaised(string)
exception RuntimeError(runtime_error)

let new_hav_id = () => Js.Math.random_int(100, 1000)
let new_env_id = () => Js.Math.random_int(1000, 10000)

module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let all_envs = ref(list{})

// hav = Heap-Allocated Values
let all_havs: ref<list<value>> = ref(list{})

let extend = (env, xs): environment => {
  if Array.length(xs) == 0 {
    env
  } else {
    let id = new_env_id()
    let frm = {
      id: Int.toString(id),
      content: xs |> Js.Array.map(x => (x, ref(None))),
    }
    let env = list{frm, ...env}
    all_envs := list{env, ...all_envs.contents}
    env
  }
}

let ifNone = (thunk: unit => 'X, o: option<'X>): 'X => {
  switch o {
  | Some(x) => x
  | None => thunk()
  }
}

let rec lookup = (env: environment, x) => {
  switch env {
  | list{} => raise(RuntimeError(UnboundIdentifier(x)))
  | list{frm, ...env} =>
    let {id: _, content} = frm
    content
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
let doSet = (env: environment, x, v) => {
  switch v {
  | Fun(Udf(_id, name, _xs, _env, _body)) =>
    switch name.contents {
    | None => name := Some(x)
    | _ => ()
    }

  | _ => ()
  }
  lookup(env, x).contents = Some(v)
}

type contextFrame =
  | Set1(symbol, unit)
  | App1(unit, list<expression>)
  | App2(value, list<value>, unit, list<expression>)
  | Let1(list<(symbol, value)>, (symbol, unit), list<(symbol, expression)>, block)
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
  | App(value, list<value>, commomState)
  | Setting(symbol, value, commomState)
  | VecSetting(vector, int, value, commomState)
  | Returning(value, stack)
type state =
  | Terminated(terminated_state)
  | Continuing(continuing_state)

let asNum = v =>
  switch v {
  | Con(Num(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("number", v)))
  }
let asStr = v =>
  switch v {
  | Con(Str(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("string", v)))
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
and asVec = v =>
  switch v {
  | Vec(id, f) => (id, f)
  | _else => raise(RuntimeError(ExpectButGiven("vector", v)))
  }

let doAdd = (u, v) => Con(Num(asNum(u) +. asNum(v)))

let pushStk = (new_env, {ctx, env, stk}) => {
  switch ctx {
  // tail-call optimization
  | list{} => {ctx: list{}, env: new_env, stk}
  | _ =>
    if new_env === env {
      // no need to push stack if the environment is the same
      {ctx, env, stk}
    } else {
      {ctx: list{}, env: new_env, stk: list{(ctx, env), ...stk}}
    }
  }
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

let eqv2 = (u, v) => {
  u == v
}

let deltaEq = (cmp, v, vs): value => {
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
  | VecNew => AtLeast(0)
  | VecRef => Exactly(2)
  | VecSet => Exactly(3)
  | VecLen => Exactly(1)
  | Eqv => AtLeast(2)
  | Error => Exactly(1)
  }

exception Impossible(string)
let rec return = (v: value, stk): state => {
  switch stk {
  | list{} => raise(Impossible("Should have been handled by the top-level"))
  | list{(ctx, env), ...stk} => continue(v, {ctx, env, stk})
  }
}
and delta = (p, vs) =>
  switch (p, vs) {
  | (Add, list{v, ...vs}) => continue(deltaNum1((. a, b) => a +. b, v, vs))
  | (Sub, list{v1, v2, ...vs}) => continue(deltaNum2((. a, b) => a -. b, v1, v2, vs))
  | (Mul, list{v, ...vs}) => continue(deltaNum1((. a, b) => a *. b, v, vs))
  | (Div, list{v1, v2, ...vs}) => continue(deltaNum2((. a, b) => a /. b, v1, v2, vs))
  | (Lt, list{v, ...vs}) => continue(deltaCmp((a, b) => a < b, v, vs))
  | (Eq, list{v, ...vs}) => continue(deltaCmp((a, b) => a == b, v, vs))
  | (Gt, list{v, ...vs}) => continue(deltaCmp((a, b) => a > b, v, vs))
  | (Le, list{v, ...vs}) => continue(deltaCmp((a, b) => a <= b, v, vs))
  | (Ge, list{v, ...vs}) => continue(deltaCmp((a, b) => a >= b, v, vs))
  | (Ne, list{v, ...vs}) => continue(deltaCmp((a, b) => a != b, v, vs))
  | (Eqv, list{v, ...vs}) => continue(deltaEq(eqv2, v, vs))
  | (VecNew, vs) => {
      let id = new_hav_id()
      let v = Vec(id, List.toArray(vs))
      all_havs := list{v, ...all_havs.contents}
      continue(v)
    }

  | (VecLen, list{v}) => {
      let (_id, vs) = asVec(v)
      continue(Con(Num(vs->Array.length->Int.toFloat)))
    }

  | (VecRef, list{v_vec, v_ind}) => {
      let (_id, vs) = asVec(v_vec)
      let v_ind = asNum(v_ind)->Float.toInt
      switch vs[v_ind] {
      | None => raise(RuntimeError(OutOfBound(Array.length(vs), v_ind)))
      | Some(v) => continue(v)
      }
    }

  | (VecSet, list{v_vec, v_ind, v_val}) => {
      let v_vec = asVec(v_vec)
      let v_ind = asNum(v_ind)->Float.toInt
      stt => Continuing(VecSetting(v_vec, v_ind, v_val, stt))
    }

  | (Error, list{v}) => {
      let v = asStr(v)
      _stt => Terminated(Err(UserRaised(v)))
    }

  | _otherwise => raise(RuntimeError(ArityMismatch(arityOf(p), List.length(vs))))
  }
and doVecSet = ((_id, vs), v_ind, v_val, stt) => {
  if vs[v_ind] = v_val {
    // The update is successful.
    continue(Uni, stt)
  } else {
    raise(RuntimeError(OutOfBound(Array.length(vs), v_ind)))
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
    | Let1(xvs, (x, ()), xes, b) => transitionLet(list{(x, v), ...xvs}, xes, b, stt)

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
and doEv = (exp: expression, stt) =>
  switch exp {
  | Con(c) =>
    let val = Con(c)
    continue(val, stt)
  | Ref(x) =>
    let val = doRef(stt.env, x)
    continue(val, stt)
  | Set(x, e) =>
    let exp = e
    doEv(exp, consCtx(Set1(x, ()), stt))
  | Lam(xs, b) => {
      let id = new_hav_id()
      let v = Fun(Udf(id, ref(None), xs |> Js.List.toVector, b, stt.env))
      all_havs := list{v, ...all_havs.contents}
      continue(v, stt)
    }

  | Let(xes, b) => transitionLet(list{}, xes, b, stt)

  | Bgn(b) => {
      let xs = xsOfBlock(b)
      let env = extend(stt.env, xs)
      transitionBgn(b, pushStk(env, stt))
    }

  | App(e, es) =>
    let exp = e
    doEv(exp, consCtx(App1((), es), stt))
  | Cnd(ebs, ob) => transitionCnd(ebs, ob, stt)
  }
and transitionLet = (xvs, xes, b, stt) => {
  switch xes {
  | list{} => {
      let xvs = xvs->List.reverse->List.toArray
      let xs = xvs->Array.map(((x, _v)) => x)
      let env = extend(stt.env, Array.concat(xs, xsOfBlock(b)))
      xvs->Array.forEach(((x, v)) => {
        doSet(env, x, v)
      })
      transitionBgn(b, pushStk(env, stt))
    }

  | list{(x, e), ...xes} => doEv(e, consCtx(Let1(xvs, (x, ()), xes, b), stt))
  }
}
and transitionBgn = ((ts, e), stt) => {
  switch ts {
  | list{} =>
    let exp = e
    doEv(exp, stt)
  | list{Def(Var(x, e0)), ...ts} =>
    let exp = e0
    doEv(exp, consCtx(BgnDef(x, (), (ts, e)), stt))
  | list{Def(Fun(f, xs, b)), ...ts} =>
    let exp = Lam(xs, b)
    doEv(exp, consCtx(BgnDef(f, (), (ts, e)), stt))
  | list{Exp(e0), ...ts} =>
    let exp = e0
    doEv(exp, consCtx(BgnExp((), (ts, e)), stt))
  }
}
and transitionPrg = (vs, ts, stt) => {
  switch ts {
  | list{} => Terminated(Tm(vs))
  | list{Def(Var(x, e0)), ...ts} =>
    let exp = e0
    doEv(exp, consCtx(PrgDef(vs, x, (), ts), stt))
  | list{Def(Fun(f, xs, b)), ...ts} =>
    let exp = Lam(xs, b)
    doEv(exp, consCtx(PrgDef(vs, f, (), ts), stt))
  | list{Exp(e0), ...ts} =>
    let exp = e0
    doEv(exp, consCtx(PrgExp(vs, (), ts), stt))
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
      doEv(exp, consCtx(Cnd1((), b, ebs, ob), stt))
    }
  }
}
and doApp = (v, vs, stt): state => {
  switch asFun(v) {
  | Prm(p) => delta(p, vs, stt)
  | Udf(_id, _name, xs, b, env) =>
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
  | list{} => {
      let vs = List.reverse(vs)
      switch f {
      // Don't pause if we are applying a primitive operator
      | Fun(Prm(_)) => doApp(f, vs, stt)
      | _ => Continuing(App(f, vs, stt))
      }
    }

  | list{e, ...es} =>
    let exp = e
    doEv(exp, consCtx(App2(f, vs, (), es), stt))
  }
}
and doBlk = (b, stt): state => {
  transitionBgn(b, pushStk(extend(stt.env, xsOfBlock(b)), stt))
}

// todo
let load = (program: program) => {
  all_envs := list{}
  all_havs := list{}
  let xs = xsOfBlock((program, Con(Num(42.0))))
  try {
    transitionPrg(list{}, program, {ctx: list{}, env: extend(initialEnv, xs), stk: list{}})
  } catch {
  | RuntimeError(err) => Terminated(Err(err))
  }
}

let transition = (state: continuing_state): state => {
  try {
    switch state {
    | Returning(v, stk) => return(v, stk)
    | Setting(x, v, stt) => setting(x, v, stt)
    | VecSetting(v, i, e, stt) => doVecSet(v, i, e, stt)
    // | Ev(exp, stt) => doEv(exp, stt)
    | App(f, vs, stt) => doApp(f, vs, stt)
    }
  } catch {
  | RuntimeError(err) => Terminated(Err(err))
  }
}