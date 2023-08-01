open Belt
open SExpression
open SMoL

let unann = ({it, ann: _}) => it
let annotate = (it, begin, end) => {it, ann: {begin, end}}
let dummy_ann = it => annotate(it, {ln: -1, ch: -1}, {ln: -1, ch: -1})

@module("./random") external make_random: string => array<unit => float> = "make_random"

let randomIntOfRandom = (. random) => {
  let f = (start, end) => {
    let delta = end - start
    let offset = Js.Math.round(random() *. Int.toFloat(delta))->Int.fromFloat
    start + offset
  }
  f
}

let makeRandomInt = seed => {
  // If I don't wrap the function in an array, Rescript will automatically collapse
  // the two arrows and eta-expand the first function.
  let random: unit => float = Array.getExn(make_random(seed), 0)
  randomIntOfRandom(. random)
}

let randomInt = ref(makeRandomInt(Js.Math.random()->Float.toString))

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
  | Udf(int, ref<option<string>>, srcrange, array<annotated<symbol>>, block, environment)
and value =
  // Constants
  | Con(constant)
  // Functions
  | VFun(function)
  // Vectors
  | Vec(vector)

let result_of_value = (v: value) => {
  switch v {
  | VFun(Prm(prm)) => PrmFun(prm)
  | Con(c) => Con(c)
  | VFun(Udf(addr, _, _, _, _, _)) => Fun(addr)
  | Vec((addr, _)) => Vec(addr)
  }
}

let observe = (v: value): annotated<expression> => {
  dummy_ann(Ref(dummy_ann(v->result_of_value->stringify.string_of_result)))
}

let initialEnv: environment = list{
  {
    id: "primordial-env",
    content: [
      ("+", ref(Some((VFun(Prm(Add)): value)))),
      ("-", ref(Some((VFun(Prm(Sub)): value)))),
      ("*", ref(Some((VFun(Prm(Mul)): value)))),
      ("/", ref(Some((VFun(Prm(Div)): value)))),
      ("<", ref(Some((VFun(Prm(Lt)): value)))),
      ("=", ref(Some((VFun(Prm(Eqv)): value)))),
      (">", ref(Some((VFun(Prm(Gt)): value)))),
      ("<=", ref(Some((VFun(Prm(Le)): value)))),
      (">=", ref(Some((VFun(Prm(Ge)): value)))),
      ("!=", ref(Some((VFun(Prm(Ne)): value)))),
      ("eq?", ref(Some((VFun(Prm(Eqv)): value)))),
      ("vec", ref(Some((VFun(Prm(VecNew)): value)))),
      ("mvec", ref(Some((VFun(Prm(VecNew)): value)))),
      ("vec-ref", ref(Some((VFun(Prm(VecRef)): value)))),
      ("vec-set!", ref(Some((VFun(Prm(VecSet)): value)))),
      ("vec-len", ref(Some((VFun(Prm(VecLen)): value)))),
      ("pair", ref(Some((VFun(Prm(PairNew)): value)))),
      ("mpair", ref(Some((VFun(Prm(PairNew)): value)))),
      ("left", ref(Some((VFun(Prm(PairRefLeft)): value)))),
      ("right", ref(Some((VFun(Prm(PairRefRight)): value)))),
      ("set-left!", ref(Some((VFun(Prm(PairSetLeft)): value)))),
      ("set-right!", ref(Some((VFun(Prm(PairSetRight)): value)))),
      ("error", ref(Some((VFun(Prm(Err)): value)))),
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

type runtimeError =
  | UnboundIdentifier(symbol)
  | UsedBeforeInitialization(symbol)
  | ExpectButGiven(string, value)
  | ArityMismatch(arity, int)
  | OutOfBound(int, int)
  | UserRaised(string)
exception RuntimeError(runtimeError)

let xsOfTerms = (ts: list<term>) => {
  open Js.List
  ts
  |> map((. trm) =>
    switch trm {
    | Def({ann: _, it: Var(x, _e)}) => list{x}
    | Def({ann: _, it: Fun(x, _ys, _b)}) => list{x}
    // | Def({ann: _, it: For(x, _e_from, _e_to, _b)}) => list{x}
    | Exp(_e) => list{}
    }
  )
  |> flatten
  |> toVector
}
let xsOfBlock = (b: block) => {
  let (ts, _e) = b
  xsOfTerms(ts)
}

let extend_block = (b: block, e: annotated<expression>): block => {
  let (ts, e0) = b
  (list{...ts, Exp(e0)}, e)
}

let newHavId = () => randomInt.contents(100, 1000)
let newEnvId = () => randomInt.contents(1000, 10000)

module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

let allEnvs = ref(list{})

type heap = ref<list<value>>
// hav = Heap-Allocated Values
let allHavs: ref<list<value>> = ref(list{})

let makeTopLevel = (env, xs): environment => {
  // Like extend but the id is hard-coded to be top-level
  let id = "top-level"
  let frm = {
    id,
    content: xs |> Js.Array.map(x => (x, ref(None))),
  }
  let env = list{frm, ...env}
  allEnvs := list{env, ...allEnvs.contents}
  env
}

let extend = (env, xs): environment => {
  // if Array.length(xs) == 0 {
    // env
  // } else {
    let id = newEnvId()
    let frm = {
      id: Int.toString(id),
      content: xs |> Js.Array.map(x => (x, ref(None))),
    }
    let env = list{frm, ...env}
    allEnvs := list{env, ...allEnvs.contents}
    env
  // }
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
    let {content} = frm
    // Js.log(`Looking up ${x} in ${id}`)
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
let doSet = (env: environment, x: annotated<symbol>, v: value) => {
  // Js.log(`Setting`)
  switch v {
  | VFun(Udf(_id, name, _meta, _xs, _env, _body)) =>
    switch name.contents {
    | None => name := Some(x.it)
    | _ => ()
    }

  | _ => ()
  }
  lookup(env, x.it).contents = Some(v)
}

type contextFrame =
  | Set1(annotated<symbol>, unit)
  | App1(unit, list<annotated<expression>>)
  | App2(value, list<value>, unit, list<annotated<expression>>)
  | AppPrm1(primitive, list<value>, unit, list<annotated<expression>>)
  | Let1(
      list<(annotated<symbol>, value)>,
      (annotated<symbol>, unit),
      list<(annotated<symbol>, annotated<expression>)>,
      block,
    )
  | If1(unit, annotated<expression>, annotated<expression>)
  | Cnd1(unit, block, list<(annotated<expression>, block)>, option<block>)
  | Bgn1(unit, list<annotated<expression>>, annotated<expression>)
type bodyBase =
  | BdyDef(annotated<symbol>, unit, block)
  | BdyExp(unit, block)
  | BdyRet
type programRedex =
  | Def(annotated<symbol>)
  | Exp
type programBase = (programRedex, list<term>)

let allVals: array<value> = []

type pile<'topping, 'base> = {topping: list<'topping>, base: 'base}
let add_pile = (new_topping, {topping, base}) => {topping: list{new_topping, ...topping}, base}
let new_pile = base => {topping: list{}, base}

type frame<'base> = {ctx: pile<contextFrame, 'base>, env: environment}
type stack = pile<frame<bodyBase>, frame<programBase>>

let current_env = (stk: stack): environment => {
  switch stk.topping->List.head->Option.map(f => f.env) {
  | Some(env) => env
  | None => stk.base.env
  }
}

let consCtx = (f: contextFrame, stk: stack) => {
  let {topping, base} = stk
  switch topping {
  | list{} => {
      let base = {...base, ctx: add_pile(f, base.ctx)}
      {topping: list{}, base}
    }
  | list{bodyFrame, ...topping} => {
      let bodyFrame = {...bodyFrame, ctx: add_pile(f, bodyFrame.ctx)}
      {topping: list{bodyFrame, ...topping}, base}
    }
  }
}

type terminated_state =
  | Err(runtimeError)
  | Tm
type entrance =
  | Let
  | App
let string_of_entrace = entrance =>
  switch entrance {
  | Let => "a `let` body"
  | App => "a function body"
  }
type redex =
  | AppPrming(primitive, list<value>)
  | Applying(value, list<value>)
  | Setting(annotated<symbol>, value)
  | VecSetting(vector, int, value)
type continuing_state =
  // no env and no ctx
  | Returning(value, stack)
  // no env
  | Entering(entrance, block, environment, stack)
  // all in
  | Reducing(redex, stack)
// a state always includes the heap. So we manage the heap as a global reference.
type state =
  | Terminated(terminated_state)
  | Continuing(continuing_state)

let asNum = (v: value) =>
  switch v {
  | Con(Num(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("number", v)))
  }
let asStr = (v: value) =>
  switch v {
  | Con(Str(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("string", v)))
  }
let asLgc = (v: value) =>
  switch v {
  | Con(Lgc(v)) => v
  | _else => raise(RuntimeError(ExpectButGiven("boolean", v)))
  }
and asFun = (v: value) =>
  switch v {
  | VFun(f) => f
  | _else => raise(RuntimeError(ExpectButGiven("function", v)))
  }
and asVec = v =>
  switch v {
  | Vec(id, f) => (id, f)
  | _else => raise(RuntimeError(ExpectButGiven("vector", v)))
  }

let deltaNum1 = (f, v, vs): value => {
  open Js.List
  let v = asNum(v)
  and vs = map((. v) => asNum(v), vs)
  Con(Num(Js.List.foldLeft(f, v, vs)))
}
let deltaNum2 = (f, v1, v2, vs): value => {
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
  | Err => Exactly(1)
  | PairNew => Exactly(2)
  | PairRefLeft | PairRefRight => Exactly(1)
  | PairSetLeft | PairSetRight => Exactly(2)
  }

exception Impossible(string)
// The current function call finished. We are returning
// a value.
let rec return = (v: value, s: stack): state => {
  switch s {
  | {topping: list{}, base: {ctx, env}} =>
    // no more function call, go to top-level
    continueTopLevel(v, ctx, env)
  | {topping: list{{ctx, env}, ...topping}, base} =>
    // continue the previous function call with the returned value
    continueBody(v, ctx, env, {topping, base})
  }
}
and delta = (p, vs) =>
  switch (p, vs) {
  | (Add, list{v, ...vs}) => return(deltaNum1((. a, b) => a +. b, v, vs))
  | (Sub, list{v1, v2, ...vs}) => return(deltaNum2((. a, b) => a -. b, v1, v2, vs))
  | (Mul, list{v, ...vs}) => return(deltaNum1((. a, b) => a *. b, v, vs))
  | (Div, list{v1, v2, ...vs}) => return(deltaNum2((. a, b) => a /. b, v1, v2, vs))
  | (Lt, list{v, ...vs}) => return(deltaCmp((a, b) => a < b, v, vs))
  | (Eq, list{v, ...vs}) => return(deltaEq(eqv2, v, vs))
  // | (Eq, list{v, ...vs}) => return(deltaCmp((a, b) => a == b, v, vs))
  | (Gt, list{v, ...vs}) => return(deltaCmp((a, b) => a > b, v, vs))
  | (Le, list{v, ...vs}) => return(deltaCmp((a, b) => a <= b, v, vs))
  | (Ge, list{v, ...vs}) => return(deltaCmp((a, b) => a >= b, v, vs))
  | (Ne, list{v, ...vs}) => return(deltaCmp((a, b) => a != b, v, vs))
  | (Eqv, list{v, ...vs}) => return(deltaEq(eqv2, v, vs))
  | (VecNew, vs) => {
      let id = newHavId()
      let v = Vec(id, List.toArray(vs))
      allHavs := list{v, ...allHavs.contents}
      return(v)
    }

  | (VecLen, list{v}) => {
      let (_id, vs) = asVec(v)
      return(Con(Num(vs->Array.length->Int.toFloat)))
    }

  | (VecRef, list{v_vec, v_ind}) => {
      let (_id, vs) = asVec(v_vec)
      let v_ind = asNum(v_ind)->Float.toInt
      switch vs[v_ind] {
      | None => raise(RuntimeError(OutOfBound(Array.length(vs), v_ind)))
      | Some(v) => return(v)
      }
    }

  | (VecSet, list{v_vec, v_ind, v_val}) => {
      let v_vec = asVec(v_vec)
      let v_ind = asNum(v_ind)->Float.toInt
      stk => Continuing(Reducing(VecSetting(v_vec, v_ind, v_val), stk))
    }

  | (Err, list{v}) => {
      let v = asStr(v)
      _stk => Terminated(Err(UserRaised(v)))
    }

  | _otherwise => raise(RuntimeError(ArityMismatch(arityOf(p), List.length(vs))))
  }
and doVecSet = ((_id, vs), v_ind, v_val, stk: stack) => {
  if vs[v_ind] = v_val {
    // The update is successful.
    return(Con(Uni), stk)
  } else {
    raise(RuntimeError(OutOfBound(Array.length(vs), v_ind)))
  }
}
and setting = (x, v, stk: stack) => {
  doSet(current_env(stk), x, v)
  return(Con(Uni), stk)
}
and continueTopLevel = (v: value, ctx: pile<contextFrame, programBase>, env: environment) => {
  let {topping, base} = ctx
  switch topping {
  | list{} =>
    let (redex, ts) = base
    switch redex {
    | Def(x) =>
      doSet(env, x, v)
      transitionPrg(ts, env)
    | Exp =>
      print(v)
      let _ = Js.Array.push(v, allVals)
      transitionPrg(ts, env)
    }
  | list{ctxFrame, ...topping} => {
      let stk: stack = {topping: list{}, base: {ctx: {topping, base}, env}}
      handleCtxFrame(v, ctxFrame, stk)
    }
  }
}
and continueBody = (v: value, ctx, env, stk): state => {
  let {topping, base} = ctx
  switch topping {
  | list{} =>
    switch base {
    | BdyRet => Continuing(Returning(v, stk))
    | BdyExp((), b) => transitionBlock(b, env, stk)
    | BdyDef(x, (), b) => {
        doSet(env, x, v)
        transitionBlock(b, env, stk)
      }
    }
  | list{ctxFrame, ...topping} =>
    handleCtxFrame(v, ctxFrame, add_pile({ctx: {topping, base}, env}, stk))
  }
}
and handleCtxFrame = (v, ctxFrame, stk: stack) => {
  switch ctxFrame {
  | Set1(x, ()) => Continuing(Reducing(Setting(x, v), stk))
  | Let1(xvs, (x, ()), xes, b) => transitionLet(list{(x, v), ...xvs}, xes, b, stk)
  | App1((), exps) =>
    let fun = v
    and vals = list{}
    transitionApp(fun, vals, exps, stk)
  | App2(fun, vals, (), exps) =>
    let vals = list{v, ...vals}
    transitionApp(fun, vals, exps, stk)
  | AppPrm1(fun, vals, (), exps) =>
    let vals = list{v, ...vals}
    transitionAppPrm(fun, vals, exps, stk)
  | Cnd1((), b, ebs, ob) =>
    switch asLgc(v) {
    | true => doBlk(b, stk)
    | false => transitionCnd(ebs, ob, stk)
    }
  | If1((), e_thn, e_els) =>
    switch asLgc(v) {
    | true => doEv(e_thn, stk)
    | false => doEv(e_els, stk)
    }
  | Bgn1((), es, e) => transitionBgn(es, e, stk)
  }
}

and doEv = (exp: annotated<expression>, stk: stack) =>
  switch exp.it {
  | Con(c) => return(Con(c), stk)
  | Ref(x) =>
    let val = doRef(current_env(stk), x.it)
    return(val, stk)
  | Set(x, e) =>
    let exp = e
    doEv(exp, consCtx(Set1(x, ()), stk))
  | Lam(xs, b) => {
      let id = newHavId()
      let v: value = VFun(Udf(id, ref(None), exp.ann, xs |> Js.List.toVector, b, current_env(stk)))
      allHavs := list{v, ...allHavs.contents}
      return(v, stk)
    }
  | Let(xes, b) => transitionLet(list{}, xes, b, stk)

  | Bgn(es, e) => transitionBgn(es, e, stk)

  | AppPrm(p, es) => transitionAppPrm(p, list{}, es, stk)
  | App(e, es) =>
    let exp = e
    doEv(exp, consCtx(App1((), es), stk))
  | Cnd(ebs, ob) => transitionCnd(ebs, ob, stk)
  | If(e_cnd, e_thn, e_els) => doEv(e_cnd, consCtx(If1((), e_thn, e_els), stk))
  }
and transitionLet = (xvs, xes: list<(annotated<symbol>, annotated<expression>)>, b, stk: stack) => {
  switch xes {
  | list{} => {
      let xvs = xvs->List.reverse->List.toArray
      let xs = xvs->Array.map(((x, _v)) => x)
      let env = extend(current_env(stk), Array.concat(xs, xsOfBlock(b))->Array.map(unann))
      xvs->Array.forEach(((x, v)) => {
        doSet(env, x, v)
      })
      Continuing(entering(Let, b, env, stk))
    }

  | list{(x, e), ...xes} => doEv(e, consCtx(Let1(xvs, (x, ()), xes, b), stk))
  }
}
and transitionBlock = ((ts, e), env: environment, stk: stack) => {
  switch ts {
  | list{} =>
    let exp = e
    doEv(exp, add_pile({ctx: new_pile(BdyRet), env}, stk))
  | list{Def({ann: _, it: Var(x, e0)}), ...ts} =>
    let exp = e0
    doEv(exp, add_pile({ctx: new_pile(BdyDef(x, (), (ts, e))), env}, stk))
  | list{Def({ann, it: Fun(f, xs, b)}), ...ts} =>
    let exp = annotate(Lam(xs, b), ann.begin, ann.end)
    doEv(exp, add_pile({ctx: new_pile(BdyDef(f, (), (ts, e))), env}, stk))
  | list{Exp(e0), ...ts} =>
    let exp = e0
    doEv(exp, add_pile({ctx: new_pile(BdyExp((), (ts, e))), env}, stk))
  }
}
and transitionPrg = (ts, env: environment) => {
  switch ts {
  | list{} => Terminated(Tm)
  | list{Def({ann: _, it: Var(x, e0)}), ...ts} =>
    let exp = e0
    doEv(exp, new_pile({env, ctx: new_pile((Def(x), ts))}))
  | list{Def({ann, it: Fun(f, xs, b)}), ...ts} =>
    let exp = annotate(Lam(xs, b), ann.begin, ann.end)
    doEv(exp, new_pile({env, ctx: new_pile((Def(f), ts))}))
  | list{Exp(e0), ...ts} =>
    let exp = e0
    doEv(exp, new_pile({env, ctx: new_pile((Exp, ts))}))
  }
}
and transitionCnd = (ebs, ob, stk: stack) => {
  switch ebs {
  | list{} =>
    switch ob {
    | None => return(Con(Uni), stk)
    | Some(b) => doBlk(b, stk)
    }
  | list{(e, b), ...ebs} => {
      let exp = e
      doEv(exp, consCtx(Cnd1((), b, ebs, ob), stk))
    }
  }
}
and transitionBgn = (es, e, stk: stack) => {
  switch es {
  | list{} => doEv(e, stk)
  | list{e0, ...es} => doEv(e0, consCtx(Bgn1((), es, e), stk))
  }
}
and doLoop = (e, b, exp, stk: stack) => {
  let e1: annotated<expression> = e
  let b1: block = extend_block(b, exp)
  let b2: block = (list{}, annotate((Con(Uni): expression), exp.ann.begin, exp.ann.end))
  let e = annotate(Cnd(list{(e1, b1)}, Some(b2)), exp.ann.begin, exp.ann.end)
  doEv(e, stk)
}
and doApp = (v, vs, stk): state => {
  switch asFun(v) {
  | Prm(p) => delta(p, vs, stk)
  | Udf(_id, _name, _ann, xs, b, env) =>
    if Js.Array.length(xs) == Js.List.length(vs) {
      let env = extend(env, Js.Array.concat(xs, xsOfBlock(b))->Array.map(unann))

      {
        xs |> Js.Array.forEachi((x, i) => {
          doSet(env, x, Js.List.nth(vs, i) |> Js.Option.getExn)
        })
      }

      Continuing(entering(App, b, env, stk))
    } else {
      raise(RuntimeError(ArityMismatch(Exactly(Js.Array.length(xs)), Js.List.length(vs))))
    }
  }
}
and entering = (entrance, b, env, stk) => {
  switch stk {
  // tail-call optimization
  | {topping: list{{ctx: {topping: list{}, base: BdyRet}, env: _}, ...topping}, base} =>
    Entering(entrance, b, env, {topping, base})
  | stk => Entering(entrance, b, env, stk)
  }
}
and doEntering = (b, env, stk): state => {
  transitionBlock(b, env, stk)
}
and transitionAppPrm = (
  f: primitive,
  vs: list<value>,
  es: list<annotated<expression>>,
  stk: stack,
) => {
  switch es {
  | list{} => {
      let vs = List.reverse(vs)
      doApp(VFun(Prm(f)), vs, stk)
    }

  | list{e, ...es} =>
    let exp = e
    doEv(exp, consCtx(AppPrm1(f, vs, (), es), stk))
  }
}
and transitionApp = (f: value, vs: list<value>, es: list<annotated<expression>>, stk: stack) => {
  switch es {
  | list{} => {
      let vs = List.reverse(vs)
      switch f {
      // Don't pause if we are applying a primitive operator
      | VFun(Prm(_)) => doApp(f, vs, stk)
      | _ => Continuing(Reducing(Applying(f, vs), stk))
      }
    }

  | list{e, ...es} =>
    let exp = e
    doEv(exp, consCtx(App2(f, vs, (), es), stk))
  }
}
and doBlk = (b, stk): state => {
  let xs = xsOfBlock(b)->Array.map(unann)
  let env = current_env(stk)
  let env = if (Array.length(xs) == 0) {
    env
  } else {
    extend(env, xs)
  }
  let stk = switch stk {
  // tail-call optimization
  | {topping: list{{ctx: {topping: list{}, base: BdyRet}, env: _}, ...topping}, base} =>
    {topping, base}
  | stk => stk
  }
  transitionBlock(b, env, stk)
}

// todo
let load = (program: program, randomSeed: string) => {
  // initialize all global things
  allEnvs := list{}
  allHavs := list{}
  let _ = Js.Array.removeFromInPlace(~pos=0, allVals)
  randomInt := makeRandomInt(randomSeed)

  // now let's get started
  let xs = xsOfTerms(program)
  let env = makeTopLevel(initialEnv, xs->Array.map(unann))
  try {
    transitionPrg(program, env)
  } catch {
  | RuntimeError(err) => Terminated(Err(err))
  }
}

let transition = (state: continuing_state): state => {
  try {
    switch state {
    | Returning(v, stk: stack) => return(v, stk)
    | Entering(_, b, env, stk: stack) => doEntering(b, env, stk)
    | Reducing(redex, stk: stack) =>
      switch redex {
      | Setting(x, v) => setting(x, v, stk)
      | VecSetting(v, i, e) => doVecSet(v, i, e, stk)
      | AppPrming(f, vs) => doApp(VFun(Prm(f)), vs, stk)
      | Applying(f, vs) => doApp(f, vs, stk)
      }
    }
  } catch {
  | RuntimeError(err) => Terminated(Err(err))
  }
}
