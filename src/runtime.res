open Belt
open SExpression
open! SMoL
open Primitive

type primitive = Primitive.t

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

type pile<'topping, 'base> = {topping: list<'topping>, base: 'base}
let new_pile = base => {topping: list{}, base}
let add_pile = (new_topping, {topping, base}) => {topping: list{new_topping, ...topping}, base}

type bodyBaseNode =
  | BdyDef(annotated<symbol, printAnn>, (unit, sourceLocation), block<printAnn>)
  | BdyExp((unit, sourceLocation), block<printAnn>)
  | BdyRet((unit, sourceLocation))
type bodyBaseBase = annotated<bodyBaseNode, printAnn>
type programBaseNode =
  | PDef(annotated<symbol, printAnn>, (unit, sourceLocation), program<printAnn>)
  | PExp((unit, sourceLocation), program<printAnn>)
type programBase = annotated<programBaseNode, printAnn>

let holeOfBodyBase = (bodyBaseNode: bodyBaseNode): sourceLocation => {
  switch bodyBaseNode {
  | BdyDef(_, ((), sourceLocation), _) => sourceLocation
  | BdyExp(((), sourceLocation), _) => sourceLocation
  | BdyRet(((), sourceLocation)) => sourceLocation
  }
}
let holeOfProgramBase = (programBaseNode: programBaseNode): sourceLocation => {
  switch programBaseNode {
  | PDef(_, ((), sourceLocation), _) => sourceLocation
  | PExp(((), sourceLocation), _) => sourceLocation
  }
}

module EnvironmentID = {
  type t =
    | Primordial
    | TopLevel
    | Extended(int)
  let toString = envID => {
    switch envID {
    | Primordial => "primordial-env"
    | TopLevel => "top-level"
    | Extended(i) => `${Int.toString(i)}`
    }
  }
}

type rec environmentFrame = {
  id: EnvironmentID.t,
  content: array<(annotated<symbol, printAnn>, ref<option<value>>)>,
}
and generatorStatus =
  | Fresh(block<printAnn>, environment)
  | Suspended(pile<contextFrame, bodyBaseBase>, environment)
  | Running
  | Done
and bodyBase = {isGen: option<generator>, base: bodyBaseBase}
and environment = list<environmentFrame>
and vector = {
  id: int,
  contents: array<value>,
}
and function = {
  id: int,
  isGen: bool,
  name: ref<option<string>>,
  sourceLocation: kindedSourceLocation,
  xs: array<annotated<symbol, printAnn>>,
  body: block<printAnn>,
  env: environment,
  print: print<kindedSourceLocation>,
}
and generator = {
  id: int,
  status: ref<generatorStatus>,
}
and value =
  // Constants
  | Con(constant)
  // Functions
  | VFun(function)
  | VGen(generator)
  // Vectors
  | Vec(vector)
and contextFrame =
  | Yield1((unit, sourceLocation))
  | Set1(annotated<symbol, printAnn>, (unit, sourceLocation))
  | App1((unit, sourceLocation), list<expression<printAnn>>)
  | App2(
      (value, sourceLocation),
      list<(value, sourceLocation)>,
      (unit, sourceLocation),
      list<expression<printAnn>>,
    )
  | AppPrm1(
      SMoL.Primitive.t,
      list<(value, sourceLocation)>,
      (unit, sourceLocation),
      list<expression<printAnn>>,
    )
  | Let1(
      list<(annotated<symbol, printAnn>, (value, sourceLocation))>,
      (annotated<symbol, printAnn>, (unit, sourceLocation)),
      list<bind<printAnn>>,
      block<printAnn>,
    )
  | If1((unit, sourceLocation), expression<printAnn>, expression<printAnn>)
  | Cnd1(
      (unit, sourceLocation),
      block<printAnn>,
      list<(expression<printAnn>, block<printAnn>)>,
      option<block<printAnn>>,
    )
  | Bgn1((unit, sourceLocation), list<expression<printAnn>>, expression<printAnn>)
and frame<'base> = {ctx: pile<contextFrame, 'base>, env: environment}

let rmSrcLoc = ((it, _): ('a, sourceLocation)): 'a => {
  it
}

let holeOfFrame = (f: contextFrame): sourceLocation => {
  switch f {
  | Yield1((_, srcLoc)) => srcLoc
  | Set1(_, ((), srcLoc)) => srcLoc
  | App1(((), srcLoc), _) => srcLoc
  | App2(_, _, ((), srcLoc), _) => srcLoc
  | AppPrm1(_, _, ((), srcLoc), _) => srcLoc
  | Let1(_, (_, (_, srcLoc)), _, _) => srcLoc
  | If1(((), srcLoc), _, _) => srcLoc
  | Cnd1(((), srcLoc), _, _, _) => srcLoc
  | Bgn1(((), srcLoc), _, _) => srcLoc
  }
}

let valuesOfFrame = (f: contextFrame): list<(value, sourceLocation)> => {
  switch f {
  | Yield1(_) => list{}
  | Set1(_, _) => list{}
  | App1(_, _) => list{}
  | App2(v, vs, _, _) => list{v, ...vs}
  | AppPrm1(_, vs, _, _) => vs
  | Let1(xvs, _, _, _) => List.map(xvs, ((_, v)) => v)
  | If1(_, _, _) => list{}
  | Cnd1(_, _, _, _) => list{}
  | Bgn1(_, _, _) => list{}
  }
}

let printCon = constant => SMoLPrinter.printOutput(list{SMoL.OVal(Con(constant))})

// This is the honest display of values
let printValue = (v: value) => {
  switch v {
  | Con(constant) => printCon(constant)
  | VFun({id}) => `@${id |> Int.toString}`
  | VGen({id}) => `@${id |> Int.toString}`
  | Vec({id}) => `@${id |> Int.toString}`
  }
}

let makePrimitiveName = name => {
  {
    it: name,
    ann: {print: Plain(name), sourceLocation: {begin: {ln: 0, ch: 0}, end: {ln: 0, ch: 0}}},
  }
}

let initialEnv: environment = list{
  {
    id: Primordial,
    content: [],
  },
}

type result =
  | Err
  | Val(value)

module RTArity = {
  type t =
    | AtLeast(int)
    | Exactly(int)
  let toString = arity => {
    switch arity {
    | AtLeast(i) => `${i |> Int.toString} or more`
    | Exactly(i) => `${i |> Int.toString}`
    }
  }
  let minimalArity = arity => {
    switch arity {
    | AtLeast(i) => i
    | Exactly(i) => i
    }
  }
}
type arity = RTArity.t

type runtimeError =
  | UnboundIdentifier(symbol)
  | RedefinedIdentifier(symbol, string) // the string is the environment id
  | UsedBeforeInitialization(symbol)
  | ExpectButGiven(string, value)
  | ArityMismatch(arity, int)
  | OutOfBound(int, int)
  | DivisionByZero
  | AnyError(string)
exception RuntimeError(runtimeError)

let raiseRuntimeError = e => raise(RuntimeError(e))

module IntHash = Belt.Id.MakeHashable({
  type t = int
  let hash = a => a
  let eq = (a, b) => a == b
})

// This is the pretty presentation of values
let outputletOfValue = (v: value): outputlet => {
  let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
  let rec p = (visited: list<int>, v: value): val => {
    switch v {
    | Con(constant) => Con(constant)
    | VFun(_) => raise(RuntimeError(AnyError("Can't print functions")))
    | VGen(_) => raise(RuntimeError(AnyError("Can't print generators")))
    | Vec({id, contents: es}) =>
      if List.has(visited, id, (a, b) => a == b) {
        let r = switch HashMap.get(hMap, id) {
        | None => {
            let r = HashMap.size(hMap)
            HashMap.set(hMap, id, r)
            r
          }
        | Some(r) => r
        }
        Ref(r)
      } else {
        let p = p(list{id, ...visited})
        let es = Array.map(es, p) |> List.fromArray
        Struct(HashMap.get(hMap, id), Vec(es))
      }
    }
  }
  OVal(p(list{}, v))
}

let xsOfDef = (d: definition<printAnn>) => {
  switch d.it {
  | Var(x, _e) => list{x}
  | Fun(x, _ys, _b) => list{x}
  | GFun(x, _ys, _b) => list{x}
  }
}

let xsOfTerms = (ts: list<term<printAnn>>) => {
  open Js.List
  ts
  |> map((. trm) =>
    switch (trm.it: termNode<printAnn>) {
    | SMoL.Def(d) => xsOfDef(d)
    | SMoL.Exp(_e) => list{}
    }
  )
  |> flatten
  |> toVector
}

// let extend_block = (b: block<printAnn>, e: expression<printAnn>): blockNode<printAnn> => {
//   let (ts, e0) = b.it
//   (list{...ts, SMoL.Exp(e0)}, e)
// }

let newHavId = () => randomInt.contents(100, 1000)
let newEnvId = () => randomInt.contents(1000, 10000)

let allEnvs = ref(list{})

type heap = ref<list<value>>
// hav = Heap-Allocated Values
let allHavs: ref<list<value>> = ref(list{})

let printTopLevel = ref(true)

// the bool tells whether the message is an error
let stdout: ref<output> = ref(list{})
let printStdout = s => {
  stdout := list{s, ...stdout.contents}
}

let rec appear_in = (x, ys) => {
  switch ys {
  | list{} => false
  | list{y, ...ys} => x == y || appear_in(x, ys)
  }
}

let has_duplicates = (xs: list<symbol>) => {
  switch xs {
  | list{} => None
  | list{x, ...xs} =>
    if appear_in(x, xs) {
      Some(x)
    } else {
      None
    }
  }
}

let check_duplicate = (xs, env_id) => {
  switch has_duplicates(xs |> List.fromArray) {
  | Some(x) => raise(RuntimeError(RedefinedIdentifier(x, env_id |> EnvironmentID.toString)))
  | None => ()
  }
}

let makeTopLevel = (env, xs): environment => {
  // Like extend but the id is hard-coded to be top-level
  let id = EnvironmentID.TopLevel
  let frm = {
    id,
    content: xs |> Js.Array.map(x => (x, ref(None))),
  }
  let env = list{frm, ...env}
  allEnvs := list{env, ...allEnvs.contents}
  check_duplicate(xs->Array.map(x => x.it), id)
  env
}

let extend = (env, xs): environment => {
  // if Array.length(xs) == 0 {
  // env
  // } else {
  let id = EnvironmentID.Extended(newEnvId())
  let frm = {
    id,
    content: xs |> Js.Array.map(x => (x, ref(None))),
  }
  let env = list{frm, ...env}
  allEnvs := list{env, ...allEnvs.contents}
  check_duplicate(xs->Array.map(x => x.it), id)
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
    |> Js.Array.find(((y, _v)) => x == y.it)
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
let doSet = (env: environment, x: annotated<symbol, printAnn>, v: value) => {
  // Js.log(`Setting`)
  switch v {
  | VFun({name}) =>
    switch name.contents {
    | None => name := Some(x.it)
    | _ => ()
    }

  | _ => ()
  }
  lookup(env, x.it).contents = Some(v)
}

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
  | Applying(value, list<value>)
  | Setting(annotated<symbol, printAnn>, value)
  | VecSetting(vector, int, value)
  | Printing(value)
  | Yielding(value)
  | Nexting(generator)
type continuing_state =
  // no env and no ctx
  | Returning(value, stack)
  // no env
  | Entering(entrance, block<printAnn>, environment, stack)
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
and asGen = (v: value) =>
  switch v {
  | VGen(udg) => udg
  | _else => raise(RuntimeError(ExpectButGiven("generator", v)))
  }
and asVec = v =>
  switch v {
  | Vec(v) => v
  | _else => raise(RuntimeError(ExpectButGiven("vector", v)))
  }
and asPair = v =>
  switch v {
  | Vec(v) => {
      let {id, contents: es} = v
      if Array.length(es) != 2 {
        raise(RuntimeError(ExpectButGiven("pair", Vec(v))))
      }
      v
    }
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
  | Arith(Add) => RTArity.AtLeast(1)
  | Arith(Sub) => AtLeast(2)
  | Arith(Mul) => AtLeast(1)
  | Arith(Div) => AtLeast(2)
  | Cmp(_) => AtLeast(2)
  | VecNew => AtLeast(0)
  | VecRef => Exactly(2)
  | VecSet => Exactly(3)
  | VecLen => Exactly(1)
  | Err => Exactly(1)
  | Not => Exactly(1)
  | PairNew => Exactly(2)
  | PairRefLeft | PairRefRight => Exactly(1)
  | PairSetLeft | PairSetRight => Exactly(2)
  | Print => Exactly(1)
  | Next => Exactly(1)
  | Cons => Exactly(2)
  }

exception Impossible(string)
// The current function call finished. We are returning
// a value.
let rec return = (v: value, s: stack): state => {
  switch s {
  | {topping: list{}, base: {ctx, env}} => continueTopLevel(v, ctx, env)
  | {topping: list{{ctx, env}, ...topping}, base} => continueBody(v, ctx, env, {topping, base})
  }
}
and yield = (v: value, s: stack): state => {
  switch s {
  | {topping: list{}, base: _} => raise(RuntimeError(AnyError("yielding from the top-level block")))
  | {topping: list{{ctx, env}, ...topping}, base} =>
    switch ctx.base.isGen {
    | None => raise(RuntimeError(AnyError("yielding from a non-generative function")))
    | Some({status: r}) =>
      switch r.contents {
      | Running => {
          r := Suspended({topping: ctx.topping, base: ctx.base.base}, env)
          return(v, {topping, base})
        }
      | _ => raise(RuntimeError(AnyError("Internal error, please contact the developer")))
      }
    }
  }
}
and make_vector = vs => {
  let id = newHavId()
  let v = Vec({id, contents: List.toArray(vs)})
  allHavs := list{v, ...allHavs.contents}
  return(v)
}
and delta = (p, vs) =>
  switch (p, vs) {
  | (Arith(Add), list{v, ...vs}) => return(deltaNum1((. a, b) => a +. b, v, vs))
  | (Arith(Sub), list{v1, v2, ...vs}) => return(deltaNum2((. a, b) => a -. b, v1, v2, vs))
  | (Arith(Mul), list{v, ...vs}) => return(deltaNum1((. a, b) => a *. b, v, vs))
  | (Arith(Div), list{v1, v2, ...vs}) => return(deltaNum2((. a, b) => {
        if b == 0. {
          raise(RuntimeError(DivisionByZero))
        } else {
          a /. b
        }
      }, v1, v2, vs))
  | (Cmp(Lt), list{v, ...vs}) => return(deltaCmp((a, b) => a < b, v, vs))
  | (Cmp(Eq), list{v, ...vs}) => return(deltaEq(eqv2, v, vs))
  // | (Cmp(Eq), list{v, ...vs}) => return(deltaCmp((a, b) => a == b, v, vs))
  | (Cmp(Gt), list{v, ...vs}) => return(deltaCmp((a, b) => a > b, v, vs))
  | (Cmp(Le), list{v, ...vs}) => return(deltaCmp((a, b) => a <= b, v, vs))
  | (Cmp(Ge), list{v, ...vs}) => return(deltaCmp((a, b) => a >= b, v, vs))
  | (Cmp(Ne), list{v, ...vs}) => return(deltaCmp((a, b) => a != b, v, vs))
  | (VecNew, vs) => {
      let id = newHavId()
      let v = Vec({id, contents: List.toArray(vs)})
      allHavs := list{v, ...allHavs.contents}
      return(v)
    }

  | (VecRef, list{v_vec, v_ind}) => {
      let {contents: vs} = asVec(v_vec)
      let v_ind = asNum(v_ind)->Float.toInt
      switch vs[v_ind] {
      | None => raise(RuntimeError(OutOfBound(Array.length(vs), v_ind)))
      | Some(v) => return(v)
      }
    }

  | (VecLen, list{v}) => {
      let {contents: vs} = asVec(v)
      return(Con(Num(vs->Array.length->Int.toFloat)))
    }

  | (VecSet, list{v_vec, v_ind, v_val}) => {
      let v_vec = asVec(v_vec)
      let v_ind = asNum(v_ind)->Float.toInt
      stk => Continuing(Reducing(VecSetting(v_vec, v_ind, v_val), stk))
    }

  | (Err, list{v}) => {
      let v = asStr(v)
      _stk => Terminated(Err(AnyError(v)))
    }

  | (Not, list{v}) => {
      let v = asLgc(v)
      return(Con(Lgc(!v)))
    }

  | (PairNew, list{v1, v2}) => make_vector(list{v1, v2})

  | (PairRefLeft, list{v}) => {
      let {contents: vs} = asPair(v)
      let v_ind = 0
      switch vs[v_ind] {
      | None => raise(Impossible("we have checked that this value is a pair"))
      | Some(v) => return(v)
      }
    }

  | (PairRefRight, list{v}) => {
      let {contents: vs} = asPair(v)
      let v_ind = 1
      switch vs[v_ind] {
      | None => raise(Impossible("we have checked that this value is a pair"))
      | Some(v) => return(v)
      }
    }

  | (PairSetLeft, list{v_vec, v_val}) => {
      let v_vec = asPair(v_vec)
      let v_ind = 0
      stk => Continuing(Reducing(VecSetting(v_vec, v_ind, v_val), stk))
    }

  | (PairSetRight, list{v_vec, v_val}) => {
      let v_vec = asPair(v_vec)
      let v_ind = 1
      stk => Continuing(Reducing(VecSetting(v_vec, v_ind, v_val), stk))
    }

  | (Print, list{v}) => stk => Continuing(Reducing(Printing(v), stk))

  // Js.Console.log(outputletOfValue(v))
  // return(Con(Uni))

  | (Next, list{v}) =>
    stk => {
      Continuing(Reducing(Nexting(asGen(v)), stk))
    }

  | _otherwise => {
      let wantedArity = arityOf(p)
      let actualArity = List.length(vs)
      if RTArity.minimalArity(wantedArity) != actualArity {
        raise(RuntimeError(ArityMismatch(wantedArity, actualArity)))
      } else {
        raise(RuntimeError(AnyError(`Internal error with ${SMoL.Primitive.toString(p)}`)))
      }
    }
  }
and doVecSet = ({contents: vs}, v_ind, v_val, stk: stack) => {
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
    switch base.it {
    | PDef(x, ((), _srcLoc), p) =>
      doSet(env, x, v)
      transitionPrg(p, env)
    | PExp(((), _srcLoc), p) =>
      if printTopLevel.contents {
        switch v {
        | Con(Uni) => transitionPrg(p, env)
        | v => Continuing(Reducing(Printing(v), new_pile({env, ctx})))
        }
      } else {
        transitionPrg(p, env)
      }
    }
  | list{ctxFrame, ...topping} => {
      let stk: stack = {topping: list{}, base: {ctx: {topping, base}, env}}
      handleCtxFrame(v, ctxFrame, stk)
    }
  }
}
and continueBody = (v: value, ctx, env, stk): state => {
  let {topping, base: {isGen, base}} = ctx
  switch topping {
  | list{} =>
    switch base.it {
    | BdyRet((), _srcLoc) => {
        isGen->Option.forEach(({status}) => {
          status := Done
        })
        Continuing(Returning(v, stk))
      }
    | BdyExp(((), _srcLoc), b) => transitionBlock(b, isGen, env, stk)
    | BdyDef(x, ((), _srcLoc), b) => {
        doSet(env, x, v)
        transitionBlock(b, isGen, env, stk)
      }
    }
  | list{ctxFrame, ...topping} =>
    handleCtxFrame(v, ctxFrame, add_pile({ctx: {topping, base: {isGen, base}}, env}, stk))
  }
}
and handleCtxFrame = (v: value, ctxFrame, stk: stack) => {
  switch ctxFrame {
  | Set1(x, ((), _srcLoc)) => Continuing(Reducing(Setting(x, v), stk))
  | Let1(xvs, (x, ((), srcLoc)), xes, b) =>
    transitionLet(list{(x, (v, srcLoc)), ...xvs}, xes, b, stk)
  | App1(((), srcLoc), exps) =>
    let fun = v
    and vals = list{}
    transitionApp((fun, srcLoc), vals, exps, stk)
  | App2(fun, vals, ((), srcLoc), exps) =>
    let vals = list{(v, srcLoc), ...vals}
    transitionApp(fun, vals, exps, stk)
  | AppPrm1(fun, vals, ((), srcLoc), exps) =>
    let vals = list{(v, srcLoc), ...vals}
    transitionAppPrm(fun, vals, exps, stk)
  | Cnd1(((), _srcLoc), b, ebs, ob) =>
    switch asLgc(v) {
    | true => doBlk(b, None, stk)
    | false => transitionCnd(ebs, ob, stk)
    }
  | If1(((), _srcLoc), e_thn, e_els) =>
    switch asLgc(v) {
    | true => doEv(e_thn, stk)
    | false => doEv(e_els, stk)
    }
  | Bgn1(((), _srcLoc), es, e) => transitionBgn(es, e, stk)
  | Yield1((), _srcLoc) =>
    // switch stk {}
    Continuing(Reducing(Yielding(v), stk))
  }
}
and doEv = (exp: expression<printAnn>, stk: stack) =>
  switch exp.it {
  | Con(c) => return(Con(c), stk)
  | Ref(x) =>
    let val = doRef(current_env(stk), x)
    return(val, stk)
  | Set(x, e) =>
    let exp = e
    doEv(exp, consCtx(Set1(x, ((), exp.ann.sourceLocation)), stk))
  | Lam(xs, b) => {
      let id = newHavId()
      let v: value = VFun({
        id,
        isGen: false,
        name: ref(None),
        sourceLocation: {
          nodeKind: Expression,
          sourceLocation: exp.ann.sourceLocation
        },
        xs: xs |> Js.List.toVector,
        body: b,
        env: current_env(stk),
        print: getPrint(exp),
      })
      allHavs := list{v, ...allHavs.contents}
      return(v, stk)
    }
  | GLam(xs, b) => {
      let id = newHavId()
      let v: value = VFun({
        id,
        isGen: false,
        name: ref(None),
        sourceLocation: {
          nodeKind: Expression,
          sourceLocation: exp.ann.sourceLocation
        },
        xs: xs |> Js.List.toVector,
        body: b,
        env: current_env(stk),
        print: getPrint(exp),
      })
      allHavs := list{v, ...allHavs.contents}
      return(v, stk)
    }
  | Let(xes, b) => transitionLet(list{}, xes, b, stk)
  | Letrec(xes, b) => transitionLetrec(exp.ann, xes, b, stk)

  | Bgn(es, e) => transitionBgn(es, e, stk)

  | AppPrm(p, es) => transitionAppPrm(p, list{}, es, stk)
  | App(e, es) =>
    let exp = e
    doEv(exp, consCtx(App1(((), exp.ann.sourceLocation), es), stk))
  | Cnd(ebs, ob) => transitionCnd(ebs, ob, stk)
  | If(e_cnd, e_thn, e_els) =>
    doEv(e_cnd, consCtx(If1(((), e_cnd.ann.sourceLocation), e_thn, e_els), stk))
  | Yield(e) => doEv(e, consCtx(Yield1((), e.ann.sourceLocation), stk))
  }
and transitionLetrec = (_ann, _xes: list<bind<printAnn>>, _b: block<printAnn>, _stk: stack) => {
  raiseRuntimeError(AnyError("letrec is no longer supported"))
}
and transitionLet = (xvs, xes: list<bind<printAnn>>, b, stk: stack) => {
  switch xes {
  | list{} => {
      let xvs = xvs->List.reverse->List.toArray
      let xs = xvs->Array.map(((x, _v)) => x)
      let env = extend(current_env(stk), Array.concat(xs, xsOfBlock(b) |> List.toArray))
      xvs->Array.forEach(((x, v)) => {
        doSet(env, x, rmSrcLoc(v))
      })
      Continuing(entering(Let, b, env, stk))
    }

  | list{{it: (x, e)}, ...xes} =>
    doEv(e, consCtx(Let1(xvs, (x, ((), e.ann.sourceLocation)), xes, b), stk))
  }
}
and transitionBlock = ({it: b, ann}: block<printAnn>, isGen, env: environment, stk: stack) => {
  switch b {
  | BRet(exp) =>
    doEv(
      exp,
      add_pile(
        {ctx: new_pile({isGen, base: {it: BdyRet((), exp.ann.sourceLocation), ann}}), env},
        stk,
      ),
    )
  | BCons(t, b) =>
    switch t.it {
    | Exp(exp) => {
        Js.Console.log(exp.ann.sourceLocation)
        doEv(
          exp,
          add_pile(
            {ctx: new_pile({isGen, base: {ann, it: BdyExp(((), exp.ann.sourceLocation), b)}}), env},
            stk,
          ),
        )
      }
    | Def({it: Var(x, e0)}) => {
        let exp = e0
        doEv(
          exp,
          add_pile(
            {
              ctx: new_pile({isGen, base: {ann, it: BdyDef(x, ((), exp.ann.sourceLocation), b)}}),
              env,
            },
            stk,
          ),
        )
      }
    | Def({it: Fun(f, xs, fb)}) => {
        let exp = {it: Lam(xs, fb), ann: t.ann}
        doEv(
          exp,
          add_pile(
            {
              ctx: new_pile({isGen, base: {ann, it: BdyDef(f, ((), exp.ann.sourceLocation), b)}}),
              env,
            },
            stk,
          ),
        )
      }
    | Def({it: GFun(f, xs, fb)}) => {
        let exp = {it: GLam(xs, fb), ann: t.ann}
        doEv(
          exp,
          add_pile(
            {
              ctx: new_pile({isGen, base: {ann, it: BdyDef(f, ((), exp.ann.sourceLocation), b)}}),
              env,
            },
            stk,
          ),
        )
      }
    }
  }
}
and transitionPrg = (p, env: environment) => {
  switch p.it {
  | PNil => Terminated(Tm)
  | PCons({it: Def({it: Var(x, e0)})}, pRest) =>
    let exp = e0
    doEv(
      exp,
      new_pile({
        env,
        ctx: new_pile({ann: p.ann, it: PDef(x, ((), exp.ann.sourceLocation), pRest)}),
      }),
    )
  | PCons({ann, it: Def({it: Fun(f, xs, b)})}, pRest) =>
    let exp = {it: Lam(xs, b), ann}
    doEv(
      exp,
      new_pile({
        env,
        ctx: new_pile({ann: p.ann, it: PDef(f, ((), exp.ann.sourceLocation), pRest)}),
      }),
    )
  | PCons({ann, it: Def({it: GFun(f, xs, b)})}, pRest) =>
    let exp = {it: GLam(xs, b), ann}
    doEv(
      exp,
      new_pile({
        env,
        ctx: new_pile({
          ann: p.ann,
          it: PDef(f, ((), exp.ann.sourceLocation), pRest),
        }),
      }),
    )
  | PCons({it: Exp(exp)}, pRest) =>
    doEv(
      exp,
      new_pile({
        env,
        ctx: new_pile({
          ann: p.ann,
          it: PExp(((), exp.ann.sourceLocation), pRest),
        }),
      }),
    )
  }
}
and transitionCnd = (ebs, ob, stk: stack) => {
  switch ebs {
  | list{} =>
    switch ob {
    | None => return(Con(Uni), stk)
    | Some(b) => doBlk(b, None, stk)
    }
  | list{(e, b), ...ebs} => {
      let exp = e
      doEv(exp, consCtx(Cnd1(((), e.ann.sourceLocation), b, ebs, ob), stk))
    }
  }
}
and transitionBgn = (es, e, stk: stack) => {
  switch es {
  | list{} => doEv(e, stk)
  | list{e0, ...es} => doEv(e0, consCtx(Bgn1(((), e0.ann.sourceLocation), es, e), stk))
  }
}
// and doLoop = (e, b, exp, stk: stack) => {
//   let e1: expression<printAnn> = e
//   let b1: block = extend_block(b, exp)
//   let b2: block = (list{}, annotate((Con(Uni): expression), exp.ann.begin, exp.ann.end))
//   let e = annotate(Cnd(list{(e1, b1)}, Some(b2)), exp.ann.begin, exp.ann.end)
//   doEv(e, stk)
// }
and doAppPrm = (p, vs, stk): state => {
  delta(p, vs, stk)
}
and doApp = (v, vs, stk): state => {
  switch asFun(v) {
  | {isGen, xs, body: b, env} =>
    if Js.Array.length(xs) == Js.List.length(vs) {
      let env = extend(env, Array.concat(xs, xsOfBlock(b)->List.toArray))

      {
        xs |> Js.Array.forEachi((x, i) => {
          doSet(env, x, Js.List.nth(vs, i) |> Js.Option.getExn)
        })
      }

      if isGen {
        let id = newHavId()
        let v = VGen({id, status: ref(Fresh(b, env))})
        allHavs := list{v, ...allHavs.contents}
        Continuing(Returning(v, stk))
        // return(v, stk)
      } else {
        Continuing(entering(App, b, env, stk))
      }
    } else {
      raise(RuntimeError(ArityMismatch(Exactly(Js.Array.length(xs)), Js.List.length(vs))))
    }
  }
}
and doPrint = (v, stk): state => {
  let v = outputletOfValue(v)
  printStdout(v)
  return(Con(Uni), stk)
}
and entering = (entrance, b, env, stk) => {
  let stk = switch stk {
  // tail-call optimization
  | {
      topping: list{
        {ctx: {topping: list{}, base: {isGen: None, base: {it: BdyRet(_)}}}},
        ...topping,
      },
      base,
    } => {topping, base}
  | stk => stk
  }
  Entering(entrance, b, env, stk)
}
and doEntering = (b: block<printAnn>, env, stk): state => {
  transitionBlock(b, None, env, stk)
}
and transitionAppPrm = (
  f: primitive,
  vs: list<(value, sourceLocation)>,
  es: list<expression<printAnn>>,
  stk: stack,
) => {
  switch es {
  | list{} => {
      let vs = List.reverse(vs)
      doAppPrm(f, List.map(vs, rmSrcLoc), stk)
    }

  | list{e, ...es} =>
    let exp = e
    doEv(exp, consCtx(AppPrm1(f, vs, ((), exp.ann.sourceLocation), es), stk))
  }
}
and transitionApp = (
  f: (value, sourceLocation),
  vs: list<(value, sourceLocation)>,
  es: list<expression<printAnn>>,
  stk: stack,
) => {
  switch es {
  | list{} => {
      let vs = List.reverse(vs)
      let f = rmSrcLoc(f)
      let vs = List.map(vs, rmSrcLoc)
      switch f {
      | _ => Continuing(Reducing(Applying(f, vs), stk))
      }
    }

  | list{e, ...es} =>
    let exp = e
    doEv(exp, consCtx(App2(f, vs, ((), exp.ann.sourceLocation), es), stk))
  }
}
and doBlk = (b: block<printAnn>, isGen, stk: stack): state => {
  let xs = xsOfBlock(b)->List.toArray
  let env = current_env(stk)
  let env = if Array.length(xs) == 0 {
    env
  } else {
    extend(env, xs)
  }
  let stk = switch stk {
  // tail-call optimization
  | {
      topping: list{
        {ctx: {topping: list{}, base: {isGen: None, base: {it: BdyRet(_)}}}, env: _},
        ...topping,
      },
      base,
    } => {
      topping,
      base,
    }
  | stk => stk
  }
  transitionBlock(b, isGen, env, stk)
}

let load = (program: program<printAnn>, randomSeed: string, p: bool) => {
  // initialize all global things
  allEnvs := list{}
  allHavs := list{}
  stdout := list{}
  printTopLevel := p
  randomInt := makeRandomInt(randomSeed)

  // now let's get started
  let xs = program |> SMoL.xsOfProgram |> List.toArray
  try {
    let env = makeTopLevel(initialEnv, xs)
    transitionPrg(program, env)
  } catch {
  | RuntimeError(err) => Terminated(Err(err))
  }
}

let doNext = (generator, stk) => {
  let {status: r} = generator
  switch r.contents {
  | Fresh(b, env) => {
      r := Running
      transitionBlock(b, Some(generator), env, stk)
    }
  | Suspended({topping, base}, env) => {
      r := Running
      return(Con(Uni), add_pile({ctx: {topping, base: {isGen: Some(generator), base}}, env}, stk))
    }
  | Running => raise(RuntimeError(AnyError("This generator is already running.")))

  | Done => raise(RuntimeError(AnyError("This generator is done.")))
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
      | Nexting(gen) => doNext(gen, stk)
      | VecSetting(v, i, e) => doVecSet(v, i, e, stk)
      | Applying(f, vs) => doApp(f, vs, stk)
      | Printing(v) => doPrint(v, stk)
      | Yielding(v) => yield(v, stk)
      }
    }
  } catch {
  | RuntimeError(err) => Terminated(Err(err))
  }
}
