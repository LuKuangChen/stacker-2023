# Stacker

This tool presents how programs work.
It focuses on presenting on the (call) stack, environments, and the heap.
Such program must be written in SMoL, an educational language that
support many language features commonly found in modern programming languages.
SMoL has *three* surface syntaxes: Python-like, JavaScript-like, and Racket-like.
Although programs must be written in the Racket-like syntax, this tool is able to *present* programs in two other, possibly more favorable syntaxes.

## What is SMoL?

SMoL = *Standard Model of Languages* It includes (mutable) variables, mutable vectors (i.e., arrays), and first-class function.

### Supported grammar

```
t ::= d
    | e
d ::= (defvar x e)
    | (deffun (f x ...) body)
e ::= c
    | x
    | (lambda (x ...) body)
    | (let ([x e] ...) body)
    | (begin e ... e)
    | (set! x e)
    | (if e e e)
    | (cond [e body] ... [else body])
    | (cond [e body] ...)
    | (e e ...)
body    ::= t ... e
program ::= t ...
```

### Supported primitive operators

| **Operators**         | **Meaning**                              |
| --------------------- | ---------------------------------------- |
| `+` `-` `*` `/`       | Arithmetic Operators                     |
| `=` `<` `>` `<=` `>=` | Number comparison                        |
| `++`                  | String concatenation                     |
| `vec`                 | Create a (mutable) vector (a.k.a. array) |
| `vec-ref`             | Look up a vector element                 |
| `vec-set!`            | Replace a vector element                 |
| `vec-len`             | Get the length of a vector               |
| `eq?`                 | Pointer equality                         |


## Limitations

### Potential Problems: limitations that might exist, but we are not sure

Programs that can be presented in Python-like syntax might or might nor have a Python-equivalent semantics.

Programs that can be presented in JavaScript-like syntax might or might nor have a JavaScript-equivalent semantics.

### To-do: limitations that we plan to remove

Parsing errors are not reported with source code locations.

Keyboard shortcut is unstable.

ReScript-React registers tons of unused event handlers to the root element.
This might raise accessibility problem.

Add list-processing higher-order functions.

### Know Issues: limitations that we don't plan to (or simply can't) resolve

Some programs can't be presented in Python-syntax. This restriction is mostly caused by the facts that Python have no let expressions, that Python's lambda only allow exactly one expression as its body.

Python uses the same syntax for variable definition and variable mutation. Although this design might work well most of the time, there are programs, especially tricky programs, where disambiguation is needed. Python disambiguates with `local` and `global` keywords. We don't intend to support this Python characteristics and use `:=` for variable mutations and `=` for variable definitions.

JavaScript similarly has a restriction -- there is no let expression in JavaScript.

## Development

### For every fresh clone

```
npm install
```

This will install dependency packages.

### For every development

```sh
npm run rescript
```

This will start a ReScript program that keeps `.res` programs and `.js` programs in sync.


```sh
npm run webpack
```

This will run a Webpack program that keeps `.css`, `.js`, etc. and the final webpage in sync.

## File Types

- `Xyz.res`: a React component
- `xyz.res`: a module
- `webpack.config.js`: the webpack configuration

## Accessibility Consideration

### No ARIA?

https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA

> "No ARIA is better than bad ARIA."

### No tooltips?

https://inclusive-components.design/tooltips-toggletips/

> Tooltips are a last resort, where space really is at a premium

## Contributions

This software application is developed by Kuang-Chen Lu (me) and [Shriram Krishnamurthi](https://cs.brown.edu/~sk/).

The design is originally based on the systems designed by Shriram and [John Clements](https://www.brinckerhoff.org/).
The original design can be found in the following publication:

> Clements, John, and Shriram Krishnamurthi. "Towards a Notional Machine for Runtime Stacks and Scope:
When Stacks Don’t Stack Up." Proceedings of the 2022 ACM Conference on
International Computing Education Research-Volume 1. 2022.

During the revision of the design, many people have contributed feedback, notably:

- Students who attended [CSCI1730 in 2022](https://cs.brown.edu/courses/cs173/2022/)
- Kuang-Chen's friends and/or peers:
  [Elijah Rivera](https://www.elijahrivera.com/)
  [Siddhartha Prasad](https://www.siddharthaprasad.com)
  [Yanyan Ren](https://yanyanr.github.io/)
  [Yanru Liao](https://www.linkedin.com/in/yanru-liao-7780b2243/)
- Several audiences of [the Stacker talk](https://youtu.be/y42WZS4spfo) in the 12th
  [RacketCon](https://con.racket-lang.org/), notably
  - [Ben Greenman](https://cs.brown.edu/people/bgreenma/)
  - [Jay McCarthy](https://jeapostrophe.github.io/)
  - [Robert Findler](https://users.cs.northwestern.edu/~robby/)
  - [Ryan Culpepper](https://www.ccs.neu.edu/home/ryanc/)
