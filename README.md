# Stacker

## Development

Start the watch-compile

```sh
npm run res:start
```

Build webpack every time you run

```sh
npm run build
```

## Build from the source code

```sh
npx build
```

## Naming conventions

- `Xyz.res`: a React component
- `xyz.res`: a module
- `webpack.config.js`: the webpack configuration

## Contributions

This software application is developed by Kuang-Chen Lu (me) and [Shriram Krishnamurthi](https://cs.brown.edu/~sk/).

The design is originally based on the systems designed by Shriram and [John Clements](https://www.brinckerhoff.org/).
The original design can be found in the following publication:

> Clements, John, and Shriram Krishnamurthi. "Towards a Notional Machine for Runtime Stacks and Scope:
When Stacks Don’t Stack Up." Proceedings of the 2022 ACM Conference on
International Computing Education Research-Volume 1. 2022.

During the revision of the design, numerous people have contributed feedback, notably:

- Students who attended [CSCI1730 in 2022](https://cs.brown.edu/courses/cs173/2022/)
- Kuang-Chen's friends and/or peers:
  [Elijah Rivera](https://www.elijahrivera.com/),
  [Siddhartha Prasad](https://www.siddharthaprasad.com),
  [Yanyan Ren](https://yanyanr.github.io/), and
  [Yanru Liao](https://www.linkedin.com/in/yanru-liao-7780b2243/)
- Several audiences of [the Stacker talk](https://youtu.be/y42WZS4spfo) in the 12th
  [RacketCon](https://con.racket-lang.org/):
  - [Ben Greenman](https://cs.brown.edu/people/bgreenma/)
  - [Jay McCarthy](https://jeapostrophe.github.io/)
  - [Robert Findler](https://users.cs.northwestern.edu/~robby/)
  - [Ryan Culpepper](https://www.ccs.neu.edu/home/ryanc/)
