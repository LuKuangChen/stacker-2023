# Stacker

## Development

### For every fresh clone

```
npm install
```

This will install dependency packages.

### For every development

```sh
npm run res:start
```

This will start a ReScript program that keeps `.res` programs and `.js` programs in sync.


```sh
npm run wpk:start
```

This will run a Webpack program that keeps `.css`, `.js`, etc. and the final webpage in sync.

## Naming conventions

- `Xyz.res`: a React component
- `xyz.res`: a module
- `webpack.config.js`: the webpack configuration

## Accessibility Consideration

### No ARIA?

https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA

> "No ARIA is better than no ARIA".

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
