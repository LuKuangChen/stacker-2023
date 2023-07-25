# SMoL

This document describes the design of SMoL and its design decisions.

```
d ::= (defvar x e)
   |  (deffun (f x ...) body)
   |  (for x e e body)
e ::= c
   |  x
   |  (lambda (x ...) body)
   |  (e e ...)
   |  (set! x e)
   |  (cond [e body] ...)
   |  (cond [e body] ... [else body])
   |  (let ([x e] ...) body)
   |  (while e body)

term ::= d | e

body    ::= t ... e
program ::= t ...
```
