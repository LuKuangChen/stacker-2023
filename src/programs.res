let program_fib = `(deffun (fib n)
  (cond
    [(<= n 1) 1]
    [else
     (+ (fib (- n 1))
        (fib (- n 2)))]))

(fib 0)
(fib 1)
(fib 2)
(fib 3)`

let program_aliasing = `(defvar v1 (vec 1 7 3))
(defvar v2 v1)
(vec-set! v1 0 42)
(vec-ref v2 0)`

let program_ctr = `(defvar f
  (let ([ctr 0])
    (lambda ()
      (set! ctr (+ ctr 1))
      ctr)))

(f)
(f)`

let program_dynscope = `(defvar x 1)
(deffun (addx y)
  (defvar x 2)
  (+ x y))
(+ (addx 0) x)`

let program_circularity = `(defvar v (mvec 2 3))
(vec-set! v 0 v)
v`