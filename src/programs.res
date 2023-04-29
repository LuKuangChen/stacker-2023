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

let program_ctr1 = `(deffun (make-counter)
  (defvar n 0)
  (deffun (inc)
    (set! n (+ n 1))
    n)
  inc)
(defvar f (make-counter))
(f)
(f)`

let program_ctr2 = `(defvar f
  (let ([n 0])
    (lambda ()
      (set! n (+ n 1))
      n)))
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

let program_object = `(defvar mk-o-static
  (let ([counter 0])
    (lambda (amount)
      (set! counter (+ 1 counter))
      (lambda (m)
        (cond
          [(eq? m "inc")
           (lambda (n) (set! amount (+ amount n)))]
          [(eq? m "dec")
           (lambda (n) (set! amount (- amount n)))]
          [(eq? m "get")
           (lambda () amount)]
          [(eq? m "count")
           counter]
          [else
           (error "no such member")])))))

(defvar o1 (mk-o-static 1000))
(defvar o2 (mk-o-static 0))
(o1 "count")
(o2 "count")`
