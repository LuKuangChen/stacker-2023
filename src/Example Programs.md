# Example Programs

## Fibonacci

```smol
(deffun (fib n)
  (cond
    [(<= n 1) 1]
    [else
     (+ (fib (- n 1))
        (fib (- n 2)))]))

(fib 5)
```

## Factorial

```smol
(deffun (fact n)
  (cond
    [(= n 0) 1]
    [else
     (* (fact (- n 1)) n)]))

(fact 5)
```

## The big complicated object example

```smol
(defvar mk-o-static-1
  (let ([counter 0])
    (lambda (amount)
      (set! counter (+ 1 counter))
      (lambda (m)
        (cond
          [(eqv? m "inc")
           (lambda (n) (set! amount (+ amount n)))]
          [(eqv? m "dec")
           (lambda (n) (set! amount (- amount n)))]
          [(eqv? m "get")
           (lambda () amount)]
          [(eqv? m "count")
           counter]
          [else
           (error "no such member")])))))

(defvar o1 (mk-o-static-1 1000))
(defvar o2 (mk-o-static-1 0))
(o1 "count")
(o2 "count")
(o2 "a-nonexisting-member")
```