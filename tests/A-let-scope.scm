(define a 4)
(define b 4)
(define c 4)

(letrec ((a 2)) (set! a 3))
(let*   ((b 2)) (set! b 3))
(let    ((c 2)) (set! c 3))

(letrec ((a (begin
  (set! a 1)
  a
))) a)

(print (and
    (= 4 a)
    (= 4 b)
    (= 4 c)
    )
)
