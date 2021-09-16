(define x 1)

(define (f) (+ 0 x))
(define (g) x)
(define y x)

(begin
    (define x 10)
    1
)

(define (h) (begin
    (define x 100)
    1
))
(h)

(print (= 21 (+ (f) (g) y)))
