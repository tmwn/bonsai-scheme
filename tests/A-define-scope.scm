(define x 1)

(define (f) (+ 1 x))
(define (g) x)

(begin
    (define x 3)
    1
)

(define (h) (begin
    (define x 9)
    1
))
(h)

(print (= 7 (+ (f) (g))))
