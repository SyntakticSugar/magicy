#lang racket
#|

    Racket implementation of the Y Combinator to save 
    doing extensive normalisation in-class. 

    Makes use of Racket's integers, booleans, and branching. 

|#


(displayln "Hello, Racket!")
(displayln "Let's do recursion with the help of the magic Y Combinator.")


; Behold, Haskell Curry's ** Y COMBINATOR ** 
(define (Y f)
  ((lambda (x) (f (lambda (y) ((x x) y))))
   (lambda (x) (f (lambda (y) ((x x) y))))))

; Addition with the Y Combinator controlling the recursion.
(define (SUM-HELPER f)
  (lambda (a)
    (lambda (b)
      (if (zero? b)
          a
          (+ ((f a) (sub1 b)) 
               1)))))

(define SUM (Y SUM-HELPER))

; Multiplication with the Y Combinator controlling the recursion.
(define (MUL-HELPER f)
    (lambda (a)
        (lambda (b)
            (if (zero? b)
                0
                (+ ((f a) (sub1 b))
                   a)))))

(define MUL (Y MUL-HELPER))

; Example: ((MUL 2) 3)

; Unbounded search with the Y combinator controlling the recursion.
(define (MU-HELPER f)
    (lambda (x)
        (if (zero? (- 5 x)) x (f (+ x 1)))))

(define MU (Y MU-HELPER))