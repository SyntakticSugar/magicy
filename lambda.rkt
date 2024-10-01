#lang racket
#| 
    Module: Recursive Lambda Terms with the Y Combinator
    Author: Robert Culling.
    Status: Experimental.
    Contact: rhsculling at pm dot com

    This module explores recursion in Racket using the Y Combinator, 
    enabling recursion without named functions. 

    Functions implemented:
        - SUM: Recursive addition of two numbers.
        - MUL: Recursive multiplication of two numbers.
        - MU: Unbounded search for the first number equal to 5.
        - bMU: Bounded search for a predicate within a range.
        - DIVIDES?: Checks if one number divides another using bounded search.

    The Y Combinator illustrates key principles of functional programming 
    and lambda calculus, providing a clean approach to recursion.
|#

(displayln "Hello, Racket!")
(displayln "Let's do recursion with the help of the magic Y Combinator.")


; Behold, Haskell B. Curry's ** Y COMBINATOR ** 
(define (Y)
  (lambda (f)
    ((lambda (x) (f (lambda (y) ((x x) y))))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

; Addition helper with zero arguments
(define (SUM-HELPER)
  (lambda (f)
    (lambda (a)
      (lambda (b)
        (if (zero? b)
            a
            (+ ((f a) (sub1 b)) ; Cheating here as no in-built succ. 
               1))))))          ; Using Racket + to compute SUM. 

; Create the recursive SUM function using the Y combinator.
(define SUM ((Y) (SUM-HELPER)))

; Helper function to evaluate the SUM function
(define (EVAL-SUM a b)
  ((SUM a) b))

; Multiplication with the Y Combinator controlling the recursion.
(define (MUL-HELPER)
    (lambda (f)
        (lambda (a)
            (lambda (b)
                (if (zero? b)
                    0
                    (EVAL-SUM ((f a) (sub1 b)) a))))))

; Create the recursive MUL function using the Y combinator. 
(define MUL ((Y) (MUL-HELPER)))

(define (EVAL-MUL a b)
    ((MUL a) b))

; Unbounded search with the Y combinator controlling the recursion.
; In theoretical computer science (recursive function theory) this
; unbound search operator is often denoted "mu".
(define (MU-HELPER)
    (lambda (f)
        (lambda (x)
            (if (zero? (- 5 x)) 
                x 
                (f (+ x 1))))))

(define MU ((Y) (MU-HELPER)))

; Bounded search with the Y combinator controlling the recursion.
(define (bMU-HELPER)
    (lambda (s)
        (lambda (p)
            (lambda (l)
                (lambda (u)
                    (if (> l u)
                        #f
                        (if (p l)
                            #t
                            (((s p) (EVAL-SUM l 1)) u))))))))

(define bMU ((Y) (bMU-HELPER)))

(define (EVAL-bMU p l u)
    (((bMU p) l) u))

; Where one number divides another is determined by a 
; bounded search for solutions to an equation. This means 
; we can use the procs above to implement DIVIDES? 
(define (DIVIDES-HELPER)
    (lambda (n)
        (lambda (d)
            (EVAL-bMU (lambda (k) (= n (EVAL-MUL k d))) 0 n))))

(define (DIVIDES? n d)
    (((DIVIDES-HELPER) n) d))


