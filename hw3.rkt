#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter

(define (fast-expt-iter b n)
  (define (helper b n a)
    (cond ((= n 0) a)
          ((even? n) (helper (square b) (/ n 2) a))
          (else (helper b (- n 1) (* b a)))))
    (helper b n 1)
)

; Exericse 2 - Define phi

(define (phi)
  ; Your code here
  (error "Not yet implemented")
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac n d k)
  (cond ((= k 0) 0)
        (else (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))
)

;; Iterative version
(define (cont-frac-iter n d k)
  (define (helper n d k quotient)
    (if (= k 0)
        (/ (n k) (+ (d k) quotient))
        (helper n d (- k 1) (/ (n k) (+ (d k) quotient)))))
    (helper n d k 0)
)

(define (e k)
  (define (n i) 1)
  (define (d i)
    (cond ((= i 1) 1)
          ((= i 2) 2)
          ((equal? (remainder (+ i 1) 3) 0)
           (* 2 (/ (+ i 1) 3)))
          (1)))
  (real->decimal-string (+ 2 (cont-frac n d k)) 4)
)


; Exercise 4 - Define next-perf
(define (sum-of-factors term a next b)
  (if (> a b)
      0
      (+ (term a) (sum-of-factors (next a) next b))))
(define (next-perf n)
  (if (equal? (sum-of-factors (lambda (helper x)
                                  (if (and (< x n) (equal? (remainder n x) 0)) x
                                      0)) 1 (lambda (x) (+ x 1)) n) n)
      n
      (next-perf (+ n 1)))
)


; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

If the base cases are interchanged, the order of growth in the new procedure would increase and the size of the space
would also increase, because the procedure will always evaluate if (< amount 0) and (empty? kinds-of-coins) before
realizing that (= amount 0).

|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt: the product of b and b^(n-1) is always equal to b^n.

Formula for expt-iter: b^0 = 1

|#
