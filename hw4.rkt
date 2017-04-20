#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1

; SICP 2.7 - Define upper-bound and lower-bound

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (cdr interval)
)

(define (lower-bound interval)
  (car interval)
)

; SICP 2.8 - Define sub-interval

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y)))
)

; SICP 2.10 - Modify div-interval

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "The number cannot be divided by zero.")
      (mul-interval x
                    (make-interval (/ 1 (upper-bound y))
                                   (/ 1 (lower-bound y))))))

;SICP 2.12 - Define make-center-percent and percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(define (percent i)
  (* 100 (/ (width i) (center i))))
(define (make-center-percent c tol)
  (make-center-width c (* (/ tol 100) c)))

; SICP 2.17 - Define last-pair

(define (last-pair lst)
  (cond ((null? list) (error "The list is empty."))
        ((null? (cdr lst)) lst)
        (else (last-pair (cdr lst))))
)

; SICP 2.20 - Define same-parity

(define (same-parity a . b)
  (cond ((null? b) (list a))
        ((even? a) (cons a (filter even? b)))
        ((odd? a) (cons a (filter odd? b))))
)

; SICP 2.22 - Write your explanation in the comment block:

#|
In the first case, the iteration process will first output the last item in the list,
and then attach the rest of list after the first item. So the order is reversed.
In the second case, since racket regards the output as a pait and "answer" as a list,
it will put the empty item in the car of the last item it squares and result in the
weird list.
|#

; Exercise 2 - Define my-substitute

(define (substitute lst old new)
  (cond ((null? lst) '())
        ((and (not (list? (car lst))) (equal? (car lst) old))
         (cons new (substitute (cdr lst) old new)))
        ((and (not (list? (car lst))) (not (equal? (car lst) old)))
         (cons (car lst) (substitute (cdr lst) old new)))
        (else (cons (substitute (car lst) old new)
                    (substitute (cdr lst) old new))))
)

; Exercise 3 - Define my-substitute2

(define (switch wd lst)
  (cond ((empty? wd) "")
        ((null? lst) wd)
        ((equal? wd (caar lst)) (cadar lst))
        (else (switch wd (cdr lst)))))

(define (substitute2 lst old new)
    (cond ((null? lst) `())
          ((not (list? lst)) (switch lst (map list old new)))
          (else (cons (substitute2 (car lst) old new)
                      (substitute2 (cdr lst) old new)))))