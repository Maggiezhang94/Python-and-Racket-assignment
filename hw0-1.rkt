#lang racket

;; A line starting with a semicolon is a "comment".  You write
;; comments in order to explain in English what your code does, and
;; Racket knows to ignore comments since they aren't part of the
;; program.

;; This tells Racket that you want to use words and sentences (which
;; are disabled by default).
(require (planet dyoo/simply-scheme))

;; This tells Racket that it should "know" about all the functions you
;; define in this file.  (Don't worry about this for now.)
(provide (all-defined-out))

;; Exercise 0 Introduce yourself

#|

This is a comment that spans multiple lines.

1) What is your name?
Maggie Zhang
2) What is your major?
Statistics
3) Are you a returning student? (i.e. Did you take 61AS last semester?)
No
4) What made you to take 61AS?
Learn more about the basics of computer programs 
5) Tell us interesting things about yourself.
I love frenchies
|#

;; Make a followup on the "Hello World!" post on Piazza introducing yourself.


;; Exercise 1 - Define sum-of-squares
(define (sum-of-squares x y) (+ (sqr x) (sqr y)))


;; Exercise 2a - Define can-drive
(define (can-drive x)
  (if (< x 16)
    '(Not yet)
    '(Good to go)))


;; Exercise 2b - Define fizzbuzz
(define (fizzbuzz x)
  (if (number? x)
    (cond ((and (= (remainder x 3) 0) (= (remainder x 5) 0)) 'fizzbuzz)
          ((= (remainder x 3) 0) 'fizz)
          ((= (remainder x 5) 0) 'buzz)
          (else x))
    (error "input is not a number")))

;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|
Because there were no chickens around.


|#

;; Exercise 4 - new-if vs if

#|
“new-if” doesn’t behave like “if” because “if” is a special form of
function that always evaluates its first argument and proceeds to the
next argument if the first argument is false. But “new-if” is a normal
procedure that evaluates every arguments before passing them to the
function, thus “new-if” evaluates infinite loop in the first example and
1 divided by 0 in the second example.
|#
