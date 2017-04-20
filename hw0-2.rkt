#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
 (cond ((> (quotient secs 86400) 0)
        (se (quotient secs 86400) 'days (describe-time (remainder secs 86400))))
       ((> (quotient secs 3600) 0)
        (se (quotient secs 3600) 'hours (describe-time (remainder secs 3600))))
       ((> (quotient secs 60) 0)
        (se (quotient secs 60) 'minutes (describe-time (remainder secs 60))))
       (else (se secs 'seconds)))
)

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  (cond ((empty? sent)
         '(The sentence is empty))
        ((equal? wd (first sent))
         (bf sent))
        (else (se (first sent) (remove-once wd (bf sent)))))
)
         
; Exercise 3 - Define differences
(define (differences nums)
  (cond ((empty? nums)
         '(The numbers are empty))
        ((equal? (count nums) 1)
         '())
        (else (se (- (item 2 nums) (first nums))
                  (differences (bf nums)))))
)

; Exercise 4 - Define location
(define (location small big)
  (cond ((empty? big) '(The sentence is empty))
        ((not (member? small big)) #f)
        ((equal? small (first big)) 1)
        (else (+ 1 (location small (bf big)))))
)
      
; Exercise 5 - Define initials
(define (initials sent)
  (if (empty? sent) '()
      (se (first (first sent)) (initials (bf sent))))
)

; Exercise 6 - Define copies
(define (copies num wd)
  (if (equal? num 0) '()
      (se wd (copies (- num 1) wd)))
)

; Exercise 7 - Define gpa
(define (base-grade grade)
  (cond ((equal? (first grade) 'A) 4)
        ((equal? (first grade) 'B) 3)
        ((equal? (first grade) 'C) 2)
        ((equal? (first grade) 'D) 1)
        ((equal? (first grade) 'F) 0))
)

(define (grade-modifier grade)
  (cond ((empty? (bf grade)) 0)
        ((equal? (bf grade) '-) -0.33)
        (0.33))
)

(define (gpa grades)
  (define (gpa-sum grades)
    (define (grade-add grade)
      (+ (base-grade grade)
         (grade-modifier grade))
      )
    (if (empty? grades) 0
        (+ (grade-add (first grades)) (gpa-sum (bf grades))))
    )
  (+ (real->decimal-string (/ (gpa-sum grades) (count grades)) 2) 0)
)

; Exercise 8 - Define repeat-words
(define (repeat-words sent)
  (if (empty? sent) '()
      (if (number? (first sent))
         (se (copies (first sent) (first (bf sent)))
             (repeat-words (bf (bf sent))))
         (se (first sent) (repeat-words (bf sent)))))
)

; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  (cond ((and (empty? sent1) (empty? sent2)) #t)
        ((not (= (count sent1) (count sent2)))
         #f)
        ((not (= (count (first sent1)) (count (first sent2))))
         #f)
        (else (same-shape? (bf sent1) (bf sent2))))
)