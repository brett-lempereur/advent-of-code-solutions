;;;
;;; Secure Container
;;;

#lang racket/base

(require racket/list)

(provide part-one part-two)

;;;
;;; Iteration
;;;

;; Applies a binary predicate to pairs of numbers in a set of digits and
;; returns the number of times the predicate holds.
(define (count-pairs-where predicate digits)
  (for/sum ([p digits] [q (cdr digits)])
    (if (predicate p q) 1 0)))

;;;
;;; Predicates
;;;

;; Holds if a sequence of digits are non-decreasing.
(define (non-decreasing? digits)
  (= (count-pairs-where > digits) 0))

;; Holds if at least two sequential digits are identical.
(define (any-same-digits? digits)
  (> (count-pairs-where = digits) 0))

;; Holds if at least one pair is isolated.
(define (isolated-pair? digits)
  (let loop ([n 0] [c 0] [digits digits])
    (if (empty? digits) (= c 1)
        (let ([is-same (= (car digits) n)]
              [is-at-limit (= c 1)])
          (cond
            [(and (not is-same) is-at-limit) #t]
            [is-same (loop (car digits) (+ c 1) (cdr digits))]
            [else (loop (car digits) 0 (cdr digits))])))))

;;;
;;; Conversion
;;;

;; Convert a number to a list of digits.
(define (number->digits number)
  (let loop ([digits (list)] [number number])
    (if (= number 0)
        digits
        (loop (cons (modulo number 10) digits)
              (floor (/ number 10))))))

;;;
;;; Solutions.
;;;

;; Compute how many numbers meet the relaxed criteria.
(define (part-one [low 138241] [high 674034])
  (for/sum ([number (in-inclusive-range low high)])
    (let ([digits (number->digits number)])
      (if (and (non-decreasing? digits) (any-same-digits? digits)) 1 0))))

;; Compute how many numbers meet the strict criteria.
(define (part-two [low 138241] [high 674034])
  (for/sum ([number (in-inclusive-range low high)])
    (let ([digits (number->digits number)])
      (if (and (non-decreasing? digits) (isolated-pair? digits)) 1 0))))
