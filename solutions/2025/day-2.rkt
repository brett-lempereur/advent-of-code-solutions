;;;
;;; Day 2: Gift Shop
;;;

#lang racket/base

(require racket/generator)
(require racket/port)
(require racket/set)
(require racket/string)

(require advent)

;;
;; Parsing
;;

;; Return a generator that yields ranges as cons-cells.
(define (in-ranges in)
  (define input (string-trim (port->string in)))
  (define chunks (string-split input ","))
  (in-generator
   (for ([chunk chunks])
     (let ([vals (string-split chunk "-")])
       (yield
        (cons (string->number (car vals))
              (string->number (cadr vals))))))))

;;
;; Conditions
;;

;; Return a generator that yields the possible strides for the given
;; string.
(define (in-strides s)
  (define p (/ (string-length s) 2))
  (in-generator
   (for ([i (in-inclusive-range 1 (floor p))])
     (when (integer? (/ (string-length s) i))
       (yield i)))))

;; Hold if the given string is of even length.
(define (has-even-length? s)
  (= (modulo (string-length s) 2) 0))

;; Hold if the given string does not start with a zero.
(define (does-not-start-with-zero? s)
  (not (eq? (string-ref s 0) #\0)))

;; Hold if the given string is symmetrical about its midpoint.
(define (is-symmetrical? s)
  (define p (/ (string-length s) 2))
  (equal? (substring s 0 p) (substring s p)))

;; Hold if the given string has a repeating pattern.
(define (has-repeating-pattern? s)
  (for/or ([stride (in-strides s)])
    (let ([chunks (for/set ([i (in-range 0 (/ (string-length s) stride))])
                    (substring s (* stride i) (+ (* stride i) stride)))])
      (= (set-count chunks) 1))))

;;
;; Processing
;;

;; Return a generator that yields all possible values as strings.
(define (in-values in)
  (in-generator
   (for ([item (in-ranges in)])
     (for ([value (in-inclusive-range (car item) (cdr item))])
       (yield (number->string value))))))

;; Return a generator that yields valid identifiers from a sequence
;; of strings.
(define (in-valid-values in . conditions)
  (in-generator
   (for ([value (in-values in)])
     (when (for/and ([c conditions]) (c value))
       (yield value)))))

;;
;; Solutions
;;

;; What do you get if you add up all of the invalid values?
(define (part-one)
  (define in (puzzle-input-port 2025 2))
  (for/sum ([value (in-valid-values in has-even-length? is-symmetrical?)])
    (string->number value)))

;; With only the repeating pattern rule, what is the sum of the invalid
;; values?
(define (part-two)
  (define in (puzzle-input-port 2025 2))
  (for/sum ([value (in-valid-values in has-repeating-pattern?)])
    (string->number value)))
