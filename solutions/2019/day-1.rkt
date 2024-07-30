;;;
;;; The Tyranny of the Rocket Equation
;;;

#lang racket/base

(require racket/generator)
(require racket/sequence)

(require advent)

(provide part-one part-two)

;;;
;;; Parsing
;;;

;; Parses a port as a sequence of numbers.
(define (in-numbers port)
  (in-generator
   (for [(line (in-lines port))]
     (yield (string->number line)))))

;;;
;;; Computation
;;;

;; Returns the amount of fuel required for a mass.
(define (get-fuel mass)
  (let [(result (- (floor (/ mass 3)) 2))]
    (if (< result 0) 0 result)))

;; Recursively computes the inclusive amount of fuel required for a mass.
(define (get-inclusive-fuel mass)
  (let loop ([total-fuel 0] [mass mass])
    (let ([fuel (get-fuel mass)])
      (if (= fuel 0)
          total-fuel
          (loop (+ total-fuel fuel) fuel)))))

;;;
;;; Solutions
;;;

(define (part-one)
  (define masses (in-numbers (puzzle-input-port 2019 1)))
  (define fuels (sequence-map get-fuel masses))
  (sequence-fold + 0 fuels))

(define (part-two)
  (define masses (in-numbers (puzzle-input-port 2019 1)))
  (define fuels (sequence-map get-inclusive-fuel masses))
  (sequence-fold + 0 fuels))
