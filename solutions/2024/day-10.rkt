;;;
;;; Day 10: Hoof It
;;;

#lang racket/base

(require racket/set)

(require advent)
(require advent/grid)

(provide part-one part-two)

;;;
;;; Parsing
;;;

;; Returns the number that is represented by a character.
(define (char->number c)
  (string->number (string c)))

;;;
;;; Navigation
;;;

;; Returns the list of coordinates of trailheads.
(define (trailheads g)
  (grid-pointf g (λ (v) (= v 0))))

;; Returns the set of final trail points that begin at the given position.
(define (reachable g p)
  (define cv (grid-ref g p))
  (if (= cv 9)
      (set p)
      (for/fold ([output (set)])
                ([np (in-neighboursf g p (λ (nv) (= nv (+ 1 cv))))])
        (set-union output (reachable g np)))))

;; Counts the number of distinct paths to the end of a trail.
(define (count-trails g p)
  (define cv (grid-ref g p))
  (if (= cv 9)
      1
      (for/sum ([np (in-neighboursf g p (λ (nv) (= nv (+ 1 cv))))])
        (count-trails g np))))

;;;
;;; Solutions
;;;

(define (part-one [input (puzzle-input-port 2024 10)])
  (define grid (port->grid input char->number))
  (for/sum ([p (trailheads grid)])
    (set-count (reachable grid p))))

(define (part-two [input (puzzle-input-port 2024 10)])
  (define grid (port->grid input char->number))
  (for/sum ([p (trailheads grid)])
    (count-trails grid p)))
