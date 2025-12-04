;;;
;;; Day 4: Printing Department
;;;

#lang racket/base

(require racket/generator)
(require racket/match)
(require racket/sequence)

(require advent)
(require advent/grid)

;;;
;;; Parsing
;;;

;; Cell types.
(define space 'space)
(define roll 'roll)

;; Converts a character into the cell.
(define (char->cell c) (match c [#\. space] [#\@ roll]))

;; Parse a port as a warehouse map grid.
(define (parse in) (port->grid in char->cell))

;;;
;;; Searching
;;;

;; The list of all cardinal and ordinal neighbours.
(define all-neighbours (append ordinal-neighbours cardinal-neighbours))

;; Return the number of rolls adjacent to a point.
(define (adjacent-roll-count g p)
  (sequence-length (in-neighboursf g p (λ (c) (eq? c roll)) all-neighbours)))

;; Return a generator that yields positions of rolls that have fewer than
;; k adjacent rolls.
(define (in-accessible-rolls g [k 4])
  (define rolls (grid-pointf g (λ (c) (eq? c roll))))
  (in-generator
   (for ([r rolls])
     (when (< (adjacent-roll-count g r) k)
       (yield r)))))

;;;
;;; Cleanup
;;;

;; Iteratively cleanup a warehouse by removing all accessible rolls.
(define (cleanup-warehouse g [k 4])
  (define n (remove-accessible g k))
  (if (equal? g n) g (cleanup-warehouse n k)))

;; Remove all accessible rolls from the warehouse.
(define (remove-accessible g [k 4])
  (for/fold ([g g]) ([r (in-accessible-rolls g k)])
    (grid-set g r space)))

;; Return the number of rolls removed between two states of a warehouse.
(define (count-removed-rolls a b)
  (define a-count (length (grid-pointf a (λ (c) (eq? c roll)))))
  (define b-count (length (grid-pointf b (λ (c) (eq? c roll)))))
  (- a-count b-count))

;;;
;;; Solutions
;;;

;; How many rolls of paper can be accessed by a forklift?
(define (part-one [in (puzzle-input-port 2025 4)])
  (define g (parse in))
  (sequence-length (in-accessible-rolls g 4)))

;; How many rolls of paper in total can be removed by the Elves and their
;; forklifts?
(define (part-two [in (puzzle-input-port 2025 4)])
  (define g (parse in))
  (define n (cleanup-warehouse g 4))
  (count-removed-rolls g n))
