;;;
;;; Day 5: Cafeteria
;;;

#lang racket/base

(require racket/generator)
(require racket/match)
(require racket/port)

(require advent)

;;;
;;; Data structures
;;;

;; A segment.
(struct segment (p q) #:transparent)

;; Holds if a segment contains a value.
(define (segment-contains? s n)
  (match-define (segment p q) s)
  (and (>= n p) (<= n q)))

;; Holds if two segments overlap.
(define (segments-overlap? p q)
  (match-define (segment a b) p)
  (match-define (segment c d) q)
  (and (<= a d) (<= c b)))

;; Merge two segments.
(define (merge p q)
  (match-define (segment a b) p)
  (match-define (segment c d) q)
  (segment (min a c) (max b d)))

;; Return the length of a segment.
(define (segment-length s)
  (match-define (segment p q) s)
  (+ 1 (- q p)))

;;;
;;; Parsing
;;;

;; Segment and item regular expression patterns.
(define segment-re #px"^(\\d+)-(\\d+)$")
(define item-re #px"^(\\d+)$")

;; Parse the segments from an inventory.
(define (parse-segments in)
  (define lines (port->lines in))
  (define matches (filter (λ (v) v) (map (λ (s) (regexp-match segment-re s)) lines)))
  (for/list ([m matches])
    (apply segment (map string->number (cdr m)))))

;; Return a generator that yields items from an inventory.
(define (in-items in)
  (in-generator
   (for ([line (in-lines in)])
     (let ([m (regexp-match item-re line)])
       (when m
         (yield (string->number (cadr m))))))))

;;;
;;; Searching
;;;

;; Holds if any segment contains the given item.
(define (is-in-segment? segments item)
  (for/or ([s segments])
    (segment-contains? s item)))

;; Reduce a list of segments to the smallest possible set of
;; non-overlapping segments.
(define (reduce-segments segments)
  (define sorted (sort segments <= #:key segment-p))
  (for/fold ([v (car sorted)]) ([s (cdr sorted)])
    (if (segments-overlap? s (car v))
        (cons (merge (car v) s) (cdr v))
        (cons s v))))

;;;
;;; Solution
;;;

;; The list of segments.
(define input-segments (parse-segments (puzzle-input-port 2025 5)))

;; How many of the available ingredient IDs are fresh?
(define (part-one [in (puzzle-input-port 2025 5)])
  (for/sum ([item (in-items in)])
    (if (is-in-segment? input-segments item) 1 0)))

;; How many ingredient IDs are considered to be fresh according to the
;; fresh ingredient ID ranges?
(define (part-two)
  (define reduced (reduce-segments input-segments))
  (for/sum ([segment reduced])
    (segment-length segment)))
