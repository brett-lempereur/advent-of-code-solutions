;;;
;;; Day 1: Historian Hysteria
;;;

#lang racket/base

(require advent)

(provide part-one part-two)

;;;
;;; Parsing
;;;

(define line-re #px"^(\\d+)   (\\d+)$")

(define (parse-columns port)
  (for/list ([line (in-lines port)])
    (let ([groups (regexp-match line-re line)])
      (map string->number (cdr groups)))))

(define (column columns i)
  (for/list ([row columns])
    (list-ref row i)))

;;;
;;; Solutions
;;;

(define (part-one [input (puzzle-input-port 2024 1)])
  (define columns (parse-columns input))
  (define (sorted i) (sort (column columns i) <))
  (for/sum ([left (sorted 0)] [right (sorted 1)])
    (abs (- left right))))

(define (part-two [input (puzzle-input-port 2024 1)])
  (define columns (parse-columns input))
  (define right-counts
    (for/fold ([output (hash)]) ([number (column columns 1)])
      (hash-update output number add1 0)))
  (for/sum ([number (column columns 0)])
    (* number (hash-ref right-counts number 0))))
