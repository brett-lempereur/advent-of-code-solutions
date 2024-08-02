;;;
;;; Universal Orbit Map
;;;

#lang racket/base

(require racket/match)
(require racket/string)

(require advent)
(require advent/graph)

(provide part-one part-two)

;;;
;;; Parsing
;;;

(define (parse-graph port)
  (define graph (make-graph))
  (for/fold ([g (make-graph)]) ([line (in-lines port)])
    (match-let ([(list u v) (string-split line ")")])
      (graph-add-edge g u v))))

;;;
;;; Solutions
;;;

;; Compute the total number of direct and indirect orbits.
(define (part-one [port (puzzle-input-port 2019 6)])
  (define graph (parse-graph port))
  (match-define-values (distances previous) (graph-dijkstra graph "COM"))
  (foldl + 0 (hash-values distances)))

;; Compute the minimum number of orbital transfers to reach santa.
(define (part-two [port (puzzle-input-port 2019 6)])
  (define graph (parse-graph port))
  (define path (graph-a* graph "YOU" "SAN"))
  (- (length path) 2))
