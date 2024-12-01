;;;
;;; Day 2: Red-Nosed Reports
;;;

#lang racket/base

(require racket/string)
(require racket/treelist)

(require advent)

;;;
;;; Parsing
;;;

(define (parse-line line)
  (map string->number (string-split line " ")))

(define (parse-reports port)
  (for/list ([line (in-lines port)])
    (list->treelist (parse-line line))))

;;;
;;; Properties
;;;

(define (is-strictly-increasing? report)
  (for/and ([a report] [b (treelist-rest report)])
    (< a b)))

(define (is-strictly-decreasing? report)
  (for/and ([a report] [b (treelist-rest report)])
    (> a b)))

(define (minimum-delta report)
  (for/fold ([Δ +inf.0]) ([a report] [b (treelist-rest report)])
    (let ([δ (abs (- a b))])
      (min δ Δ))))

(define (maximum-delta report)
  (for/fold ([Δ -inf.0]) ([a report] [b (treelist-rest report)])
    (let ([δ (abs (- a b))])
      (max δ Δ))))

;;;
;;; Dampening
;;;

(define (dampen report)
  (for/list ([i (in-range 0 (treelist-length report))])
    (treelist-delete report i)))

;;;
;;; Solutions
;;;

(define (is-valid-report? report)
  (and (or (is-strictly-increasing? report) (is-strictly-decreasing? report))
       (<= (maximum-delta report) 3)
       (>= (minimum-delta report) 1)))

(define (is-valid-report-dampened? report)
  (or (is-valid-report? report)
      (for/or ([dampened (dampen report)]) (is-valid-report? dampened))))

(define (part-one [input (puzzle-input-port 2024 2)])
  (define reports (parse-reports input))
  (define valid (filter is-valid-report? reports))
  (length valid))

(define (part-two [input (puzzle-input-port 2024 2)])
  (define reports (parse-reports input))
  (define valid (filter is-valid-report-dampened? reports))
  (length valid))
