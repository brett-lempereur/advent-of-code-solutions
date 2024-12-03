;;;
;;; Day 3: Mull It Over
;;;

#lang racket/base

(require racket/port)

(require advent)

(provide part-one part-two)

;;;
;;; Parsing
;;;

(define segment-re #px"don't\\(\\).*?(?:$|do\\(\\))")
(define mul-re #px"mul\\((\\d+),(\\d+)\\)")

(define (parse-expressions input)
  (for/list ([expr (regexp-match* mul-re (port->string input))])
    (map string->number (cdr (regexp-match mul-re expr)))))

(define (parse-segments input)
  (parse-expressions
   (open-input-string
    (regexp-replace* segment-re (port->string input) ""))))

;;;
;;; Solutions
;;;

(define (evaluate-expressions expressions)
  (foldl + 0 (map (λ (l) (apply * l)) expressions)))

(define (part-one [input (puzzle-input-port 2024 3)])
  (evaluate-expressions (parse-expressions input)))

(define (part-two [input (puzzle-input-port 2024 3)])
  (evaluate-expressions (parse-segments input)))
