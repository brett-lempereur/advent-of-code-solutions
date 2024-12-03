;;;
;;; Day 7: Bridge Repair
;;;

#lang racket/base

(require racket/list)
(require racket/sequence)

(require advent)

(provide part-one part-two)

;;;
;;; Data model
;;;

(struct equation (target numbers) #:transparent)

;;;
;;; Parsing
;;;

(define target-re #px"^(\\d+):")
(define numbers-re #px"(\\d+)(?= |$)")

(define (parse-equations input)
  (sequence-map parse-equation (in-lines input)))

(define (parse-equation line)
  (define target (string->number (cadr (regexp-match target-re line))))
  (define numbers (map string->number (regexp-match* numbers-re line)))
  (equation target numbers))

;;;
;;; Solving
;;;

(define (|| left right)
  (string->number (string-append (number->string left) (number->string right))))

(define (has-solution? equation [operators (list + *)])
  (define target (equation-target equation))
  (define numbers (equation-numbers equation))
  (let loop ([acc (car numbers)] [numbers (cdr numbers)])
    (cond
      [(empty? numbers) (= acc target)]
      [(<= acc target)
       (for/or ([operator operators])
         (loop (operator acc (car numbers)) (cdr numbers)))]
      [else #f])))

;;;
;;; Solutions
;;;

(define (part-one [input (puzzle-input-port 2024 7)])
  (define equations (parse-equations input))
  (define solvable (sequence-filter has-solution? equations))
  (sequence-fold + 0 (sequence-map equation-target solvable)))

(define (part-two [input (puzzle-input-port 2024 7)])
  (define equations (parse-equations input))
  (define solvable (sequence-filter (λ (e) (has-solution? e (list + * ||))) equations))
  (sequence-fold + 0 (sequence-map equation-target solvable)))
