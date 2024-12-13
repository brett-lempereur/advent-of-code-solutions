;;;
;;; Day 11: Plutonian Pebbles
;;;

#lang racket/base

(require racket/list)
(require racket/treelist)

(require memo)

(require advent)

(provide part-one part-two)

;;;
;;; Parsing
;;;

(define (port->stones port)
  (for/list ([digits (regexp-match* #px"\\d+" port)])
    (string->number (bytes->string/utf-8 digits))))

;;;
;;; Evaluation
;;;

(define/memoize (evaluate stone iterations)
  (define ni (- iterations 1))
  (if (= iterations 0) 1
      (cond
        [(= stone 0) (evaluate 1 ni)]
        [(even? (string-length (number->string stone)))
         (let* ([digits (string->list (number->string stone))]
                [split (/ (length digits) 2)])
           (+ (evaluate (string->number (list->string (take digits split))) ni)
              (evaluate (string->number (list->string (drop digits split))) ni)))]
        [else (evaluate (* stone 2024) ni)])))

;;;
;;; Solutions
;;;

(define (part-one [input (puzzle-input-port 2024 11)])
  (define stones (port->stones input))
  (for/sum ([stone stones])
    (evaluate stone 25)))

(define (part-two [input (puzzle-input-port 2024 11)])
  (define stones (port->stones input))
  (for/sum ([stone stones])
    (evaluate stone 75)))
