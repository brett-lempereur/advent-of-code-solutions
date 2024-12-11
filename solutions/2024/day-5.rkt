;;;
;;; Day 5: Print Queue
;;;

#lang racket/base

(require racket/list)
(require racket/port)
(require racket/set)
(require racket/string)

(require advent)


;;;
;;; Parsing
;;;

(define (parse-rules input)
  (define rules (regexp-match* #px"\\d+\\|\\d+" input))
  (for/fold ([output (hash)]) ([rule rules])
    (let ([pair (map string->number (string-split rule "|"))])
      (hash-update output (car pair) (λ (s) (set-add s (cadr pair))) (set)))))

(define (parse-updates input)
  (define updates (regexp-match* #px"(?m:^\\d+,.*,\\d+$)" input))
  (for/fold ([output '()]) ([update updates])
    (cons (map string->number (string-split update ",")) output)))

;;;
;;; Validation
;;;

(define (valid-updates rules updates)
  (filter (λ (update) (is-valid? rules update)) updates))

(define (invalid-updates rules updates)
  (filter (λ (update) (not (is-valid? rules update))) updates))

(define (is-valid? rules update)
  (define current (car update))
  (define remaining (list->set (cdr update)))
  (define expected (hash-ref rules current (set)))
  (define unexpected (set-subtract remaining expected))
  (if (set-empty? unexpected)
      (if (> (length (cdr update)) 1)
          (is-valid? rules (cdr update))
          #t)
      #f))

(define (is-next? rules update current)
  (define expected (hash-ref rules current (set)))
  (define actual (set-remove (list->set update) current))
  (set-empty? (set-subtract actual expected)))

(define (corrected-update rules update)
  (if (= (length update) 1)
      (car update)
      (let ([next (for/first ([c update] #:when (is-next? rules update c)) c)])
        (cons next (corrected-update rules (remove next update))))))

;;;
;;; Solutions
;;;

(define (part-one [port (puzzle-input-port 2024 5)])
  (define input (port->string port))
  (define rules (parse-rules input))
  (define updates (parse-updates input))
  (for/sum ([update (valid-updates rules updates)])
    (list-ref update (floor (/ (length update) 2)))))

(define (part-two [port (puzzle-input-port 2024 5)])
  (define input (port->string port))
  (define rules (parse-rules input))
  (define updates (parse-updates input))
  (for/sum ([update (invalid-updates rules updates)])
    (list-ref (corrected-update rules update) (floor (/ (length update) 2)))))
