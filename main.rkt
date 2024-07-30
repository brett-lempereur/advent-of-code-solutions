;;;
;;; main.rkt
;;;
;;; A harness for running Advent of Code solutions.
;;;

#lang racket/base

(require racket/cmdline)
(require racket/runtime-path)

;; Command-line parameters.
(define skip-one (make-parameter #f))
(define skip-two (make-parameter #f))

;; The path of the solutions directory.
(define-runtime-path solutions "solutions")

(module+ main
  (define solution-path
    (command-line
     #:program "advent-of-code"
     #:usage-help "Run the solution for a day."
     #:once-each
     [("-o" "--skip-one") "skip the first part of the solution" (skip-one #t)]
     [("-t" "--skip-two") "skip the second part of the solution" (skip-two #t)]
     #:args (year day)
     (build-path solutions year (format "day-~a.rkt" day))))
  (when (not (file-exists? solution-path))
    (printf "No such solution: ~a\n" solution-path)
    (exit 1))
  (unless (skip-one)
    (printf "Computing solution to part one...")
    (time
     (let* ([solution (dynamic-require solution-path 'part-one)]
            [result (solution)])
       (printf " result: ~a\n" result))))
  (unless (skip-two)
    (printf "Computing solution to part two...")
    (time
     (let* ([solution (dynamic-require solution-path 'part-two)]
            [result (solution)])
       (printf " result: ~a\n" result)))))
