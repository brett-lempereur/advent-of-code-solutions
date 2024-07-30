;;;
;;; 1202 Program alarm
;;;

#lang racket/base

(require racket/match)
(require racket/port)
(require racket/string)
(require racket/vector)

(require advent)

(provide part-one part-two)

;;;
;;; Interpreter
;;;

;; An interpreter state.
(struct state (memory [pc #:mutable]) #:transparent)

;; Returns the value at the specified position.
(define (state-ref s p)
  (vector-ref (state-memory s) p))

;; Sets the value at the specified position.
(define (state-set! s p v)
  (vector-set! (state-memory s) p v))

;; Returns the current operation.
(define (state-operation s)
  (define pc (state-pc s))
  (state-ref s pc))

;; Returns the literal value of an operand to the current operation.
(define (state-literal s i)
  (define pc (state-pc s))
  (state-ref s (+ pc 1 i)))

;; Dereferences an operand of the current operation.
(define (state-dereference s i)
  (state-ref s (state-literal s i)))

;; Clones a state.
(define (state-clone s)
  (define memory (state-memory s))
  (struct-copy state s [memory (vector*-copy memory)]))

;;;
;;; Execution
;;;

;; Runs the program to completion.
(define (run s)
  (when (execute s)
    (run s)))

;; Executes the operation at the program counter, returning false if the
;; program terminates.
(define (execute s)
  (define operation (state-operation s))
  (match operation
    [1 (let [(lhs (state-dereference s 0)) (rhs (state-dereference s 1))]
         (state-set! s (state-literal s 2) (+ lhs rhs))
         (set-state-pc! s (+ (state-pc s) 4))
         #t)]
    [2 (let [(lhs (state-dereference s 0)) (rhs (state-dereference s 1))]
         (state-set! s (state-literal s 2) (* lhs rhs))
         (set-state-pc! s (+ (state-pc s) 4))
         #t)]
    [99 #f]))

;; Holds if the given noun and verb produce the required output.
(define (target? s noun verb target)
  (state-set! s 1 noun)
  (state-set! s 2 verb)
  (run s)
  (= (state-ref s 0) target))

;;;
;;; Parsing
;;;

;; Reads an interpreter state from a port.
(define (read-state port)
  (define input (string-split (string-trim (port->string port)) ","))
  (define positions (map string->number input))
  (state (list->vector positions) 0))

;;;
;;; Solutions
;;;

;; Modifies and executes the program, returning the value at position zero.
(define (part-one)
  (define state (read-state (puzzle-input-port 2019 2)))
  (state-set! state 1 12)
  (state-set! state 2 2)
  (run state)
  (state-ref state 0))

;; Finds the inputs that produce the requested output.
(define (part-two)
  (define state (read-state (puzzle-input-port 2019 2)))
  (define target 19690720)
  (for*/first ([noun (in-range 0 100)]
               [verb (in-range 0 100)]
               #:when (target? (state-clone state) noun verb target))
    (+ (* 100 noun) verb)))
