;;;
;;; Day 6: Guard Gallivant
;;;

#lang racket/base

(require racket/generator)
(require racket/match)
(require racket/sequence)
(require racket/stream)
(require racket/set)

(require advent)

;;;
;;; Data model
;;;

(struct state (width height obstacles start) #:transparent)

(struct guard (x y direction) #:transparent)

;;;
;;; Parsing
;;;

(define (parse-state input)
  (define width -inf.0)
  (define height -inf.0)
  (define obstacles (mutable-set))
  (define start null)
  (for ([y (in-naturals)] [line (in-lines input)])
    (set! height (max height y))
    (for ([x (in-naturals)] [char (in-string line)])
      (set! width (max width x))
      (cond
        [(eq? char #\#) (set-add! obstacles (cons x y))]
        [(eq? char #\^) (set! start (guard x y 'up))])))
  (state width height obstacles start))

;;;
;;; Traversal
;;;

(define (next-direction current)
  (define x (guard-x current))
  (define y (guard-y current))
  (define direction (guard-direction current))
  (match direction
    ['up (guard x y 'right)]
    ['right (guard x y 'down)]
    ['down (guard x y 'left)]
    ['left (guard x y 'up)]))

(define (next-position current)
  (define x (guard-x current))
  (define y (guard-y current))
  (define direction (guard-direction current))
  (match direction
    ['up (guard x (sub1 y) direction)]
    ['right (guard (add1 x) y direction)]
    ['down (guard x (add1 y) direction)]
    ['left (guard (sub1 x) y direction)]))

(define (is-inside-grid? state current)
  (define x (guard-x current))
  (define y (guard-y current))
  (and (>= x 0) (<= x (state-width state))
       (>= y 0) (<= y (state-height state))))

(define (is-blocked? state current)
  (define x (guard-x current))
  (define y (guard-y current))
  (set-member? (state-obstacles state) (cons x y)))

(define (in-walk state [current (state-start state)])
  (in-generator
   (let loop ([current current])
     (when (is-inside-grid? state current)
       (yield current)
       (if (is-blocked? state (next-position current))
           (loop (next-direction current))
           (loop (next-position current)))))))

(define (count-new-obstacles state)
  (for/sum ([position (in-walk state)])
    (if (is-loop? state (next-position (next-direction position))) 1 0)))

(define (is-loop? state current)
  (let loop ([walk (sequence->stream (in-walk state current))] [visited (set)])
    (if (stream-empty? walk)
        #f
        (if (set-member? visited (stream-first walk))
            #t
            (loop (stream-rest walk) (set-add visited (stream-first walk)))))))

;;;
;;; Solutions
;;;

(define (part-one [input (puzzle-input-port 2024 6)])
  (define state (parse-state input))
  (define positions (for/set ([position (in-walk state)])
                      (cons (guard-x position) (guard-y position))))
  (set-count positions))

(define (part-two [input (puzzle-input-port 2024 6)])
  (define state (parse-state input))
  (count-new-obstacles state))
