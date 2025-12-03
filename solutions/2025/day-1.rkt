;;;
;;; Day 1: Secret Entrance
;;;

#lang racket/base

(require racket/generator)
(require racket/match)

(require advent)

;;;
;;; Parsing
;;;

;; Regular expression for parsing rotations.
(define rotation-re #px"^([LR])(\\d+)")

;; Parse an input sequence and return a generator that yields the relative
;; rotation values for an input.
(define (in-rotations in)
  (define (parse-line line)
    (match-define (list _ dir amt) (regexp-match rotation-re line))
    (let ([amt (string->number amt)])
      (if (equal? (string-downcase dir) "r") amt (- amt))))
  (in-generator
   (for ([line (in-lines in)])
     (yield (parse-line line)))))

;;;
;;; Generation
;;;

;; Return the sequence of dial positions.
(define (in-positions in [start 50] [size 100])
  (define dial start)
  (in-generator
   (yield dial)
   (for ([rotation (in-rotations in)])
     (set! dial (modulo (+ dial rotation) size))
     (yield dial))))

;;;
;;; Solutions
;;;

;; The actual password is the number of times the dial is left pointing at
;; 0 after any rotation in the sequence.
;;
;; What's the actual password?
(define (part-one)
  (define in (puzzle-input-port 2025 1))
  (for/sum ([position (in-positions in)])
    (if (= position 0) 1 0)))

;; Count the number of times the dial passes through 0 during a rotation.
;; pos: starting position, rot: signed rotation (positive = right, negative = left)
(define (count-zeros pos rot [size 100])
  (if (>= rot 0)
      (quotient (+ pos rot) size)
      (let ([d (- rot)])
        (if (= pos 0)
            (quotient d size)
            (if (<= pos d)
                (+ 1 (quotient (- d pos) size))
                0)))))

;; Count the number of times any click causes the dial to point at 0.
;;
;; What is the password to open the door?
(define (part-two)
  (define in (puzzle-input-port 2025 1))
  (define dial 50)
  (for/sum ([rotation (in-rotations in)])
    (let ([zeros (count-zeros dial rotation)])
      (set! dial (modulo (+ dial rotation) 100))
      zeros)))
