;;;
;;; Day 8: Resonant Collinearity
;;;

#lang racket/base

(require racket/generator)
(require racket/match)
(require racket/set)

(require advent)
(require advent/planar/point)

;;;
;;; Data model
;;;

(struct arena (width height antennas) #:transparent)

;;;
;;; Parsing
;;;

(define (parse-arena input)
  (define width -inf.0)
  (define height -inf.0)
  (define antennas (make-hash))
  (for ([y (in-naturals)] [line (in-lines input)])
    (set! height (max height y))
    (for ([x (in-naturals)] [char (in-string line)])
      (set! width (max width x))
      (when (not (eq? char #\.))
        (hash-update! antennas char (λ (ps) (set-add ps (point x y))) (set)))))
  (arena width height antennas))

;;;
;;; Computation
;;;

(define (in-antinodes arena)
  (define width (arena-width arena))
  (define height (arena-height arena))
  (in-generator
   (for ([antennas (in-hash-values (arena-antennas arena))])
     (for ([antinode (in-antennas-antinodes width height antennas)])
       (yield antinode)))))

(define (in-antennas-antinodes width height antennas)
  (in-generator
   (for* ([a antennas] [b antennas])
     (when (not (equal? a b))
       (let* ([Δ (point- b a)]
              [ω (point+ b Δ)])
         (when (and (>= (point-x ω) 0) (<= (point-x ω) width)
                    (>= (point-y ω) 0) (<= (point-y ω) height))
           (yield ω)))))))

(define (in-resonant-antinodes arena)
  (define width (arena-width arena))
  (define height (arena-height arena))
  (in-generator
   (for ([antennas (in-hash-values (arena-antennas arena))])
     (for ([antinode (in-antennas-resonant-antinodes width height antennas)])
       (yield antinode)))))

(define (in-antennas-resonant-antinodes width height antennas)
  (in-generator
   (for* ([a antennas] [b antennas])
     (when (not (equal? a b))
       (yield b)
       (let ([Δ (point- b a)])
         (let loop ([ω (point+ b Δ)])
           (when (and (>= (point-x ω) 0) (<= (point-x ω) width)
                      (>= (point-y ω) 0) (<= (point-y ω) height))
             (yield ω)
             (loop (point+ ω Δ)))))))))

;;;
;;; Solutions
;;;

(define (part-one [input (puzzle-input-port 2024 8)])
  (define input-arena (parse-arena input))
  (define antinodes (for/set ([a (in-antinodes input-arena)]) a))
  (set-count antinodes))

(define (part-two [input (puzzle-input-port 2024 8)])
  (define input-arena (parse-arena input))
  (define antinodes (for/set ([a (in-resonant-antinodes input-arena)]) a))
  (set-count antinodes))
