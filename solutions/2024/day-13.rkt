;;;
;;; Day 13: Claw Contraption
;;;

#lang racket/base

(require racket/generator)
(require racket/sequence)

(require advent)
(require advent/planar/point)

(provide part-one part-two)

;;;
;;; Data model
;;;

;; The buttons and prize of a game.
(struct game (a b prize) #:transparent)

;; The number of times the buttons need to be pressed.
(struct ratio (a b) #:transparent)

;;;
;;; Parsing
;;;

(define game-re #px"Button A.*?Y=\\d+")
(define button-a-re #px"Button A: X\\+(\\d+), Y\\+(\\d+)")
(define button-b-re #px"Button B: X\\+(\\d+), Y\\+(\\d+)")
(define prize-re #px"Prize: X=(\\d+), Y=(\\d+)")

(define (port->games port)
  (define as-point (λ (s) (apply point (map string->number s))))
  (in-generator
   (for ([input (sequence-map bytes->string/utf-8 (regexp-match* game-re port))])
     (let ([button-a (cdr (regexp-match button-a-re input))]
           [button-b (cdr (regexp-match button-b-re input))]
           [prize (cdr (regexp-match prize-re input))])
       (yield (game (as-point button-a)
                    (as-point button-b)
                    (as-point prize)))))))

(define (games->scaled-games games [scale 10000000000000])
  (in-generator
   (for ([g games])
     (let ([np (point (+ (point-x (game-prize g)) scale)
                      (+ (point-y (game-prize g)) scale))])
       (yield (struct-copy game g [prize np]))))))

;;;
;;; Ratios
;;;

(define (in-ratios games [limit 100])
  (in-generator
   (for ([game games])
     (let ([a (game-a game)] [b (game-b game)] [p (game-prize game)])
       (let* ([tb (/ (point-cross-product p a) (point-cross-product b a))]
              [ta (/ (- (point-x p) (* (point-x b) tb)) (point-x a))])
         (when (and (integer? ta) (integer? tb) (<= ta limit) (<= tb limit))
           (yield (ratio ta tb))))))))

(define (ratio->score ratio)
  (+ (* 3 (ratio-a ratio)) (ratio-b ratio)))

;;;
;;; Solutions
;;;

(define (part-one [input (puzzle-input-port 2024 13)])
  (for/sum ([r (in-ratios (port->games input) 100)])
    (ratio->score r)))

(define (part-two [input (puzzle-input-port 2024 13)])
  (for/sum ([r (in-ratios (games->scaled-games (port->games input)) +inf.0)])
    (ratio->score r)))
