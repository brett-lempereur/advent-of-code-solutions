;;;
;;; Day 4: Ceres Search
;;;

#lang racket/base

(require racket/list)
(require racket/generator)
(require racket/sequence)
(require racket/set)

(require advent)

;;;
;;; Data model
;;;

(struct grid (letters width height) #:transparent)

;;;
;;; Parsing
;;;

(define (parse-grid input)
  (define width 0)
  (define height 0)
  (define letters
    (for/fold ([indices (hash)]) ([line (in-lines input)] [y (in-naturals)])
      (set! height (max height y))
      (for/fold ([indices indices]) ([char (in-string line)] [x (in-naturals)])
        (set! width (max width x))
        (hash-update indices char (λ (s) (set-add s (cons x y))) (set)))))
  (grid letters width height))

;;;
;;; Searching
;;;

(define vectors '((0 . -1) (1 . -1) (1 . 0) (1 . 1) (0 . 1) (-1 . 1) (-1 . 0) (-1 . -1)))

(define target '(#\X #\M #\A #\S))

(define (in-all-matches grid)
  (define locations (hash-ref (grid-letters grid) (car target)))
  (in-generator
   (for ([pos locations])
     (for ([m (in-matches grid pos)])
       (yield m)))))

(define (in-matches grid pos)
  (define letters (grid-letters grid))
  (in-generator
   (for ([line (in-candidates grid pos)])
     (when (for/and ([p line] [t target]) (set-member? (hash-ref letters t) p))
       (yield line)))))

(define (in-candidates grid pos)
  (in-generator
   (for ([vec vectors])
     (let ([line (sequence->list (in-line grid pos vec))])
       (when (not (empty? line))
         (yield line))))))

(define (in-line grid pos vec)
  (define lx (+ (car pos) (* 3 (car vec))))
  (define ly (+ (cdr pos) (* 3 (cdr vec))))
  (in-generator
   (when (and (>= lx 0) (>= ly 0)
              (<= lx (grid-width grid)) (<= ly (grid-height grid)))
     (for ([i (in-range 0 4)])
       (yield (cons (+ (car pos) (* i (car vec)))
                    (+ (cdr pos) (* i (cdr vec)))))))))

;;;
;;; Cross-searching
;;;

(define cross-selectors
  (list (list '(-1 . -1) '(0 . 0) '(1 . 1))
        (list '(1 . -1) '(0 . 0) '(-1 . 1))))

(define cross-patterns
  (set '(#\M #\A #\S) '(#\S #\A #\M)))

(define (in-cross-matches grid)
  (define locations (hash-ref (grid-letters grid) #\A))
  (in-generator
   (for ([pos locations])
     (when (is-cross-match? grid pos)
       (yield pos)))))

(define (is-cross-match? grid pos)
  (for/and ([selector cross-selectors])
    (for/or ([pattern cross-patterns])
      (for/and ([c pattern] [p selector])
        (set-member? (hash-ref (grid-letters grid) c)
                     (cons (+ (car p) (car pos))
                           (+ (cdr p) (cdr pos))))))))

;;;
;;; Solutions
;;;

(define (part-one [input (puzzle-input-port 2024 4)])
  (define puzzle-grid (parse-grid input))
  (sequence-length (in-all-matches puzzle-grid)))

(define (part-two [input (puzzle-input-port 2024 4)])
  (define puzzle-grid (parse-grid input))
  (sequence-length (in-cross-matches puzzle-grid)))
