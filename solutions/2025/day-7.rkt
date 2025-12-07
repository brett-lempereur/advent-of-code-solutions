;;;
;;; Day 7: Laboratories
;;;

#lang racket/base

(require racket/class)
(require racket/gui/base)
(require racket/list)
(require racket/math)
(require racket/port)

(require advent)

;;;
;;; Data structures
;;;

;; The state of the tachyon manifold.
(struct state (timelines splitters))

;; Return the width of a tachyon manifold.
(define (state-width state)
  (vector-length (car (state-splitters state))))

;; Return the height of a tachyon manifold.
(define (state-height state)
  (length (state-splitters state)))

;; Add a row of timelines to a state.
(define (state-append-timelines s t)
  (struct-copy state s [timelines (append (state-timelines s) (list t))]))

;;;
;;; Parsing
;;;

;; Parse a port into a tachyon manifold state.
(define (port->state in)
  (define lines (port->lines in))
  (define first-row-timelines (lines->first-row lines))
  (define splitters (lines->rows lines))
  (state (list first-row-timelines) splitters))

;; Parse a port into a single vector that represents the starting
;; position of the tachyon beam.
(define (lines->first-row lines)
  (define first-line (car lines))
  (for/vector ([c (in-string first-line)])
    (if (eq? c #\S) 1 0)))

;; Parse a port into a list of vectors, one per-row, where 1
;; indicates the presence of a splitter.
(define (lines->rows lines)
  (for/list ([line lines])
    (for/vector ([c (in-string line)])
      (if (eq? c #\^) 1 0))))

;;;
;;; Raycasting
;;;

;; Cast all timelines to the bottom of the tachyon manifold.
(define (cast state)
  (for/fold ([s state]) ([_ (in-range (sub1 (state-height state)))])
    (cast-timelines s)))

;; Compute the next timeline row and return the new tachyon manifold state.
(define (cast-timelines state)
  (define width (state-width state))
  (define splitters (list-ref (state-splitters state) (length (state-timelines state))))
  (define previous (last (state-timelines state)))
  (define timelines (make-vector width 0))
  (for ([i (in-range width)]
        [t (in-vector previous)]
        [s (in-vector splitters)])
    (when (> t 0)
      (if (= s 1)
          (begin
            (vector-set! timelines (sub1 i)
                         (+ (vector-ref timelines (sub1 i)) t))
            (vector-set! timelines (add1 i)
                         (+ (vector-ref timelines (add1 i)) t)))
          (vector-set! timelines i
                       (+ (vector-ref timelines i) t)))))
  (state-append-timelines state timelines))

;;;
;;; Counting
;;;

;; Count the number of times beams split in the tachyon manifold.
(define (count-splits state)
  (define timelines (state-timelines state))
  (define splitters (state-splitters state))
  (for/sum ([timeline-row timelines] [splitter-row (cdr splitters)])
    (for/sum ([t (in-vector timeline-row)] [s (in-vector splitter-row)])
      (if (and (> t 0) (= s 1)) 1 0))))

;; Count the total number of timelines at the bottom of the manifold.
(define (count-timelines state)
  (for/sum ([t (in-vector (last (state-timelines state)))]) t))

;;;
;;; Drawing
;;;

;; Find the maximum timeline count in the state.
(define (state-max-timelines state)
  (for*/fold ([m 0]) ([row (state-timelines state)]
                      [t (in-vector row)])
    (max m t)))

;; Show a frame that renders the tachyon manifold state.
(define (show-state state [scale 10])
  (define width (* scale (state-width state)))
  (define height (* scale (state-height state)))
  (define frame (new frame%
                     [label "Tachyon Manifold"]
                     [width width]
                     [height height]))
  (new canvas%
       [parent frame]
       [paint-callback (λ (canvas dc) (draw-state dc state scale))])
  (send frame show #t))

;; Render the tachyon manifold state.
(define (draw-state dc state scale)
  (define timelines (state-timelines state))
  (define splitters (state-splitters state))
  (define max-t (max 1 (state-max-timelines state)))
  (define (timeline->color t)
    (if (= t 0)
        (make-color 255 255 255)
        (let* ([ratio (/ (log (add1 t)) (log (add1 max-t)))]
               [r (exact-round (* 255 ratio))]
               [b (exact-round (* 255 (- 1 ratio)))])
          (make-color r 0 b))))
  (for ([y (in-naturals)]
        [timeline-row (in-sequences
                       timelines
                       (in-producer (λ () (make-vector (state-width state) 0))))]
        [splitter-row splitters])
    (for ([x (in-naturals)]
          [t (in-vector timeline-row)]
          [s (in-vector splitter-row)])
      (let ([px (* x scale)] [py (* y scale)])
        (if (= s 1)
            (send dc set-brush "green" 'solid)
            (send dc set-brush (timeline->color t) 'solid))
        (send dc draw-rectangle px py scale scale)))))

;;;
;;; Solutions
;;;

;; How many times will the beam be split?
(define (part-one [in (puzzle-input-port 2025 7)])
  (define state (port->state in))
  (define final-state (cast state))
  (show-state final-state)
  (count-splits final-state))

;; How many timelines does a single particle end up on?
(define (part-two [in (puzzle-input-port 2025 7)])
  (define state (port->state in))
  (define final-state (cast state))
  (show-state final-state)
  (count-timelines final-state))
