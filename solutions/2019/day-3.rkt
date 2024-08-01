;;;
;;;
;;;

#lang racket/base

(require racket/list)
(require racket/match)
(require racket/port)
(require racket/set)
(require racket/string)

(require advent)
(require advent/planar/point)
(require advent/planar/segment)

;;;
;;; Parsing
;;;

;; Regular expression patterns.
(define move-re #px"^([UDLR])(\\d+)")

;; Parse the input and return both segment sequences.
(define (parse port)
  (define lines (port->lines port))
  (values
   (parse-segments (string-trim (first lines)))
   (parse-segments (string-trim (second lines)))))

;; Parse a line into a sequence of segments.
(define (parse-segments line)
  (define steps (map (λ (s)
                       (let ([m (regexp-match move-re s)])
                         (cons (second m) (string->number (third m)))))
                     (string-split line ",")))
  (define points
    (let loop ([path (list (point 0 0))] [steps steps])
      (if (empty? steps) path
          (let ([current-point (car path)]
                [delta (match (car steps)
                         [(cons "U" n) (point 0 (- n))]
                         [(cons "D" n) (point 0 n)]
                         [(cons "L" n) (point (- n) 0)]
                         [(cons "R" n) (point n 0)])])
            (loop (cons (point+ current-point delta) path) (cdr steps))))))
  (let loop ([segments (list)] [points (reverse points)])
    (if (<= (length points) 1) (reverse segments)
        (loop (cons (segment (car points) (cadr points)) segments)
              (cdr points)))))

;;;
;;; Intersection
;;;

;; Collect the set of points at which two sequences of segments intersect.
(define (find-intersections p q)
  (for*/fold ([points (set)]) ([ps p] [qs q])
    (let ([i (segment-intersect ps qs)])
      (if i (set-add points i) points))))

;; Order the points by their distance from the origin.
(define (sort-intersections is)
  (sort (set->list is)
        <
        #:key (λ (p) (point-manhattan-distance p (point 0 0)))))

;; Find the number of steps along a wire each intersection is.
(define (find-distance segments i)
  (let loop ([segments segments] [total 0])
    (if (empty? segments) (error "point did not intersect wire")
        (let ([current-segment (car segments)])
          (if (segment-contains? current-segment i)
              (+ total (point-distance (segment-p current-segment) i))
              (loop (cdr segments)
                    (+ total (segment-manhattan-length current-segment))))))))

;;;
;;; Solutions
;;;

;; Find the intersection point closest to the origin.
(define (part-one)
  (define-values (p q) (parse (puzzle-input-port 2019 3)))
  (point-manhattan-distance
   (car (sort-intersections (find-intersections p q)))
   (point 0 0)))

;; Find the intersection point closest to the origin along both wires.
(define (part-two)
  (define-values (p q) (parse (puzzle-input-port 2019 3)))
  (define intersections (find-intersections p q))
  (define pd (for/fold ([output (hash)]) ([i intersections])
               (hash-set output i (find-distance p i))))
  (define qd (for/fold ([output (hash)]) ([i intersections])
               (hash-set output i (find-distance q i))))
  (define t (for/list ([i intersections])
              (+ (hash-ref pd i) (hash-ref qd i))))
  (car (sort t <)))
