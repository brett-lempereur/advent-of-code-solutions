;;;
;;; Day 12: Garden Groups
;;;

#lang racket/base

(require racket/generator)
(require racket/list)
(require racket/set)

(require advent)
(require advent/grid)
(require advent/planar/point)

;;;
;;; Parsing
;;;

(define (port->garden port)
  (port->grid port))

;;;
;;; Plots
;;;

(define (in-plots g)
  (in-generator
   (let loop ([unvisited (for/set ([p (in-coordinates g)]) p)])
     (unless (set-empty? unvisited)
       (let* ([current-point (set-first unvisited)]
              [current-plant (grid-ref g current-point)]
              [plot (for/set ([n (in-regionf g current-point (λ (c) (eq? c current-plant)))]) n)])
         (yield plot)
         (loop (set-subtract unvisited plot)))))))

(define (in-cell-prices g)
  (in-generator
   (for ([plot (in-plots g)])
     (let ([area (set-count plot)]
           [perimiter
            (for/sum ([p plot])
              (- 4 (for/sum ([d cardinal-neighbours]) (if (set-member? plot (point+ p d)) 1 0))))])
       (yield (* area perimiter))))))

(define (in-edge-prices g)
  (in-generator
   (for ([plot (in-plots g)])
     (let* ([area (set-count plot)])
       (yield (* area (count-edges plot)))))))

(define (external-corners plot)
  (for/sum ([p plot])
    (let ([n (for/fold ([o '()]) ([d cardinal-neighbours])
               (if (not (set-member? plot (point+ p d))) (cons (point+ p d) o) o))])
      (cond
        [(= 2 (length n))
         (if (and (not (= (point-x (first n)) (point-x (second n))))
                  (not (= (point-y (first n)) (point-y (second n)))))
             1
             0)]
        [(= 3 (length n)) 2]
        [(= 4 (length n)) 4]
        [else 0]))))

(define (internal-corners plot)
  (for/sum ([p plot])
    (for/sum ([d ordinal-neighbours])
      (let ([a (point+ (point (point-x d) 0) p)]
            [b (point+ (point 0 (point-y d)) p)])
        (if (and (set-member? plot a)
                 (set-member? plot b)
                 (not (set-member? plot (point+ p d))))
            1
            0)))))

;;;
;;; Solutions
;;;

(define (part-one [port (puzzle-input-port 2024 12)])
  (define garden (port->garden port))
  (for/sum ([p (in-cell-prices garden)]) p))

(define (part-two [port (puzzle-input-port 2024 12)])
  (define garden (port->garden port))
  (for/sum ([p (in-edge-prices garden)]) p))
