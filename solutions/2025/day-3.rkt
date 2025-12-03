;;;
;;; Day 3: Lobby
;;;

#lang racket/base

(require racket/generator)
(require racket/list)

(require advent)

;;;
;;; Parsing
;;;

;; Return a generator that yields the lists of numbers in each bank.
(define (in-banks in)
  (in-generator
   (for ([line (in-lines in)])
     (let ([digits (string->list line)])
       (yield
        (for/list ([digit digits])
          (string->number (list->string (list digit)))))))))

;;;
;;; Processing
;;;

;; Return a list truncated to the first-highest value that is less than
;; or equal to the given value.
(define (greatest-<= numbers [target 9])
  (define current -inf.0)
  (define output #f)
  (for ([i (in-naturals)] [n numbers])
    (when (and (> n current) (<= n target))
      (set! current n)
      (set! output i)))
  (if output (drop numbers output) null))

;; Find the greatest k-digit joltage that can be created from a bank.
(define (greatest-k-joltage bank [k 12])
  (define output
    (let loop ([b bank] [j null] [k k])
      (cond
        [(<= k 0) j]
        [(< (length b) k) #f]
        [(= (length b) k) (append (reverse b) j)]
        [else
         (let inner-loop ([d 9])
           (let ([n (greatest-<= b d)])
             (if (not (null? n))
                 (let ([r (loop (cdr n) (cons (car n) j) (sub1 k))])
                   (if r
                       r
                       (inner-loop (sub1 d))))
                 #f)))])))
  (for/fold ([t 0]) ([n (reverse output)])
    (+ (* t 10) n)))

;;;
;;; Solutions
;;;

;; There are many batteries in front of you. Find the maximum joltage
;; possible from each bank; what is the total output joltage?
(define (part-one [in (puzzle-input-port 2025 3)])
  (for/sum ([bank (in-banks in)])
    (greatest-k-joltage bank 2)))

;; The joltage output for the bank is still the number formed by the
;; digits of the batteries you've turned on; the only difference is that
;; now there will be 12 digits in each bank's joltage output instead of
;; two.
;;
;; What is the total output joltage?
(define (part-two [in (puzzle-input-port 2025 3)])
  (for/sum ([bank (in-banks in)])
    (greatest-k-joltage bank 12)))
