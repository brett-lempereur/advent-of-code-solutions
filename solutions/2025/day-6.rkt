;;;
;;; Day 6: Trash Compactor
;;;

#lang racket/base

(require racket/list)
(require racket/match)
(require racket/port)
(require racket/string)

(require advent)

;;
;; Data structures
;;

;; An equation is an operator and a list of operands.
(struct equation (operator operands) #:transparent)

;;
;; Parsing
;;

;; Parse a sheet of cephalopod homework into a pair of a list of
;; operations and the raw columns of characters.
(define (parse in)
  (define lines (port->lines in))
  (define operands (map string->list (take lines (- (length lines) 1))))
  (define operators (last lines))
  (for/list ([operator (parse-operators operators)]
             [operand (split-columns operands)])
    (equation operator operand)))

;; Parse a list of operators.
(define (parse-operators line)
  (for/list ([c (string-split line " " #:repeat? #t)])
    (match c
      ["*" '*]
      ["+" '+])))

;; Split a list of lists of characters into columns.
(define (split-columns rows)
  (reverse
   (let loop ([rows rows] [columns '()])
     (let ([row-length (length (car rows))])
       (if (= row-length 0)
           columns
           (let ([split (find-next-split rows)])
             (if (>= split row-length)
                 (cons rows columns)
                 (let ([column (map (λ (s) (take s split)) rows)]
                       [remaining (map (λ (s) (drop s (add1 split))) rows)])
                   (loop remaining (cons column columns))))))))))

;; Given a list of lists of characters, returns the index of the next
;; column that contains only spaces or the end of the string.
(define (find-next-split rows)
  (define row-length (length (car rows)))
  (let loop ([i 0] [rows rows])
    (if (>= i row-length)
        row-length
        (if (for/and ([r rows]) (eq? (car r) #\ ))
            i
            (loop (+ i 1) (map cdr rows))))))

;;;
;;; Evaluation
;;;

;; Evaluate an operation against a set of operands.
(define (evaluate operator operands)
  (match operator
    ['* (apply * operands)]
    ['+ (apply + operands)]))

;; Evaluate an equation against the row-values in a column.
(define (evaluate-rowwise equation)
  (evaluate (equation-operator equation)
            (map (compose string->number string-trim list->string)
                 (equation-operands equation))))

;; Evaluate an equation against the column-values in a column.
(define (evaluate-columnwise equation)
  (define operands
    (let loop ([vals '()] [operands (equation-operands equation)])
      (if (empty? (car operands))
          vals
          (loop
           (cons (for/fold ([n 0]) ([v (filter
                                        (λ (c) (not (eq? c #\ )))
                                        (map car operands))])
                   (+ (* n 10) (string->number (list->string (list v)))))
                 vals)
           (map cdr operands)))))
  (display (format "operator=~a operands=~a\n" (equation-operator equation) operands))
  (evaluate (equation-operator equation) operands))

;;;
;;; Solutions
;;;

;; What is the grand total found by adding together all of the answers to
;; the individual problems?
(define (part-one [in (puzzle-input-port 2025 6)])
  (define equations (parse in))
  (for/sum ([e equations]) (evaluate-rowwise e)))

;;
(define (part-two [in (puzzle-input-port 2025 6)])
  (define equations (parse in))
  (for/sum ([e equations]) (evaluate-columnwise e)))
