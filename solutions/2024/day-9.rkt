;;;
;;; Day 9: Disk Fragmenter
;;;

#lang racket/base

(require racket/list)

(require advent)

(provide part-one part-two)

;;;
;;; Data model
;;;

;; A delmited region of the disk.
(struct extent (offset len) #:transparent #:mutable)

;; A file node.
(struct node (id extent) #:transparent)

;; A disk layout.
(struct disk (files free) #:transparent)

;;;
;;; Parsing
;;;

;; Parse a disk layout from an input port.
(define (parse-disk input)
  (let loop ([files '()] [free '()] [id 0] [offset 0] [is-node? #t])
    (let ([next (read-char input)])
      (if (or (eof-object? next) (not (string->number (string next))))
          (disk (reverse files) (reverse free))
          (let* ([len (string->number (string next))]
                 [extent (extent offset len)]
                 [offset (+ offset len)]
                 [files (if is-node? (cons (node id extent) files) files)]
                 [free (if is-node? free (cons extent free))]
                 [id (if is-node? (+ id 1) id)])
            (loop files free id offset (not is-node?)))))))

;;;
;;; Compaction
;;;

;; Compact the layout of a disk.
(define (compact input)
  (define files (disk-files input))
  (define free (disk-free input))
  (let loop ([resolved (list (car files))] [unresolved (reverse (cdr files))] [free free])
    (let ([last-resolved (car resolved)]
          [next-unresolved (car unresolved)]
          [next-free (car free)])
      ;; If the next unresolved file starts before the offset of the last
      ;; resolved file, we have finished.
      (if (< (extent-offset (node-extent next-unresolved))
             (extent-offset (node-extent last-resolved)))
          (disk (append unresolved resolved) free)
          (cond
            ;; Case 1 - The file fits into the next free block perfectly
            [(= (extent-len (node-extent next-unresolved)) (extent-len next-free))
             (let ([resolved-node (node (node-id next-unresolved) next-free)])
               (loop (cons resolved-node resolved)
                     (cdr unresolved)
                     (add-free (cdr free) (node-extent next-unresolved))))]
            ;; Case 2 - The file fits into the next free block and leaves a gap
            [(< (extent-len (node-extent next-unresolved)) (extent-len next-free))
             (let* ([resolved-node (node (node-id next-unresolved)
                                         (extent
                                          (extent-offset next-free)
                                          (extent-len (node-extent next-unresolved))))]
                    [resolved-free (extent (+ (extent-offset next-free)
                                              (extent-len (node-extent next-unresolved)))
                                           (- (extent-len next-free)
                                              (extent-len (node-extent next-unresolved))))])
               (loop (cons resolved-node resolved)
                     (cdr unresolved)
                     (add-free
                      (cons resolved-free (cdr free))
                      (extent
                       (extent-offset (node-extent next-unresolved))
                       (extent-len (node-extent next-unresolved))))))]
            ;; Case 3 - The file overflows the next free block
            [else
             (let* ([resolved-node (node (node-id next-unresolved)
                                         (extent
                                          (extent-offset next-free)
                                          (extent-len next-free)))]
                    [unresolved-node (node (node-id next-unresolved)
                                           (extent
                                            (+ (extent-offset (node-extent next-unresolved))
                                               (extent-len next-free))
                                            (- (extent-len (node-extent next-unresolved))
                                               (extent-len next-free))))])
               (loop (cons resolved-node resolved)
                     (cons unresolved-node (cdr unresolved))
                     (add-free
                      (cdr free)
                      (extent
                       (extent-offset (node-extent next-unresolved))
                       (extent-len next-free)))))])))))

;;;
;;; Defragmentation
;;;

(define (defragment input)
  (define files (disk-files input))
  (define free (disk-free input))
  (let loop ([resolved '()] [unresolved (reverse files)] [free free])
    (if (empty? unresolved)
        (disk resolved free)
        (let* ([next-unresolved (car unresolved)]
               [next-extent (node-extent next-unresolved)])
          ;; Find the point at which a new element could be inserted
          (let ([insertion-point (findf (λ (f) (and (< (extent-offset f) (extent-offset next-extent))
                                                    (>= (extent-len f) (extent-len next-extent))))
                                        free)])
            ;; If there is nowhere, skip the file and move onto the next
            (if (not insertion-point)
                (loop (cons next-unresolved resolved) (cdr unresolved) free)
                ;; Otherwise we can move the file, so do it.
                (let ([next-resolved (node (node-id next-unresolved)
                                           (extent (extent-offset insertion-point) (extent-len next-extent)))])
                  (if (< (extent-len next-extent) (extent-len insertion-point))
                      (begin
                        (set-extent-offset! insertion-point (+ (extent-offset insertion-point)
                                                               (extent-len next-extent)))
                        (set-extent-len! insertion-point (- (extent-len insertion-point)
                                                            (extent-len next-extent))))
                      (set-extent-len! insertion-point 0))
                  (loop (cons next-resolved resolved)
                        (cdr unresolved)
                        (add-free (clean-free free) next-extent)))))))))

;;;
;;; Utilities
;;;

;; Remove empty extents from the free-list.
(define (clean-free free)
  (let loop ([result '()] [free free])
    (if (empty? free)
        (reverse result)
        (if (= (extent-len (car free)) 0)
            (loop result (cdr free))
            (loop (cons (car free) result) (cdr free))))))

;; Add an extent to the free-list.
(define (add-free free extent)
  (let loop ([result '()] [free free])
    (if (empty? free)
        (reverse (cons extent result))
        (if (< (extent-offset (car free)) (extent-offset extent))
            (loop (cons (car free) result) (cdr free))
            (reverse (append (cons extent free) result))))))

;;;
;;; Printing
;;;

;; Prints a disk layout.
(define (print-disk input)
  (define items
    (sort (append (disk-files input) (disk-free input))
          <
          #:key (λ (i) (if (node? i) (extent-offset (node-extent i)) (extent-offset i)))))
  (for ([item items])
    (let ([symbol (if (node? item) (number->string (node-id item)) ".")]
          [extent (if (node? item) (node-extent item) item)])
      (for ([_ (in-range 0 (extent-len extent))])
        (printf "~a" symbol))))
  (printf "\n"))

;;;
;;; Solutions
;;;

(define (part-one [input (puzzle-input-port 2024 9)])
  (define input-disk (parse-disk input))
  (define compacted-disk (compact input-disk))
  (for/sum ([node (sort (disk-files compacted-disk) < #:key (λ (n) (extent-offset (node-extent n))))])
    (let ([offset (extent-offset (node-extent node))]
          [len (extent-len (node-extent node))])
      (for/sum ([index (in-range offset (+ offset len))])
        (* (node-id node) index)))))

(define (part-two [input (puzzle-input-port 2024 9)])
  (define input-disk (parse-disk input))
  (define defragmented-disk (defragment input-disk))
  (for/sum ([node (sort (disk-files defragmented-disk) < #:key (λ (n) (extent-offset (node-extent n))))])
    (let ([offset (extent-offset (node-extent node))]
          [len (extent-len (node-extent node))])
      (for/sum ([index (in-range offset (+ offset len))])
        (* (node-id node) index)))))
