(use-modules ((ice-9 textual-ports) #:select (get-string-all)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (map string->number
       (string-split
         (string-trim-right input-data #\linefeed)
         #\linefeed)))

(define (shift-mix-prune shift secret-number)
  (logand
    (logxor (ash secret-number shift) secret-number)
    (1- (ash 1 24))))

(define (evolve-secret-number secret-number)
  (shift-mix-prune 11
                   (shift-mix-prune -5
                                    (shift-mix-prune 6 secret-number))))

(define (simulate-secret-numbers initial-secret-number steps)
  (let loop ((step 0) (secret-number initial-secret-number))
    (if (= step steps)
      secret-number
      (loop (1+ step) (evolve-secret-number secret-number)))))

(define (part-1 input-data)
  (apply +
         (map (lambda (secret-number) (simulate-secret-numbers secret-number 2000))
              (parse-input input-data))))

(display (part-1 input-data))
(newline)

; Part 2

(define (simulate-sequences initial-secret-number steps)
  (let loop ((step 1)
             (prev-price (euclidean-remainder initial-secret-number 10))
             (curr-number (evolve-secret-number initial-secret-number))
             (prev-diff #f)
             (prev-prev-diff #f)
             (prev-prev-prev-diff #f)
             (diffs '()))
    (if (= step steps)
      (list-tail (reverse diffs) 3)
      (let ((curr-price (euclidean-remainder curr-number 10)))
        (loop (1+ step)
              curr-price
              (evolve-secret-number curr-number)
              (- curr-price prev-price)
              prev-diff
              prev-prev-diff
              (cons (list
                      curr-price
                      prev-prev-prev-diff
                      prev-prev-diff
                      prev-diff
                      (- curr-price prev-price))
                    diffs))))))

(define (build-sequence-to-price-table initial-secret-number steps)
  (let ((table (make-hash-table))
        (sequences (simulate-sequences initial-secret-number steps)))
    (for-each (lambda (seq)
                ; Only the first occurrence of the sequence matters
                (when (not (hash-ref table (cdr seq)))
                  (hash-set! table (cdr seq) (car seq))))
              sequences)
    table))

(define (build-all-sequences-set sequence-to-price-tables)
  (let ((all-sequences-set (make-hash-table)))
    (for-each (lambda (table)
                (hash-for-each
                  (lambda (key value) (hash-set! all-sequences-set key #t))
                  table))
              sequence-to-price-tables)
    (hash-map->list (lambda (key value) key) all-sequences-set)))

(define (find-best-sequence all-sequences-set sequence-to-price-tables)
  (let loop ((best-price -inf.0) (best-sequence '()) (sequences all-sequences-set))
    (if (null? sequences)
      (cons best-price best-sequence)
      (let ((price
              (apply +
                     (map (lambda (table) (hash-ref table (car sequences) 0))
                          sequence-to-price-tables))))
        (when (equal? (car sequences) '(-2 1 -1 3))
          (map (lambda (table) (hash-ref table (car sequences) 0))
               sequence-to-price-tables))
        (if (> price best-price)
          (loop price (car sequences) (cdr sequences))
          (loop best-price best-sequence (cdr sequences)))))))

; Attention: The test input for part 2 is different from the one for part 1!

(define (part-2 input-data)
  (let* ((secret-numbers (parse-input input-data))
         (sequence-to-price-tables
           (map (lambda (secret-number)
                  (build-sequence-to-price-table secret-number 2000))
                secret-numbers))
         (all-sequences-set
           (build-all-sequences-set sequence-to-price-tables)))
    (car
      (find-best-sequence all-sequences-set sequence-to-price-tables))))

(display (part-2 input-data))
(newline)
