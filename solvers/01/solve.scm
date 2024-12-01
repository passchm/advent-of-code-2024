(use-modules ((ice-9 textual-ports) #:select (get-string-all)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Parse

(define (split-by-substring str delimiter)
  (if (or (string=? str "") (string=? delimiter ""))
    (error "Both the main string and the delimiter must be non-empty strings"))
  (let loop ((s str) (acc '()))
    (let ((pos (string-contains s delimiter)))
      (if pos
        (loop (substring s (+ pos (string-length delimiter)))
              (cons (substring s 0 pos) acc))
        (reverse (cons s acc))))))

(define (parse-input input-data)
  (map (lambda (line)
         (map string->number (split-by-substring line "   ")))
       (string-split
         (string-trim-right input-data #\linefeed)
         #\linefeed)))

; Part 1

(define (part-1 input-data)
  (let* ((side-by-side (parse-input input-data))
         (left-numbers (sort-list (map car side-by-side) <))
         (right-numbers (sort-list (map cadr side-by-side) <)))
    (apply +
           (let loop ((dists '()) (l-nums left-numbers) (r-nums right-numbers))
             (if (and (pair? l-nums) (pair? r-nums))
               (loop
                 (cons (abs (- (car l-nums) (car r-nums))) dists)
                 (cdr l-nums)
                 (cdr r-nums))
               dists)))))

(display (part-1 input-data))
(newline)

; Part 2

(define (count-occurrences numbers)
  (let ((counters (make-hash-table)))
    (let loop ((nums numbers))
      (if (pair? nums)
        (begin
          (hashq-set! counters (car nums) (+ 1 (hashq-ref counters (car nums) 0)))
          (loop (cdr nums)))
        '()))
    counters))

(define (calculate-similarity-score left-numbers right-counters)
  (apply +
         (map (lambda (num)
                (* num (hashq-ref right-counters num 0)))
              left-numbers)))

(define (part-2 input-data)
  (let* ((side-by-side (parse-input input-data))
         (left-numbers (sort-list (map car side-by-side) <))
         (right-numbers (sort-list (map cadr side-by-side) <))
         (right-counters (count-occurrences right-numbers)))
    (calculate-similarity-score left-numbers right-counters)))

(display (part-2 input-data))
(newline)
