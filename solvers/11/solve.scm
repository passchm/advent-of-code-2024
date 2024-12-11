(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 receive) #:select (receive)))
(use-modules ((ice-9 arrays) #:select (array-copy)))
(use-modules ((srfi srfi-1) #:select (every partition)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (map string->number
       (string-split
         (string-trim-right input-data #\linefeed)
         #\space)))

(define (apply-rules stone)
  (cond
    ((= stone 0) '(1))
    ((= 0 (euclidean-remainder (+ 1 (truncate (log10 stone))) 2))
     (let ((num-digits (inexact->exact (truncate (+ 1 (log10 stone))))))
       (receive (q r)
                (euclidean/ stone
                            (expt 10
                                  (+ 1 (- (truncate (log10 stone)) (/ num-digits 2)))))
                (list (inexact->exact q) (inexact->exact r)))))
    (else (list (* 2024 stone)))))

(define (watch-stone max-step step stone)
  (if (>= step max-step)
    (list stone)
    (apply append
           (map (lambda (s) (watch-stone max-step (+ step 1) s))
                (apply-rules stone)))))

(define (part-1 input-data)
  (length
    (apply append
           (map (lambda (stone) (watch-stone 25 0 stone))
                (parse-input input-data)))))

(display (part-1 input-data))
(newline)

; Part 2

(define (reduce-stone max-step step stone)
  ; Check for (>= stone 1) to eliminate stones with value zero
  ; as these change each into a stone with value one in a single step.
  (if (or (>= step max-step) (and (< stone 10) (>= stone 1)))
    (list (cons step stone))
    (apply append
           (map (lambda (s) (reduce-stone max-step (+ step 1) s))
                (apply-rules stone)))))

(define (track-single-digit-stone step stone)
  (if (and (> step 0) (< stone 10))
    (list (cons step stone))
    (apply append
           (map (lambda (s) (track-single-digit-stone (+ step 1) s))
                (apply-rules stone)))))

(define single-digit-lookup-table
  (list->vector
    (map (lambda (digit)
           (track-single-digit-stone 0 digit))
         (iota 10))))

(define (create-steps-table max-step stones)
  (let ((steps-table (make-array 0 (+ max-step 1) 10)))
    (for-each (lambda (s)
                (array-set! steps-table
                            (+ 1 (array-ref steps-table (car s) (cdr s)))
                            (car s)
                            (cdr s)))
              stones)
    steps-table))

(define (fast-forward-pile! steps-table step digit)
  (let ((count (array-ref steps-table step digit))
        (moves (vector-ref single-digit-lookup-table digit)))
    (when (every (lambda (move) (array-in-bounds? steps-table (+ step (car move)) digit)) moves)
      (array-set! steps-table 0 step digit)
      (for-each (lambda (p)
                  (array-set! steps-table
                              (+ (array-ref steps-table (+ step (car p)) (cdr p))
                                 count)
                              (+ step (car p))
                              (cdr p)))
                moves))))

(define (fast-forward-stones original-steps-table)
  (let ((steps-table (array-copy original-steps-table)))
    (for-each
      (lambda (step)
        (for-each
          (lambda (digit)
            (when (not (= (array-ref steps-table step digit) 0))
              (fast-forward-pile! steps-table step digit)))
          (iota (cadr (array-dimensions steps-table)))))
      (iota (car (array-dimensions steps-table))))
    steps-table))

(define (steps-table->counts-list steps-table)
  ; ((step digit count) ...)
  (filter (lambda (p) (> (caddr p) 0))
          (apply append
                 (map
                   (lambda (step)
                     (map
                       (lambda (digit)
                         (list step digit (array-ref steps-table step digit)))
                       (iota (cadr (array-dimensions steps-table)))))
                   (iota (car (array-dimensions steps-table)))))))

(define (forward-counts max-step counts-list)
  (receive (completed pending)
           (partition (lambda (s) (= (car s) max-step))
                      counts-list)
           (if (null? pending)
             completed
             (append completed
                     (forward-counts
                       max-step
                       (apply append
                              (map (lambda (entry)
                                     (map (lambda (e)
                                            (list (+ (car entry) 1)
                                                  e
                                                  (caddr entry)))
                                          (apply-rules (cadr entry))))
                                   pending)))))))

(define (handle-stone max-step stone)
  (receive (completed pending)
           (partition (lambda (s) (= (car s) max-step))
                      (reduce-stone max-step 0 stone))
           (apply +
                  (length completed)
                  (map caddr
                       (forward-counts max-step
                                       (steps-table->counts-list
                                         (fast-forward-stones
                                           (create-steps-table max-step pending))))))))

(define (part-2 input-data)
  (apply +
         (map (lambda (stone) (handle-stone 75 stone))
              (parse-input input-data))))

(display (part-2 input-data))
(newline)
