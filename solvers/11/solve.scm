(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 receive) #:select (receive)))

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

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
