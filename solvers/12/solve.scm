(use-modules ((ice-9 textual-ports) #:select (get-string-all)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (let* ((lines
           (string-split
             (string-trim-right input-data #\linefeed)
             #\linefeed))
         (raw-grid
           (map (lambda (line)
                  (string->list line))
                lines)))
    (list->array 2 raw-grid)))

(define (find-and-mark-region! grid seen region-value r c)
  (if (and (array-in-bounds? seen r c)
           (not (array-ref seen r c))
           (eqv? (array-ref grid r c) region-value))
    (begin
      (array-set! seen #t r c)
      (cons
        (cons r c)
        (append
          ; up
          (find-and-mark-region! grid seen region-value (- r 1) c)
          ; down
          (find-and-mark-region! grid seen region-value (+ r 1) c)
          ; left
          (find-and-mark-region! grid seen region-value r (- c 1))
          ; right
          (find-and-mark-region! grid seen region-value r (+ c 1)))))
    '()))

(define (find-regions grid)
  (let ((seen (apply make-array #f (array-shape grid)))
        (num-rows (car (array-dimensions grid)))
        (num-columns (cadr (array-dimensions grid))))
    (let loop ((r 0) (c 0) (regions '()))
      (if (>= r num-rows)
        (reverse (filter pair? regions))
        (if (>= c num-columns)
          (loop (+ r 1) 0 regions)
          (loop r (+ c 1)
                (cons
                  (find-and-mark-region! grid seen (array-ref grid r c) r c)
                  regions)))))))

(define (count-neighbors-in-region region garden-plot)
  (let ((r (car garden-plot)) (c (cdr garden-plot)))
    (length
      (filter identity
              (list
                ; up
                (member (cons (- r 1) c) region)
                ; down
                (member (cons (+ r 1) c) region)
                ; left
                (member (cons r (- c 1)) region)
                ; right
                (member (cons r (+ c 1)) region))))))

(define (calculate-perimeter region)
  (apply +
         (map (lambda (garden-plot)
                (- 4 (count-neighbors-in-region region garden-plot)))
              region)))

(define (part-1 input-data)
  (let ((grid (parse-input input-data)))
    (apply +
           (map (lambda (region)
                  (* (length region)
                     (calculate-perimeter region)))
                (find-regions grid)))))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
