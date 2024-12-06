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
           (map (lambda (line) (map char->integer (string->list line)))
                lines)))
    (list->typed-array 'u8 2 raw-grid)))

(define (find-value grid value)
  (let ((num-rows (array-length grid)) (num-columns (cadr (array-dimensions grid))))
    (let loop ((r 0) (c 0) (indices '()))
      (if (>= r num-rows)
        (reverse indices)
        (if (>= c num-columns)
          (loop (+ r 1) 0 indices)
          (loop r (+ c 1)
                (if (= (array-ref grid r c) value)
                  (cons (cons r c) indices)
                  indices)))))))

(define obstruction
  (char->integer #\#))

(define (move-guard grid starting-position starting-direction)
  (let ((visited-positions (apply make-array #f (array-dimensions grid))))
    (let loop ((pos-r (car starting-position))
               (pos-c (cdr starting-position))
               (dir-r (car starting-direction))
               (dir-c (cdr starting-direction)))
      (if (array-in-bounds? grid pos-r pos-c)
        (begin
          (array-set! visited-positions #t pos-r pos-c)
          (if (and (array-in-bounds? grid (+ pos-r dir-r) (+ pos-c dir-c))
                   (= obstruction (array-ref grid (+ pos-r dir-r) (+ pos-c dir-c))))
            (let ((turned-dir-r dir-c) (turned-dir-c (* -1 dir-r)))
              (loop (+ pos-r turned-dir-r) (+ pos-c turned-dir-c) turned-dir-r turned-dir-c))
            (loop (+ pos-r dir-r) (+ pos-c dir-c) dir-r dir-c)))
        '()))
    visited-positions))

(define (part-1 input-data)
  (let* ((grid (parse-input input-data))
         (starting-position (car (find-value grid (char->integer #\^))))
         (starting-direction '(-1 . 0)))
    (length
      (filter identity
              (apply append (array->list
                              (move-guard grid starting-position starting-direction)))))))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
