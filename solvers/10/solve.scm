(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((srfi srfi-1) #:select (delete-duplicates)))

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
                  (map (lambda (c)
                         (- (char->integer c) (char->integer #\0)))
                       (string->list line)))
                lines)))
    (list->typed-array 'u8 2 raw-grid)))

(define (find-in-grid grid pred)
  (let ((num-rows (array-length grid)) (num-columns (cadr (array-dimensions grid))))
    (let loop ((r 0) (c 0) (indices '()))
      (if (>= r num-rows)
        (reverse indices)
        (if (>= c num-columns)
          (loop (+ r 1) 0 indices)
          (loop r (+ c 1)
                (if (pred (array-ref grid r c))
                  (cons (cons r c) indices)
                  indices)))))))

(define (move-higher grid n r c)
  (if (and (array-in-bounds? grid r c) (= (array-ref grid r c) n))
    (if (= n 9)
      (list (cons r c))
      (apply append
             (list
               ; up
               (move-higher grid (+ n 1) (- r 1) c)
               ; down
               (move-higher grid (+ n 1) (+ r 1) c)
               ; left
               (move-higher grid (+ n 1) r (- c 1))
               ; right
               (move-higher grid (+ n 1) r (+ c 1)))))
    '()))

(define (part-1 input-data)
  (let ((grid (parse-input input-data)))
    (apply +
           (map length
                (map (lambda (start-point)
                       (delete-duplicates
                         (move-higher grid 0 (car start-point) (cdr start-point))))
                     (find-in-grid grid (lambda (n) (= n 0))))))))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  (let ((grid (parse-input input-data)))
    (apply +
           (map length
                (map (lambda (start-point)
                       (move-higher grid 0 (car start-point) (cdr start-point)))
                     (find-in-grid grid (lambda (n) (= n 0))))))))

(display (part-2 input-data))
(newline)
