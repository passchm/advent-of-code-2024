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

(define (valid-word? grid word r-coord c-coord r-dir c-dir)
  (let loop ((r r-coord) (c c-coord) (i 0))
    (if (= i (string-length word))
      #t
      (if (and (array-in-bounds? grid r c)
               (= (array-ref grid r c) (char->integer (string-ref word i))))
        (loop (+ r r-dir) (+ c c-dir) (+ i 1))
        #f))))

(define (part-1 input-data)
  (let ((grid (parse-input input-data)))
    (length
      (apply append
             (map
               (lambda (start)
                 (filter (lambda (dir)
                           (valid-word? grid "XMAS" (car start) (cdr start) (car dir) (cdr dir)))
                         '((-1 . -1) (-1 . 0) (-1 . 1) (0 . -1) (0 . 1) (1 . -1) (1 . 0) (1 . 1))))
               (find-value grid (char->integer #\X)))))))

(display (part-1 input-data))
(newline)

; Part 2

(define (get-relative-char grid r-coord c-coord r-dir c-dir)
  (if (array-in-bounds? grid (+ r-coord r-dir) (+ c-coord c-dir))
    (integer->char (array-ref grid (+ r-coord r-dir) (+ c-coord c-dir)))
    #\.))

(define (get-cross-neighbors grid row column)
  (list->string
    (list
      (get-relative-char grid row column -1 -1)
      (get-relative-char grid row column -1 +1)
      (get-relative-char grid row column +1 -1)
      (get-relative-char grid row column +1 +1))))

(define (part-2 input-data)
  (let ((grid (parse-input input-data)))
    (length (filter (lambda (coord)
                      (string-contains "MMSS MSMS SMSM SSMM"
                                       (get-cross-neighbors grid (car coord) (cdr coord))))
                    (find-value grid (char->integer #\A))))))

(display (part-2 input-data))
(newline)
