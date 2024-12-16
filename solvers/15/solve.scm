(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 receive) #:select (receive)))
(use-modules ((ice-9 arrays) #:select (array-copy)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (let ((split-index (string-contains input-data "\n\n")))
    (values
      (list->array
        2
        (map string->list
             (string-split
               (substring/read-only input-data 0 split-index)
               #\newline)))
      (list->vector
        (string->list
          (string-delete
            #\newline
            (substring/read-only input-data (+ split-index 2))))))))

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

(define (attempt-move! grid pos dir)
  (let* ((target-pos (cons (+ (car pos) (car dir))
                           (+ (cdr pos) (cdr dir))))
         (new-pos
           (case (array-ref grid (car target-pos) (cdr target-pos))
             ((#\#) pos)
             ((#\.) target-pos)
             ((#\O)
              (let ((new-box-pos (attempt-move! grid target-pos dir)))
                (cons (- (car new-box-pos) (car dir))
                      (- (cdr new-box-pos) (cdr dir)))))
             (else (error "Bad grid character"))))
         (old-char (array-ref grid (car pos) (cdr pos))))
    (when (not (equal? new-pos pos))
      (array-set! grid #\. (car pos) (cdr pos))
      (array-set! grid old-char (car new-pos) (cdr new-pos)))
    new-pos))

(define (move-around initial-grid initial-position moves)
  (let ((grid (array-copy initial-grid)))
    (let loop ((robot-position initial-position) (move-index 0))
      (if (< move-index (vector-length moves))
        (loop
          (attempt-move! grid
                         robot-position
                         (case (vector-ref moves move-index)
                           ((#\^) '(-1 . 0))
                           ((#\v) '(1 . 0))
                           ((#\<) '(0 . -1))
                           ((#\>) '(0 . 1))))
          (+ move-index 1))))
    grid))

(define (print-grid grid)
  (let ((num-rows (array-length grid)) (num-columns (cadr (array-dimensions grid))))
    (let loop ((r 0) (c 0))
      (when (< r num-rows)
        (if (>= c num-columns)
          (begin
            (display "\n")
            (loop (+ r 1) 0))
          (begin
            (display (array-ref grid r c))
            (loop r (+ c 1))))))))

(define (sum-gps-coordinates char grid)
  (apply +
         (map (lambda (box)
                (+ (* (car box) 100) (cdr box)))
              (find-in-grid grid (lambda (c) (eqv? c char))))))

(define (part-1 input-data)
  (receive (grid moves)
           (parse-input input-data)
           (let ((initial-position (car (find-in-grid grid (lambda (c) (eqv? c #\@))))))
             (sum-gps-coordinates #\O
                                  (move-around grid initial-position moves)))))

(display (part-1 input-data))
(newline)

; Part 2

(define (scale-grid unscaled-grid)
  (list->array
    2
    (map
      (lambda (row)
        (apply append
               (map
                 (lambda (c)
                   (case c
                     ((#\#) '(#\# #\#))
                     ((#\O) '(#\[ #\]))
                     ((#\.) '(#\. #\.))
                     ((#\@) '(#\@ #\.))))
                 row)))
      (array->list unscaled-grid))))

(define (positions-to-move grid pos dir)
  ; Returns the list of positions that must move such that the current position can be moved.
  (let ((target-pos (cons (+ (car pos) (car dir)) (+ (cdr pos) (cdr dir)))))
    (case (array-ref grid (car target-pos) (cdr target-pos))
      ((#\#) (list target-pos))
      ((#\.) '())
      ((#\[) (if (= (car dir) 0)
               ; Horizontal move
               (cons target-pos (positions-to-move grid target-pos dir))
               ; Vertical move
               (cons target-pos
                     (append
                       ; [
                       (positions-to-move grid target-pos dir)
                       ; ]
                       (list (cons (car target-pos) (+ (cdr target-pos) 1)))
                       (positions-to-move
                         grid
                         (cons (car target-pos) (+ (cdr target-pos) 1))
                         dir)))))
      ((#\]) (if (= (car dir) 0)
               ; Horizontal move
               (cons target-pos (positions-to-move grid target-pos dir))
               ; Vertical move
               (cons
                 target-pos
                 (append
                   ; [
                   (list (cons (car target-pos) (- (cdr target-pos) 1)))
                   (positions-to-move
                     grid
                     (cons (car target-pos) (- (cdr target-pos) 1))
                     dir)
                   ; ]
                   (positions-to-move grid target-pos dir))))))))

(define (move-all-at-once! grid targets dir)
  (let ((old-chars
          (map-in-order (lambda (pos) (array-ref grid (car pos) (cdr pos))) targets)))
    (for-each
      (lambda (pos)
        (array-set! grid #\. (car pos) (cdr pos)))
      targets)
    (for-each
      (lambda (pos old-char)
        (array-set! grid
                    old-char
                    (+ (car pos) (car dir))
                    (+ (cdr pos) (cdr dir))))
      targets
      old-chars)))

(define (attempt-wide-move! grid pos dir)
  (let ((targets (positions-to-move grid pos dir)))
    (if (null?
          (filter (lambda (target) (eqv? (array-ref grid (car target) (cdr target)) #\#)) targets))
      ; Valid move
      (begin
        (move-all-at-once! grid (cons pos targets) dir)
        (cons (+ (car pos) (car dir)) (+ (cdr pos) (cdr dir))))
      ; Invalid move
      pos)))

(define (move-wide-things-around initial-grid initial-position moves)
  (let ((grid (array-copy initial-grid)))
    (let loop ((robot-position initial-position) (move-index 0))
      (if (< move-index (vector-length moves))
        (loop
          (attempt-wide-move!
            grid
            robot-position
            (case (vector-ref moves move-index)
              ((#\^) '(-1 . 0))
              ((#\v) '(1 . 0))
              ((#\<) '(0 . -1))
              ((#\>) '(0 . 1))))
          (+ move-index 1))))
    grid))

(define (part-2 input-data)
  (receive (unscaled-grid moves)
           (parse-input input-data)
           (let* ((scaled-grid (scale-grid unscaled-grid))
                  (initial-position (car (find-in-grid scaled-grid (lambda (c) (eqv? c #\@)))))
                  (final-grid
                    (move-wide-things-around scaled-grid initial-position moves)))
             (sum-gps-coordinates #\[ final-grid))))

(display (part-2 input-data))
(newline)
