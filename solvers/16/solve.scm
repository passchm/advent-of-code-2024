(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((srfi srfi-9 gnu) #:select (define-immutable-record-type)))

(add-to-load-path (dirname (current-filename)))
(use-modules ((dijkstra) #:select (dijkstra-find-best-paths)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (list->array
    2
    (map string->list
         (string-split
           (string-trim-right input-data #\linefeed)
           #\newline))))

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

(define-immutable-record-type
  <gray>
  (gray r c dr dc)
  gray?
  (r gray-r set-gray-r)
  (c gray-c set-gray-c)
  (dr gray-dr set-gray-dr)
  (dc gray-dc set-gray-dc))

(define (possible-moves grid current-ray)
  (append
    ; move forward one tile
    (let ((forward-ray (gray (+ (gray-r current-ray) (gray-dr current-ray))
                             (+ (gray-c current-ray) (gray-dc current-ray))
                             (gray-dr current-ray)
                             (gray-dc current-ray))))
      (if (eqv? (array-ref grid (gray-r forward-ray) (gray-c forward-ray)) #\.)
        (list (cons 1 forward-ray))
        '()))
    ; rotate clockwise by 90 degrees
    (list
      (cons
        1000
        (gray (gray-r current-ray)
              (gray-c current-ray)
              (gray-dc current-ray)
              (- (gray-dr current-ray)))))
    ; rotate counterclockwise by 90 degrees
    (list
      (cons
        1000
        (gray (gray-r current-ray)
              (gray-c current-ray)
              (- (gray-dc current-ray))
              (gray-dr current-ray))))))

(define (score-path path)
  (let loop ((score 0) (step (car path)) (steps (cdr path)))
    (if (null? steps)
      score
      (loop
        (+ score
           (if (and (= (gray-dr step) (gray-dr (car steps)))
                    (= (gray-dc step) (gray-dc (car steps))))
             1
             1000))
        (car steps)
        (cdr steps)))))

(define (find-one-of-many-best-paths grid start-tile end-tile)
  (let* ((candidate-paths
           (map-in-order
             (lambda (end-tile-dir)
               (car
                 (dijkstra-find-best-paths
                   (gray (car start-tile) (cdr start-tile) 0 1)
                   (gray (car end-tile) (cdr end-tile) (car end-tile-dir) (cdr end-tile-dir))
                   (lambda (current-ray) (possible-moves grid current-ray)))))
             '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))))
         (scores
           (map-in-order score-path candidate-paths))
         (min-score (apply min scores)))
    (car
      (filter (lambda (path) (= (score-path path) min-score))
              candidate-paths))))

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

(define (part-1 input-data)
  (let* ((grid (parse-input input-data))
         (start-tile (car (find-in-grid grid (lambda (c) (eqv? c #\S)))))
         (end-tile (car (find-in-grid grid (lambda (c) (eqv? c #\E))))))
    (array-set! grid #\. (car start-tile) (cdr start-tile))
    (array-set! grid #\. (car end-tile) (cdr end-tile))
    (score-path
      (find-one-of-many-best-paths grid start-tile end-tile))))

(display (part-1 input-data))
(newline)

; Part 2

(define (find-all-best-paths grid start-tile end-tile)
  (let* ((candidate-paths
           (apply append
                  (map-in-order
                    (lambda (end-tile-dir)
                      (dijkstra-find-best-paths
                        (gray (car start-tile) (cdr start-tile) 0 1)
                        (gray (car end-tile) (cdr end-tile) (car end-tile-dir) (cdr end-tile-dir))
                        (lambda (current-ray) (possible-moves grid current-ray))))
                    '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))))
         (scores
           (map-in-order score-path candidate-paths))
         (min-score (apply min scores)))
    (filter (lambda (path) (= (score-path path) min-score))
            candidate-paths)))

(define (part-2 input-data)
  (let* ((grid (parse-input input-data))
         (start-tile (car (find-in-grid grid (lambda (c) (eqv? c #\S)))))
         (end-tile (car (find-in-grid grid (lambda (c) (eqv? c #\E))))))
    (array-set! grid #\. (car start-tile) (cdr start-tile))
    (array-set! grid #\. (car end-tile) (cdr end-tile))
    (let ((best-paths (find-all-best-paths grid start-tile end-tile)))
      (for-each
        (lambda (path)
          (for-each
            (lambda (step) (array-set! grid #\O (gray-r step) (gray-c step)))
            path))
        best-paths)
      (length (find-in-grid grid (lambda (c) (eqv? c #\O)))))))

(display (part-2 input-data))
(newline)
