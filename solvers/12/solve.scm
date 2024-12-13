(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((srfi srfi-9 gnu) #:select (define-immutable-record-type)))

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
        (min-row (car (car (array-shape grid))))
        (max-row (cadr (car (array-shape grid))))
        (min-column (car (cadr (array-shape grid))))
        (max-column (cadr (cadr (array-shape grid)))))
    (let loop ((r min-row) (c min-column) (regions '()))
      (if (> r max-row)
        (reverse (filter pair? regions))
        (if (> c max-column)
          (loop (+ r 1) min-column regions)
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

(define-immutable-record-type
  <gray>
  (gray r c dr dc)
  gray?
  (r gray-r set-gray-r)
  (c gray-c set-gray-c)
  (dr gray-dr set-gray-dr)
  (dc gray-dc set-gray-dc))

(define (rays-for-garden-plot region garden-plot)
  ;      2
  ; 3<---^
  ;  | 0 |
  ;  v--->1
  ;  4
  (let ((r (car garden-plot)) (c (cdr garden-plot)))
    (append
      ; 1
      (if (not (member (cons (+ r 1) c) region))
        (list (gray r c 0 1))
        '())
      ; 2
      (if (not (member (cons r (+ c 1)) region))
        (list (gray r c -1 0))
        '())
      ; 3
      (if (not (member (cons (- r 1) c) region))
        (list (gray r c 0 -1))
        '())
      ; 4
      (if (not (member (cons r (- c 1)) region))
        (list (gray r c 1 0))
        '()))))

(define (gray-pos ray)
  (cons (gray-r ray) (gray-c ray)))

(define (group-by-dir rays)
  (map (lambda (dir)
         (map gray-pos
              (filter (lambda (ray)
                        (and (= (gray-dr ray) (car dir)) (= (gray-dc ray) (cdr dir))))
                      rays)))
       '((0 . 1) (-1 . 0) (0 . -1) (1 . 0))))

(define (group-neighbors positions)
  (let* ((min-row (apply min (map car positions)))
         (max-row (apply max (map car positions)))
         (min-column (apply min (map cdr positions)))
         (max-column (apply max (map cdr positions)))
         (simple-grid (make-array #f (list min-row (+ max-row 1)) (list min-column (+ max-column 1)))))
    (for-each (lambda (pos) (array-set! simple-grid #t (car pos) (cdr pos))) positions)
    (filter (lambda (region)
              (array-ref simple-grid (caar region) (cdar region)))
            (find-regions simple-grid))))

(define (calculate-number-of-sides region)
  (apply +
         (map (compose length group-neighbors)
              (group-by-dir
                (apply append
                       (map (lambda (garden-plot)
                              (rays-for-garden-plot region garden-plot))
                            region))))))

(define (part-2 input-data)
  (let ((grid (parse-input input-data)))
    (apply +
           (map (lambda (region)
                  (* (length region)
                     (calculate-number-of-sides region)))
                (find-regions grid)))))

(display (part-2 input-data))
(newline)
