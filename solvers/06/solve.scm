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
    (let loop ((r (car starting-position))
               (c (cdr starting-position))
               (dir-r (car starting-direction))
               (dir-c (cdr starting-direction)))
      (if (array-in-bounds? grid r c)
        (begin
          (array-set! visited-positions #t r c)
          (if (and (array-in-bounds? grid (+ r dir-r) (+ c dir-c))
                   (= obstruction (array-ref grid (+ r dir-r) (+ c dir-c))))
            (let ((turned-dir-r dir-c) (turned-dir-c (* -1 dir-r)))
              (loop (+ r turned-dir-r) (+ c turned-dir-c) turned-dir-r turned-dir-c))
            (loop (+ r dir-r) (+ c dir-c) dir-r dir-c)))
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

(define-immutable-record-type
  <gray>
  (gray r c dr dc)
  gray?
  (r gray-r set-gray-r)
  (c gray-c set-gray-c)
  (dr gray-dr set-gray-dr)
  (dc gray-dc set-gray-dc))

(define (turn-right ray)
  (gray
    (gray-r ray)
    (gray-c ray)
    (gray-dc ray)
    (* -1 (gray-dr ray))))

(define (move-forward ray)
  (gray
    (+ (gray-r ray) (gray-dr ray))
    (+ (gray-c ray) (gray-dc ray))
    (gray-dr ray)
    (gray-dc ray)))

(define (next-tile-obstructed? grid ray)
  (let ((next-state (move-forward ray)))
    (and (array-in-bounds? grid (gray-r next-state) (gray-c next-state))
         (= obstruction (array-ref grid (gray-r next-state) (gray-c next-state))))))

(define (position-in-grid? grid ray)
  (array-in-bounds? grid (gray-r ray) (gray-c ray)))

(define (hash-set-add! hash-set element)
  (hash-set! hash-set element #t))

(define (hash-set-contains? hash-set element)
  (hash-ref hash-set element #f))

(define (trapped-in-loop? grid initial-ray)
  (let ((taken-rays (make-hash-table)))
    (let loop ((ray initial-ray))
      (if (hash-set-contains? taken-rays ray)
        #t
        (begin
          (hash-set-add! taken-rays ray)
          (if (not (position-in-grid? grid ray))
            #f
            (if (next-tile-obstructed? grid ray)
              (loop (turn-right ray))
              (loop (move-forward ray)))))))))

; Optimization: A loop which gets the guard stuck must "start" somewhere on the path of the guard.

(define (find-in-grid grid pred)
  (let ((num-rows (array-length grid)) (num-columns (cadr (array-dimensions grid))))
    (let loop ((r 0) (c 0) (indices '()))
      (if (>= r num-rows)
        (reverse indices)
        (if (>= c num-columns)
          (loop (+ r 1) 0 indices)
          (loop r (+ c 1)
                (if (pred r c (array-ref grid r c))
                  (cons (cons r c) indices)
                  indices)))))))

(define (find-obstruction-loops grid starting-position starting-direction)
  (let ((starting-ray (gray (car starting-position) (cdr starting-position)
                            (car starting-direction) (cdr starting-direction))))
    (find-in-grid
      grid
      (lambda (r c value)
        (if (or (= value obstruction) (equal? (cons r c) starting-position))
          #f
          (begin
            (array-set! grid obstruction r c)
            (let ((trapped (trapped-in-loop? grid starting-ray)))
              (array-set! grid (char->integer #\.) r c)
              trapped)))))))

(define (part-2 input-data)
  (let* ((grid (parse-input input-data))
         (starting-position (car (find-value grid (char->integer #\^))))
         (starting-direction '(-1 . 0)))
    (length
      (find-obstruction-loops grid starting-position starting-direction))))

(display (part-2 input-data))
(newline)
