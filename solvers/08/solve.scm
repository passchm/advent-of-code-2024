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
           (map (lambda (line) (map char->integer (string->list line)))
                lines)))
    (list->typed-array 'u8 2 raw-grid)))

(define (find-frequencies grid)
  (delete-duplicates
    (sort-list
      (filter (lambda (c) (not (= c (char->integer #\.))))
              (apply append (array->list grid)))
      <)))

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

(define (calculate-antinodes antenna-1 antenna-2)
  (let ((delta-r (- (car antenna-2) (car antenna-1)))
        (delta-c (- (cdr antenna-2) (cdr antenna-1))))
    (list (cons (- (car antenna-1) delta-r) (- (cdr antenna-1) delta-c))
          (cons (+ (car antenna-2) delta-r) (+ (cdr antenna-2) delta-c)))))

(define (find-antinodes grid frequency)
  (let ((antennas (find-in-grid grid (lambda (c) (= c frequency)))))
    (apply append
           (map
             (lambda (antenna-1)
               (apply append
                      (map (lambda (antenna-2)
                             (if (equal? antenna-1 antenna-2)
                               '()
                               (calculate-antinodes antenna-1 antenna-2)))
                           antennas)))
             antennas))))

(define (position-in-grid? grid r c)
  (array-in-bounds? grid r c))

(define (part-1 input-data)
  (let* ((grid (parse-input input-data))
         (frequencies (find-frequencies grid)))
    (length
      (filter (lambda (pos) (position-in-grid? grid (car pos) (cdr pos)))
              (delete-duplicates
                (apply append
                       (map (lambda (frequency) (find-antinodes grid frequency)) frequencies)))))))

(display (part-1 input-data))
(newline)

; Part 2

(define (diagonal-length-of-grid grid)
  (let ((rows (car (array-dimensions grid)))
        (columns (cadr (array-dimensions grid))))
    (sqrt (+ (* rows rows) (* columns columns)))))

(define (calculate-extended-antinodes grid antenna-1 antenna-2)
  (let ((diagonal-length (+ (inexact->exact (floor (diagonal-length-of-grid grid))) 1))
        (delta-r (- (car antenna-2) (car antenna-1)))
        (delta-c (- (cdr antenna-2) (cdr antenna-1))))
    (filter (lambda (pos) (position-in-grid? grid (car pos) (cdr pos)))
            (apply append
                   (map (lambda (i)
                          (list
                            (cons (- (car antenna-1) (* delta-r i)) (- (cdr antenna-1) (* delta-c i)))
                            (cons (+ (car antenna-2) (* delta-r i)) (+ (cdr antenna-2) (* delta-c i)))))
                        (iota diagonal-length))))))

(define (find-extended-antinodes grid frequency)
  (let ((antennas (find-in-grid grid (lambda (c) (= c frequency)))))
    (apply append
           (map
             (lambda (antenna-1)
               (apply append
                      (map (lambda (antenna-2)
                             (if (equal? antenna-1 antenna-2)
                               '()
                               (calculate-extended-antinodes grid antenna-1 antenna-2)))
                           antennas)))
             antennas))))

(define (part-2 input-data)
  (let* ((grid (parse-input input-data))
         (frequencies (find-frequencies grid)))
    (length
      (filter (lambda (pos) (position-in-grid? grid (car pos) (cdr pos)))
              (delete-duplicates
                (apply append
                       (map (lambda (frequency) (find-extended-antinodes grid frequency)) frequencies)))))))

(display (part-2 input-data))
(newline)
