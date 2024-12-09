(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((srfi srfi-1) #:select (fold)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (list->vector (map (lambda (c) (- (char->integer c) (char->integer #\0)))
                     (string->list
                       (string-trim-right input-data #\linefeed)))))

(define (calculate-filesystem-size disk-map)
  (apply + (vector->list disk-map)))

(define (disk-map->filesystem-data disk-map)
  (let ((filesystem-data (make-vector (calculate-filesystem-size disk-map) -1))
        (disk-map-length (vector-length disk-map)))
    (let loop ((id-number 0) (block-offset 0))
      (if (< block-offset (vector-length filesystem-data))
        (let ((file-length (vector-ref disk-map (* 2 id-number)))
              (free-space-length (if (< (+ (* 2 id-number) 1) disk-map-length)
                                   (vector-ref disk-map (+ (* 2 id-number) 1))
                                   0)))
          (vector-fill! filesystem-data id-number block-offset (+ block-offset file-length))
          (loop (+ id-number 1) (+ block-offset file-length free-space-length)))
        filesystem-data))))

(define (compact-filesystem filesystem-data)
  (let ((compact-data (make-vector (vector-length filesystem-data) -1)))
    (let loop ((left-index 0) (right-index (- (vector-length filesystem-data) 1)))
      (if (> left-index right-index)
        compact-data
        (if (not (= (vector-ref filesystem-data left-index) -1))
          (begin
            (vector-set! compact-data left-index (vector-ref filesystem-data left-index))
            (loop (+ left-index 1) right-index))
          (if (not (= (vector-ref filesystem-data right-index) -1))
            (begin
              (vector-set! compact-data left-index (vector-ref filesystem-data right-index))
              (loop (+ left-index 1) (- right-index 1)))
            (loop left-index (- right-index 1))))))))

(define (calculate-filesystem-checksum filesystem-data)
  (apply +
         (map
           (lambda (position)
             (if (= (vector-ref filesystem-data position) -1)
               0
               (* position (vector-ref filesystem-data position))))
           (iota (vector-length filesystem-data)))))

(define (part-1 input-data)
  (let ((disk-map (parse-input input-data)))
    (calculate-filesystem-checksum
      (compact-filesystem
        (disk-map->filesystem-data disk-map)))))

(display (part-1 input-data))
(newline)

; Part 2

(define (find-free-space-of-size filesystem-data file-size)
  (let loop ((i 0))
    (if (>= (+ i file-size) (vector-length filesystem-data))
      #f
      (if (and (= (vector-ref filesystem-data i) -1)
               (equal? (vector-copy filesystem-data i (+ i file-size))
                       (make-vector file-size -1)))
        i
        (loop (+ i 1))))))

(define (find-file-with-id-number filesystem-data id-number)
  (let ((found-file-range
          (let loop ((i 0) (file-range '()))
            (if (>= i (vector-length filesystem-data))
              file-range
              (if (= (vector-ref filesystem-data i) id-number)
                (loop (+ i 1) (cons i file-range))
                (if (null? file-range)
                  (loop (+ i 1) file-range)
                  file-range))))))
    (cons (car (reverse found-file-range))
          (+ 1 (- (car found-file-range) (car (reverse found-file-range)))))))

(define (move-file! filesystem-data file-size from-offset to-offset)
  (let ((data (vector-copy filesystem-data from-offset (+ from-offset file-size))))
    (vector-fill! filesystem-data -1 from-offset (+ from-offset file-size))
    (vector-copy! filesystem-data to-offset data)))

(define (move-file-left-if-possible! filesystem-data id-number)
  (let* ((file-range (find-file-with-id-number filesystem-data id-number))
         (file-offset (car file-range))
         (file-size (cdr file-range))
         (free-space-offset (find-free-space-of-size filesystem-data file-size)))
    (when (and free-space-offset (< free-space-offset file-offset))
      (move-file! filesystem-data file-size file-offset free-space-offset))))

(define (max-in-list lst)
  (fold (lambda (x y) (if (> x y) x y)) (car lst) (cdr lst)))

(define (compact-filesystem-whole-files filesystem-data)
  (let ((compact-data (vector-copy filesystem-data))
        (max-id-number (max-in-list (vector->list filesystem-data))))
    (for-each
      (lambda (id-number)
        (move-file-left-if-possible! compact-data id-number))
      (iota (+ max-id-number 1) max-id-number -1))
    compact-data))

(define (part-2 input-data)
  (let ((disk-map (parse-input input-data)))
    (calculate-filesystem-checksum
      (compact-filesystem-whole-files
        (disk-map->filesystem-data disk-map)))))

(display (part-2 input-data))
(newline)
