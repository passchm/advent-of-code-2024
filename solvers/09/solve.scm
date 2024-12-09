(use-modules ((ice-9 textual-ports) #:select (get-string-all)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; (define input-data "2333133121414131402")

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

; (display (part-1 input-data))
(write (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
