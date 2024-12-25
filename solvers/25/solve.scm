(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 receive) #:select (receive)))
(use-modules ((srfi srfi-1) #:select (every partition)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (split-by-substring str delimiter)
  (if (or (string=? str "") (string=? delimiter ""))
    (error "Both the main string and the delimiter must be non-empty strings"))
  (let loop ((s str) (acc '()))
    (let ((pos (string-contains s delimiter)))
      (if pos
        (loop (substring s (+ pos (string-length delimiter)))
              (cons (substring s 0 pos) acc))
        (reverse (cons s acc))))))

(define (transpose-list-of-lists lst)
  (if (null? (car lst))
    '()
    (cons (map car lst)
          (transpose-list-of-lists (map cdr lst)))))

(define (parse-schematic raw-schematic)
  (let* ((lines (string-split raw-schematic #\newline))
         (heights
           (map (lambda (column)
                  (1-
                   (length
                     (filter (lambda (c) (eqv? c #\#)) column))))
                (transpose-list-of-lists
                  (map string->list lines)))))
    (cons
      (if (string=? (car lines) "#####") 'lock 'key)
      heights)))

(define (parse-input input-data)
  (let ((all-heights
          (map parse-schematic
               (split-by-substring (string-trim-right input-data #\linefeed) "\n\n"))))
    (receive (locks keys)
             (partition (lambda (s) (eq? (car s) 'lock))
                        all-heights)
             (values
               (map cdr locks)
               (map cdr keys)))))

(define (part-1 input-data)
  (receive (locks keys)
           (parse-input input-data)
           (length
             (filter identity
                     (apply append
                            (map (lambda (lock)
                                   (map (lambda (key)
                                          (every (lambda (c) (<= c 5))
                                                 (map + lock key)))
                                        keys))
                                 locks))))))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
