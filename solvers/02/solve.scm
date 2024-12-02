(use-modules ((ice-9 textual-ports) #:select (get-string-all)))

(use-modules ((srfi srfi-1) #:select (any every)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Parse

(define (parse-input input-data)
  (map (lambda (line)
         (map string->number (string-split line #\space)))
       (string-split
         (string-trim-right input-data #\linefeed)
         #\linefeed)))

; Part 1

(define (adjacent-differences lst)
  (if (null? lst)
    '()
    (let loop ((curr (car lst)) (rest (cdr lst)))
      (if (null? rest)
        '()
        (cons
          (- (car rest) curr)
          (loop (car rest) (cdr rest)))))))

(define (safe-report? levels)
  (let ((diffs (adjacent-differences levels)))
    (and
      (or
        (every (lambda (elem) (> 0 elem)) diffs)
        (every (lambda (elem) (< 0 elem)) diffs))
      (every (lambda (elem) (and (>= (abs elem) 1) (<= (abs elem) 3))) diffs))))

(define (part-1 input-data)
  (let ((reports (parse-input input-data)))
    (length (filter safe-report? reports))))

(display (part-1 input-data))
(newline)

; Part 2

(define (tolerated-report? levels)
  (any identity
       (let loop ((left-list '()) (right-list levels))
         (if (null? right-list)
           '()
           (cons
             (safe-report? (append left-list (cdr right-list)))
             (loop
               (append left-list (list (car right-list)))
               (cdr right-list)))))))

(define (part-2 input-data)
  (let ((reports (parse-input input-data)))
    (length
      (filter
        (lambda (report) (or (safe-report? report) (tolerated-report? report)))
        reports))))

(display (part-2 input-data))
(newline)
