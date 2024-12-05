(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 receive) #:select (receive)))
(use-modules ((srfi srfi-1) #:select (partition)))

(add-to-load-path (dirname (current-filename)))
(use-modules ((topological-sort) #:select (topological-sort)))

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

(define (parse-input input-data)
  (apply
    (lambda (str-rules str-updates)
      (values
        (map (lambda (line) (apply cons (map string->number (string-split line #\|))))
             (string-split str-rules #\linefeed))
        (map (lambda (line) (map string->number (string-split line #\,)))
             (string-split str-updates #\linefeed))))
    (split-by-substring (string-trim-right input-data #\linefeed) "\n\n")))

(define (reshape-rules rules)
  (if (pair? rules)
    (let ((current-page (caar rules)))
      (receive (matching-rules remaining-rules)
               (partition (lambda (rule) (= (car rule) current-page)) rules)
               (cons
                 (cons current-page (map cdr matching-rules))
                 (reshape-rules remaining-rules))))
    '()))

; Attention: "within each update, the ordering rules that involve missing page numbers are not used"!
; The notation X|Y means that if both page number X and page number Y are to be produced as part of an update,
; page number X must be printed at some point before page number Y.
(define (does-rule-apply? rule update)
  (and (memv (car rule) update) (memv (cdr rule) update)))

(define (find-order-for-update update rules)
  (topological-sort
    (reshape-rules
      (filter (lambda (rule) (does-rule-apply? rule update)) rules))
    =))

(define (valid-update? update rules)
  (equal? update (find-order-for-update update rules)))

(define (middle-page-number update)
  (list-ref update (floor (/ (length update) 2))))

(define (part-1 input-data)
  (receive (rules updates)
           (parse-input input-data)
           (apply +
                  (map middle-page-number
                       (filter (lambda (update) (valid-update? update rules))
                               updates)))))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  (receive (rules updates)
           (parse-input input-data)
           (let* ((incorrectly-ordered-updates
                    (filter (lambda (update) (not (valid-update? update rules))) updates))
                  (corrected-updates
                    (map (lambda (update) (find-order-for-update update rules))
                         incorrectly-ordered-updates)))
             (apply +
                    (map middle-page-number corrected-updates)))))

(display (part-2 input-data))
(newline)
