(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 receive) #:select (receive)))
(use-modules ((srfi srfi-1) #:select (any)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (let ((lines
          (string-split
            (string-trim-right input-data #\linefeed)
            #\linefeed)))
    (values
      (map string-trim (string-split (car lines) #\,))
      (cddr lines))))

(define (possible-design? desired-design towel-patterns)
  (if (string=? desired-design "")
    #t
    (any (lambda (pattern)
           (if (string-prefix? pattern desired-design)
             (possible-design?
               (substring/read-only desired-design (string-length pattern))
               towel-patterns)
             #f))
         towel-patterns)))

(define (part-1 input-data)
  (receive (towel-patterns desired-designs)
           (parse-input input-data)
           (length
             (filter (lambda (design) (possible-design? design towel-patterns))
                     desired-designs))))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
