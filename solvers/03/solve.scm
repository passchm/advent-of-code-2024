(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 peg) #:select (define-peg-pattern search-for-pattern peg:end peg:tree)))
(use-modules ((ice-9 match) #:select (match match-lambda)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define-peg-pattern num-pattern all (+ (range #\0 #\9)))

(define-peg-pattern mul-pattern all
                    (and
                      (ignore "mul(")
                      num-pattern
                      (ignore ",")
                      num-pattern
                      (ignore ")")))

(define (find-all-matches pattern str)
  (let loop ((matches '()) (start-index 0))
    (let ((match-rec (search-for-pattern pattern (substring str start-index))))
      (if match-rec
        (loop (cons (peg:tree match-rec) matches) (+ start-index (peg:end match-rec)))
        (reverse matches)))))

(define (eval-instructions instructions)
  (apply +
         (map (match-lambda
                (('mul-pattern ('num-pattern x) ('num-pattern y)) (* (string->number x) (string->number y))))
              instructions)))

(define (part-1 input-data)
  (eval-instructions (find-all-matches mul-pattern input-data)))

(display (part-1 input-data))
(newline)

; Part 2

(define-peg-pattern do-pattern body (or "do()" "don't()" mul-pattern))

(define (filter-instructions instructions)
  (let loop ((enabled #t) (enabled-instr '()) (remaining-instr instructions))
    (if (null? remaining-instr)
      enabled-instr
      (match (car remaining-instr)
             ("do()" (loop #t enabled-instr (cdr remaining-instr)))
             ("don't()" (loop #f enabled-instr (cdr remaining-instr)))
             (mul-instr (loop enabled
                              (if enabled (cons mul-instr enabled-instr) enabled-instr)
                              (cdr remaining-instr)))))))

(define (part-2 input-data)
  (eval-instructions
    (filter-instructions
      (find-all-matches do-pattern input-data))))

(display (part-2 input-data))
(newline)
