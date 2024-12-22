(use-modules ((ice-9 textual-ports) #:select (get-string-all)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (map string->number
       (string-split
         (string-trim-right input-data #\linefeed)
         #\linefeed)))

(define (shift-mix-prune shift secret-number)
  (logand
    (logxor (ash secret-number shift) secret-number)
    (1- (ash 1 24))))

(define (evolve-secret-number secret-number)
  (shift-mix-prune 11
                   (shift-mix-prune -5
                                    (shift-mix-prune 6 secret-number))))

(define (simulate-secret-numbers initial-secret-number steps)
  (let loop ((step 0) (secret-number initial-secret-number))
    (if (= step steps)
      secret-number
      (loop (1+ step) (evolve-secret-number secret-number)))))

(define (part-1 input-data)
  (apply +
         (map (lambda (secret-number) (simulate-secret-numbers secret-number 2000))
              (parse-input input-data))))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
