(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 receive) #:select (receive)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (let* ((raw-entries
           (map (lambda (line)
                  (if (> (string-length line) 0)
                    (map (lambda (num) (string->number (string-trim num #\space)))
                         (string-split
                           (cadr (string-split line #\:))
                           #\,))
                    '()))
                (string-split
                  (string-trim-right input-data #\linefeed)
                  #\linefeed)))
         (registers
           (map car (list-head raw-entries 3)))
         (program
           (list->vector (car (last-pair raw-entries)))))
    (values registers program)))

(define (eval-instruction ip opcode operand reg-a reg-b reg-c)
  (let ((combo
          (case operand ((0) 0) ((1) 1) ((2) 2) ((3) 3) ((4) reg-a) ((5) reg-b) ((6) reg-c) (else #f)))
        (outputs '()))
    (case opcode
      ; adv
      ((0)
       (set! reg-a (truncate-quotient reg-a (expt 2 combo)))
       (set! ip (+ ip 2)))
      ; bxl
      ((1)
       (set! reg-b (logxor reg-b operand))
       (set! ip (+ ip 2)))
      ; bst
      ((2)
       (set! reg-b (euclidean-remainder combo 8))
       (set! ip (+ ip 2)))
      ; jnz
      ((3)
       (set! ip (if (= reg-a 0) (+ ip 2) operand)))
      ; bxc
      ((4)
       (set! reg-b (logxor reg-b reg-c))
       (set! ip (+ ip 2)))
      ; out
      ((5)
       (set! outputs (cons (euclidean-remainder combo 8) outputs))
       (set! ip (+ ip 2)))
      ; bdv
      ((6)
       (set! reg-b (truncate-quotient reg-a (expt 2 combo)))
       (set! ip (+ ip 2)))
      ; cdv
      ((7)
       (set! reg-c (truncate-quotient reg-a (expt 2 combo)))
       (set! ip (+ ip 2)))
      (else
        (error "Bad opcode")))
    (values ip reg-a reg-b reg-c outputs)))

(define (run-program registers program)
  (let next ((ip 0)
             (reg-a (list-ref registers 0))
             (reg-b (list-ref registers 1))
             (reg-c (list-ref registers 2))
             (outputs '()))
    (if (< ip (vector-length program))
      (let ((opcode (vector-ref program ip))
            (operand (vector-ref program (+ ip 1))))
        (receive (new-ip new-reg-a new-reg-b new-reg-c new-outputs)
                 (eval-instruction ip opcode operand reg-a reg-b reg-c)
                 (next new-ip new-reg-a new-reg-b new-reg-c (append outputs new-outputs))))
      outputs)))

(define (part-1 input-data)
  (receive (registers program)
           (parse-input input-data)
           (string-join
             (map number->string
                  (run-program registers program))
             ",")))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
