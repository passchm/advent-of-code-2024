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
        (output #f))
    (case opcode
      ; adv
      ((0)
       (set! reg-a (ash reg-a (- combo)))
       (set! ip (+ ip 2)))
      ; bxl
      ((1)
       (set! reg-b (logxor reg-b operand))
       (set! ip (+ ip 2)))
      ; bst
      ((2)
       (set! reg-b (logand combo #b111))
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
       (set! output (logand combo #b111))
       (set! ip (+ ip 2)))
      ; bdv
      ((6)
       (set! reg-b (ash reg-a (- combo)))
       (set! ip (+ ip 2)))
      ; cdv
      ((7)
       (set! reg-c (ash reg-a (- combo)))
       (set! ip (+ ip 2)))
      (else
        (error "Bad opcode")))
    (values ip reg-a reg-b reg-c output)))

(define (run-program registers program)
  (let next ((ip 0)
             (reg-a (list-ref registers 0))
             (reg-b (list-ref registers 1))
             (reg-c (list-ref registers 2))
             (outputs '()))
    (if (< ip (vector-length program))
      (let ((opcode (vector-ref program ip))
            (operand (vector-ref program (+ ip 1))))
        (receive (new-ip new-reg-a new-reg-b new-reg-c new-output)
                 (eval-instruction ip opcode operand reg-a reg-b reg-c)
                 (next new-ip new-reg-a new-reg-b new-reg-c
                       (if new-output (cons new-output outputs) outputs))))
      (reverse outputs))))

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

(define (run-until-output program ip reg-a reg-b reg-c)
  (let next ((ip ip) (reg-a reg-a) (reg-b reg-b) (reg-c reg-c))
    (if (< ip (vector-length program))
      (let ((opcode (vector-ref program ip))
            (operand (vector-ref program (+ ip 1))))
        (receive (new-ip new-reg-a new-reg-b new-reg-c new-output)
                 (eval-instruction ip opcode operand reg-a reg-b reg-c)
                 (if new-output
                   (values new-ip new-reg-a new-reg-b new-reg-c new-output)
                   (next new-ip new-reg-a new-reg-b new-reg-c))))
      (values ip reg-a reg-b reg-c #f))))

(define (outputs-itself? program reg-a-init reg-b-init reg-c-init)
  (let loop ((output-index 0) (ip 0) (reg-a reg-a-init) (reg-b reg-b-init) (reg-c reg-c-init))
    (if (< output-index (vector-length program))
      (receive (new-ip new-reg-a new-reg-b new-reg-c output)
               (run-until-output program ip reg-a reg-b reg-c)
               (if (and output (= output (vector-ref program output-index)))
                 (loop (1+ output-index) new-ip new-reg-a new-reg-b new-reg-c)
                 #f))
      #t)))

(define (part-2 input-data)
  (receive (registers program)
           (parse-input input-data)
           (let ((reg-b (list-ref registers 1)) (reg-c (list-ref registers 2)))
             (let loop ((reg-a 0))
               (if (outputs-itself? program reg-a reg-b reg-c)
                 reg-a
                 (loop (1+ reg-a)))))))

(display (part-2 input-data))
(newline)
