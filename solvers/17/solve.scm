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

(define (run-program program reg-a reg-b reg-c)
  (let next ((ip 0)
             (reg-a reg-a)
             (reg-b reg-b)
             (reg-c reg-c)
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
                  (run-program
                    program
                    (list-ref registers 0)
                    (list-ref registers 1)
                    (list-ref registers 2)))
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
                   (values new-output new-ip new-reg-a new-reg-b new-reg-c)
                   (next new-ip new-reg-a new-reg-b new-reg-c))))
      (values #f ip reg-a reg-b reg-c))))

(define (outputs-itself? program reg-a-init reg-b-init reg-c-init)
  (let loop ((output-index 0) (ip 0) (reg-a reg-a-init) (reg-b reg-b-init) (reg-c reg-c-init))
    (if (< output-index (vector-length program))
      (receive (output new-ip new-reg-a new-reg-b new-reg-c)
               (run-until-output program ip reg-a reg-b reg-c)
               (if (and output (= output (vector-ref program output-index)))
                 (loop (1+ output-index) new-ip new-reg-a new-reg-b new-reg-c)
                 #f))
      #t)))

(define (brute-force-solution input-data)
  (receive (registers program)
           (parse-input input-data)
           (let ((reg-b (list-ref registers 1)) (reg-c (list-ref registers 2)))
             (let loop ((reg-a 0))
               (if (outputs-itself? program reg-a reg-b reg-c)
                 reg-a
                 (loop (1+ reg-a)))))))

; This solution only works for my input program.
;
; The input program (translated into C syntax):
;
; #0 :: 2,4 :: bst :: B = A & 0b111
; #1 :: 1,3 :: bxl :: B = B ^ 0b011
; #2 :: 7,5 :: cdv :: C = A >> B
; #3 :: 4,2 :: bxc :: B = B ^ C
; #4 :: 0,3 :: adv :: A = A >> 3
; #5 :: 1,5 :: bxl :: B = B ^ 0b101
; #6 :: 5,5 :: out :: OUT = B & 0b111
; #7 :: 3,0 :: jnz 0
;
; Using Boolean algebra, the following observations can be made:
; In #0, B is set to a 3-bit number.
; The XOR in #1 does not change the number of bits, so B is still a 3-bit number.
; For OUT in #6, only the three least significant bits of B are needed.
; As #7 jumps to #0, where B is set, the value of B is not used after instruction #6.
; With the fact that the right number in the XOR in #5 has 3-bits,
; it is possible to consider only the three least significant bits of B in #5.
; That is, from #4 onwards, only the three least significant bits of B are needed.
; This implies that only the three least significant bits of C are needed in #3.
; Because B is a 3-bit number, the maximum number of bit shifts that may happen in #2 is 7.
; Using the properties of B and C in #2, it follows that ten least significant bits
; of A are sufficient to calculate OUT.
;
; So, to find the solution, check all 10-bit integers from 0 (inclusive) to 1023 (inclusive)
; as the initial value of A.
; Out of these, take the values of A which produce the correct last output value OUT.
; Pick the lowest of these and shift it 3 bits to the left. Call this number D.
; Then, for all 3-bit numbers, bitwise OR it to D and check if this number produces the correct
; next output value.
; Repeat until all output values have been reproduced.
; This searches for the final A backwards.

(define (find-last-output-inits program)
  (let ((correct-output (vector-ref program (1- (vector-length program)))))
    (filter (lambda (reg-a-init)
              (let ((output (run-until-output program 0 reg-a-init 0 0)))
                (= output correct-output)))
            (iota 1024))))

(define (find-output-inits program previous-init correct-output)
  (filter (lambda (reg-a-init)
            (let ((output (run-until-output program 0 reg-a-init 0 0)))
              (= output correct-output)))
          (map (lambda (init-part) (logior (ash previous-init 3) init-part))
               (iota 8))))

(define (find-init-value program init-value output-index)
  (if (< output-index 0)
    (list init-value)
    (apply append
           (map (lambda (valid-init)
                  (find-init-value program valid-init (1- output-index)))
                (find-output-inits program init-value (vector-ref program output-index))))))

(define (part-2 input-data)
  (receive (registers program)
           (parse-input input-data)
           (apply min
                  (apply append
                         (map (lambda (last-output-init)
                                (find-init-value program last-output-init (- (vector-length program) 2)))
                              (find-last-output-inits program))))))

(display (part-2 input-data))
(newline)
