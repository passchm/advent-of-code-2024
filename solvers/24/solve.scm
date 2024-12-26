(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 hash-table) #:select (alist->hash-table)))
(use-modules ((ice-9 match) #:select (match)))
(use-modules ((ice-9 receive) #:select (receive)))
(use-modules ((srfi srfi-1) #:select (delete-duplicates fold-right)))
(use-modules ((srfi srfi-9 gnu) #:select (define-immutable-record-type)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define-immutable-record-type
  <gate>
  (gate type x y z)
  gate?
  (type gate-type)
  (x gate-x)
  (y gate-y)
  (z gate-z set-gate-z))

(define (parse-input input-data)
  (let* ((split-index (1+ (string-contains input-data "\n\n")))
         (wires-text (substring/read-only input-data 0 split-index))
         (gates-text (substring/read-only input-data (1+ split-index))))
    (values
      (map (lambda (wire-def)
             (match (string-split wire-def #\space)
                    ((name value) (cons name (string->number value)))))
           (string-split
             (string-trim-right
               (string-delete #\: wires-text)
               #\linefeed)
             #\linefeed))
      (map (lambda (line)
             (match (string-tokenize line)
                    ((wire-in-a type wire-in-b "->" wire-out)
                     (gate (string->symbol type) wire-in-a wire-in-b wire-out))))
           (string-split
             (string-trim-right gates-text #\linefeed)
             #\linefeed)))))

(define (step-gates! gates wires)
  (filter gate?
          (map-in-order
            (lambda (g)
              (if (and (hash-ref wires (gate-x g))
                       (hash-ref wires (gate-y g)))
                (if (not (hash-ref wires (gate-z g)))
                  (let* ((x (hash-ref wires (gate-x g)))
                         (y (hash-ref wires (gate-y g)))
                         (z (cond
                              ((eq? (gate-type g) 'AND) (logand x y))
                              ((eq? (gate-type g) 'OR) (logior x y))
                              ((eq? (gate-type g) 'XOR) (logxor x y)))))
                    (hash-set! wires (gate-z g) z)
                    #f)
                  #f)
                g))
            gates)))

(define (compute-final-state wires gates)
  (let ((wires-table (alist->hash-table wires)))
    (let loop ((remaining-gates gates))
      (if (null? remaining-gates)
        (sort-list
          (hash-map->list (lambda (key value) (cons key value)) wires-table)
          (lambda (a b) (string<? (car a) (car b))))
        (loop (step-gates! remaining-gates wires-table))))))

(define (part-1 input-data)
  (receive (wires gates)
           (parse-input input-data)
           (fold-right (lambda (cur acc)
                         (logior (ash acc 1) (cdr cur)))
                       0
                       (filter (lambda (wire) (string-prefix? "z" (car wire)))
                               (compute-final-state wires gates)))))

(display (part-1 input-data))
(newline)

; Part 2

(define (swap-gate-inputs g)
  (gate
    (gate-type g)
    (gate-y g)
    (gate-x g)
    (gate-z g)))

(define (fix-order-of-inputs gates)
  (map (lambda (g)
         (if (or
               (and (string-prefix? "y" (gate-x g))
                    (string-prefix? "x" (gate-y g)))
               (and (string-prefix? "x" (gate-x g))
                    (string-prefix? "x" (gate-y g))
                    (string<? (gate-y g) (gate-x g)))
               (and (string-prefix? "y" (gate-x g))
                    (string-prefix? "y" (gate-y g))
                    (string<? (gate-y g) (gate-x g)))
               (and (string-prefix? "x" (gate-y g))
                    (not (string-prefix? "x" (gate-x g))))
               (and (string-prefix? "y" (gate-x g))
                    (not (string-prefix? "y" (gate-y g))))
               (and (not (string-prefix? "x" (gate-x g)))
                    (not (string-prefix? "y" (gate-y g)))
                    (string<? (gate-y g) (gate-x g))))
           (swap-gate-inputs g)
           g))
       gates))

(define (sort-gates gates)
  (sort-list gates
             (lambda (a b)
               (or
                 (string<? (gate-x a) (gate-x b))
                 (and (string=? (gate-x a) (gate-x b))
                      (string<? (gate-y a) (gate-y b)))
                 (and (string=? (gate-x a) (gate-x b))
                      (string=? (gate-y a) (gate-y b))
                      (string<? (symbol->string (gate-type a))
                                (symbol->string (gate-type b))))))))

(define (build-outputs-to-gates-map gates)
  (let ((table (make-hash-table)))
    (for-each (lambda (g)
                (hash-set! table (gate-z g) g))
              gates)
    table))

(define (fix-structure gates)
  (let* ((sorted-gates (sort-gates (fix-order-of-inputs gates)))
         (outputs-map (build-outputs-to-gates-map sorted-gates)))
    (sort-gates
      (map (lambda (g)
             (if (eq? 'OR (gate-type g))
               (let ((sub-y (hash-ref outputs-map (gate-y g))))
                 (if (and sub-y
                          (eq? 'AND (gate-type sub-y))
                          (string-prefix? "x" (gate-x sub-y))
                          (string-prefix? "y" (gate-y sub-y)))
                   (swap-gate-inputs g)
                   g))
               g))
           (map (lambda (g)
                  (if (and (hash-ref outputs-map (gate-x g))
                           (hash-ref outputs-map (gate-y g))
                           (eq? 'XOR (gate-type
                                       (hash-ref outputs-map (gate-y g)))))
                    (swap-gate-inputs g)
                    g))
                sorted-gates)))))

(define* (print-logic gates output-name #:optional (level 0) outputs-map)
         (let* ((outputs-map
                  (if outputs-map outputs-map (build-outputs-to-gates-map gates)))
                (g (hash-ref outputs-map output-name))
                (indent 2)
                (max-level 5))
           (if g
             (begin
               (display (make-string (* indent level) #\space))
               (display (gate-type g))
               (display " ")
               (display (gate-z g))
               (newline)
               (unless (> level max-level)
                 (print-logic gates (gate-x g) (1+ level) outputs-map)
                 (print-logic gates (gate-y g) (1+ level) outputs-map)))
             (begin
               (display (make-string (* indent level) #\space))
               (display output-name)
               (newline)))))

(define (count-output-bits gates)
  (length
    (filter (lambda (g) (string-prefix? "z" (gate-z g)))
            gates)))

(define (swap-output-wires gates swaps)
  (if (null? swaps)
    gates
    (let ((sw (car swaps)))
      (fix-structure
        (map (lambda (g)
               (cond
                 ((string=? (gate-z g) (car sw)) (set-gate-z g (cdr sw)))
                 ((string=? (gate-z g) (cdr sw)) (set-gate-z g (car sw)))
                 (else g)))
             (swap-output-wires gates (cdr swaps)))))))

; Because the logic is an adder, the output bit z[k] must depend on all inputs
; x[k], x[k - 1], ..., x[0] and y[k], y[k - 1], ..., y[0].
; It must not depend on any other inputs x[m] and y[m] where m > k.

; Manual inspection revealed that the input is a ripple-carry adder.
; The swaps have also been found using manual inspection.

(define (part-2 input-data)
  (receive (wires gates)
           (parse-input input-data)
           (let* ((sorted-gates (fix-structure gates))
                  (swaps
                    '(("hwk" . "z06")
                      ("qmd" . "tnt")
                      ("hpc" . "z31")
                      ("cgr" . "z37")))
                  (fixed-gates (swap-output-wires sorted-gates swaps)))
             (when #f
               (for-each (lambda (output-bit-index)
                           (print-logic
                             fixed-gates
                             (format #f "z~2,'0d" output-bit-index))
                           (newline))
                         (iota (count-output-bits gates))))
             (string-join
               (sort-list
                 (apply append
                        (map (lambda (sw) (list (car sw) (cdr sw))) swaps))
                 string<?)
               ","))))

(display (part-2 input-data))
(newline)
