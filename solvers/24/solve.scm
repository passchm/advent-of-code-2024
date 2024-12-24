(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 hash-table) #:select (alist->hash-table)))
(use-modules ((ice-9 match) #:select (match)))
(use-modules ((ice-9 receive) #:select (receive)))
(use-modules ((srfi srfi-1) #:select (fold-right)))
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
  (z gate-z))

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

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
