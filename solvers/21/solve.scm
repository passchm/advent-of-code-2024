(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 match) #:select (match-lambda)))
(use-modules ((srfi srfi-1) #:select (zip)))

(add-to-load-path (dirname (current-filename)))
(use-modules ((dijkstra) #:select (dijkstra-find-best-paths)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

; OP4: me
; KP4: directional keypad
; OP3: robot (-40 degrees)
; KP3: directional keypad
; OP2: robot (high levels of radiation)
; KP2: directional keypad
; OP1: robot (depressurized)
; KP1: numeric keypad
;
; To press any button on KP1, the A button must be pressed on each of the other keypads.
; Which means that OP2, OP3, and OP4 return to their initial state.
; This also implies that each code (line of the input) is independent of the others.
;
; Unfortunately, the shortest paths between two numbers on KP1 may not result in the
; smallest number of key presses by OP4. Is this really true?

(define numeric-keypad-grid
  (list->array 2 '((#\7 #\8 #\9)
                   (#\4 #\5 #\6)
                   (#\1 #\2 #\3)
                   (#f #\0 #\A))))

(define directional-keypad-grid
  (list->array 2 '((#f #\^ #\A)
                   (#\< #\v #\>))))

(define (parse-input input-data)
  (string-split
    (string-trim-right input-data #\linefeed)
    #\linefeed))

(define (find-in-grid grid pred)
  (let ((num-rows (array-length grid)) (num-columns (cadr (array-dimensions grid))))
    (let loop ((r 0) (c 0) (indices '()))
      (if (>= r num-rows)
        (reverse indices)
        (if (>= c num-columns)
          (loop (+ r 1) 0 indices)
          (loop r (+ c 1)
                (if (pred (array-ref grid r c))
                  (cons (cons r c) indices)
                  indices)))))))

(define (keypad-edges keypad-grid button)
  (let ((pos (car (find-in-grid keypad-grid (lambda (c) (eqv? c button))))))
    (map (lambda (p) (cons 1 (array-ref keypad-grid (car p) (cdr p))))
         (filter (lambda (p)
                   (and (array-in-bounds? keypad-grid (car p) (cdr p))
                        (array-ref keypad-grid (car p) (cdr p))))
                 (list
                   ; up
                   (cons (1- (car pos)) (cdr pos))
                   ; down
                   (cons (1+ (car pos)) (cdr pos))
                   ; left
                   (cons (car pos) (1- (cdr pos)))
                   ; right
                   (cons (car pos) (1+ (cdr pos))))))))

(define (keypad-path->moves keypad-grid path)
  (let* ((path-positions
           (map (lambda (button)
                  (car (find-in-grid keypad-grid
                                     (lambda (c) (eqv? c button)))))
                path))
         (deltas
           (map (match-lambda (((r1 . c1) (r2 . c2))
                               (cons (- r2 r1) (- c2 c1))))
                (zip path-positions (cdr path-positions)))))
    (map (match-lambda ((-1 . 0) #\^)
                       ((1 . 0) #\v)
                       ((0 . -1) #\<)
                       ((0 . 1) #\>))
         deltas)))

(define (keypad-moves keypad-grid start-button end-button)
  (map (lambda (path) (keypad-path->moves keypad-grid path))
       (dijkstra-find-best-paths
         start-button
         end-button
         (lambda (button) (keypad-edges keypad-grid button)))))

; '((1) (2 3) (4) (5 6))
; '((1 2 4 5) (1 2 4 6) (1 3 4 5) (1 3 4 6))
(define (combinations-lists lst)
  (if (null? lst)
    '(())
    (let ((first (car lst))
          (rest (combinations-lists (cdr lst))))
      (apply append
             (map (lambda (e)
                    (map (lambda (r)
                           (cons e r))
                         rest))
                  first)))))

(define (propagate-sequences keypad-grid alternative-sequences)
  (apply append
         (map (lambda (sequence)
                (map (lambda (comb) (apply append comb))
                     (combinations-lists
                       (map (lambda (lower-move)
                              (map (lambda (possibility) (append possibility '(#\A)))
                                   (keypad-moves keypad-grid
                                                 (car lower-move)
                                                 (cadr lower-move))))
                            (zip (cons #\A sequence) sequence)))))
              alternative-sequences)))

(define (propagate-code code)
  (map list->string
       ; OP4
       (propagate-sequences
         directional-keypad-grid
         ; OP3
         (propagate-sequences
           directional-keypad-grid
           ; OP2
           (propagate-sequences
             numeric-keypad-grid
             ; OP1
             (list (string->list code)))))))

(define (part-1 input-data)
  (apply +
         (map (lambda (code)
                (let ((sequences (propagate-code code)))
                  (*
                    (apply min (map string-length sequences))
                    (string->number (string-drop-right code 1)))))
              (parse-input input-data))))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
