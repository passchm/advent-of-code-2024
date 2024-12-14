(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 match) #:select (match)))
(use-modules ((ice-9 peg) #:select (define-peg-pattern match-pattern peg:end peg:tree)))
(use-modules ((srfi srfi-9 gnu) #:select (define-immutable-record-type)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define-peg-pattern num-pattern all (+ (range #\0 #\9)))

(define-peg-pattern label-pattern all (or "A" "B"))

(define-peg-pattern button-pattern all
                    (and
                      (ignore "Button ")
                      label-pattern
                      (ignore ": X+")
                      num-pattern
                      (ignore ", Y+")
                      num-pattern))

(define-peg-pattern prize-pattern all
                    (and
                      (ignore "Prize: X=")
                      num-pattern
                      (ignore ", Y=")
                      num-pattern))

(define-peg-pattern machine-pattern all
                    (and
                      button-pattern
                      (ignore "\n")
                      button-pattern
                      (ignore "\n")
                      prize-pattern
                      (ignore "\n")))

(define-peg-pattern configuration-pattern body (+ (and machine-pattern (ignore (? "\n")))))

(define-immutable-record-type
  <machine>
  (machine ax ay bx by px py)
  machine?
  (ax machine-ax)
  (ay machine-ay)
  (bx machine-bx)
  (by machine-by)
  (px machine-px set-machine-px)
  (py machine-py set-machine-py))

(define (machine-config->machine machine-config)
  (match machine-config
         ((machine-pattern
            (button-pattern
              _
              (num-pattern ax)
              (num-pattern ay))
            (button-pattern
              _
              (num-pattern bx)
              (num-pattern by))
            (price-pattern
              (num-pattern px)
              (num-pattern py)))
          (apply machine (map string->number (list ax ay bx by px py))))))

(define (parse-input input-data)
  (let ((rec (match-pattern configuration-pattern input-data)))
    (when (not (= (peg:end rec) (string-length input-data)))
      (error "Failed to parse input"))
    (let ((full-config (peg:tree rec)))
      (map machine-config->machine full-config))))

; All paths that lead to the price consist of the same number of A and B button presses.
; Just the order of A and B button presses is different for each path.
; So there is either no path or just a single combination of button presses
; which can be in any order.

(define (valid-path? machine a-presses b-presses)
  (and
    (= (machine-px machine)
       (+ (* a-presses (machine-ax machine)) (* b-presses (machine-bx machine))))
    (= (machine-py machine)
       (+ (* a-presses (machine-ay machine)) (* b-presses (machine-by machine))))))

(define (find-path machine)
  (let loop-a ((a-presses 0))
    (if (> a-presses 100)
      #f
      (let ((path
              (let loop-b ((b-presses 0))
                (if (> b-presses 100)
                  #f
                  (if (valid-path? machine a-presses b-presses)
                    (cons a-presses b-presses)
                    (loop-b (+ b-presses 1)))))))
        (if path
          path
          (loop-a (+ a-presses 1)))))))

(define (part-1 input-data)
  (apply +
         (map (lambda (path) (+ (* 3 (car path)) (cdr path)))
              (filter identity
                      (map find-path (parse-input input-data))))))

(display (part-1 input-data))
(newline)

; Part 2

(define (move-prize machine)
  (set-machine-py
    (set-machine-px machine
                    (+ (machine-px machine) 10000000000000))
    (+ (machine-py machine) 10000000000000)))

; As was derived in the previous part, there is either no path
; or a single combination of A and B button presses that reach the price.
;
; For a path that reaches the price, the following two linear equations must hold:
;
; x_A * n_A + x_B * n_B = x_p
; y_A * n_A + y_B * n_B = y_p
;
; This can be written in matrix form, where the unknowns are n_A and n_B.
; Because n_A and n_B must be integers, not all solutions are valid paths.

(define (find-path-smart machine)
  ; In this procedure, the formula for the inverse of a 2-by-2 matrix is used.
  ; Because x_A, y_A, x_B, and y_B are all integers, the determinant is an integer.
  (let* ((x_A (machine-ax machine))
         (y_A (machine-ay machine))
         (x_B (machine-bx machine))
         (y_B (machine-by machine))
         (x_p (machine-px machine))
         (y_p (machine-py machine))
         (det (- (* x_A y_B) (* y_A x_B)))
         (n_A_times_det (+ (* y_B x_p) (* (- x_B) y_p)))
         (n_B_times_det (+ (* (- y_A) x_p) (* x_A y_p))))
    ; Check that the matrix is invertible.
    (if (= det 0)
      #f
      ; Check that n_A and n_B are integers.
      (if (and (= (euclidean-remainder n_A_times_det det) 0)
               (= (euclidean-remainder n_B_times_det det) 0))
        (cons (/ n_A_times_det det) (/ n_B_times_det det))
        #f))))

(define (part-2 input-data)
  (apply +
         (map (lambda (path) (+ (* 3 (car path)) (cdr path)))
              (filter identity
                      (map find-path-smart
                           (map move-prize (parse-input input-data)))))))

(display (part-2 input-data))
(newline)
