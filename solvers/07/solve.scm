(use-modules ((ice-9 textual-ports) #:select (get-string-all)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (map (lambda (line)
         (map string->number
              (string-split (string-delete #\: line) #\space)))
       (string-split
         (string-trim-right input-data #\linefeed)
         #\linefeed)))

(define (valid-combination? test-value intermediate-value remaining-numbers)
  (if (null? remaining-numbers)
    (= intermediate-value test-value)
    (or
      (valid-combination? test-value (+ intermediate-value (car remaining-numbers)) (cdr remaining-numbers))
      (valid-combination? test-value (* intermediate-value (car remaining-numbers)) (cdr remaining-numbers)))))

(define (valid-calibration-equation? equation)
  (valid-combination? (car equation) (cadr equation) (cddr equation)))

(define (part-1 input-data)
  (apply +
         (map car (filter valid-calibration-equation? (parse-input input-data)))))

(display (part-1 input-data))
(newline)

; Part 2

(define (concat-digits a b)
  (string->number (string-append (number->string a) (number->string b))))

(define (valid-extended-combination? test-value intermediate-value remaining-numbers)
  (if (null? remaining-numbers)
    (= intermediate-value test-value)
    (or
      (valid-extended-combination? test-value (+ intermediate-value (car remaining-numbers)) (cdr remaining-numbers))
      (valid-extended-combination? test-value (* intermediate-value (car remaining-numbers)) (cdr remaining-numbers))
      (valid-extended-combination? test-value (concat-digits intermediate-value (car remaining-numbers)) (cdr remaining-numbers)))))

(define (valid-extended-calibration-equation? equation)
  (valid-extended-combination? (car equation) (cadr equation) (cddr equation)))

(define (part-2 input-data)
  (apply +
         (map car (filter valid-extended-calibration-equation? (parse-input input-data)))))

(display (part-2 input-data))
(newline)
