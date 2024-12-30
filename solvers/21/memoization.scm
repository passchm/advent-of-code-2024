(define-module (memoization)
               #:export (define-memoized))

(define (memoize f)
  (let ((cache (make-hash-table)))
    (lambda args
      (let* ((key (list->vector args))
             (result (hash-ref cache key #f)))
        (if result
          result
          (let ((computed-result (apply f args)))
            (hash-set! cache key computed-result)
            computed-result))))))

(define-syntax-rule
  (define-memoized (name . args) . body)
  (begin
    (define (name . args) . body)
    (set! name (memoize name))))

(define-memoized (fib n)
                 (if (< n 2)
                   n
                   (+ (fib (- n 1)) (fib (- n 2)))))

; (display (fib 100))
; (newline)
