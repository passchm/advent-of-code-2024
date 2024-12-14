(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 match) #:select (match)))
(use-modules ((ice-9 peg) #:select (define-peg-pattern match-pattern peg:end peg:tree)))
(use-modules ((srfi srfi-9 gnu) #:select (define-immutable-record-type)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define-peg-pattern num-pattern all (and (? "-") (+ (range #\0 #\9))))

(define-peg-pattern robot-pattern all
                    (and (ignore "p=")
                         num-pattern
                         (ignore ",")
                         num-pattern
                         (ignore " v=")
                         num-pattern
                         (ignore ",")
                         num-pattern
                         (ignore "\n")))

(define-peg-pattern robots-pattern body (+ robot-pattern))

(define-immutable-record-type
  <robot>
  (robot px py vx vy)
  robot?
  (px robot-px)
  (py robot-py)
  (vx robot-vx)
  (vy robot-vy))

(define (robot-config->robot robot-config)
  (match robot-config
         ((robot-pattern
            (num-pattern px)
            (num-pattern py)
            (num-pattern vx)
            (num-pattern vy))
          (apply robot (map string->number (list px py vx vy))))))

(define (parse-input input-data)
  (let ((rec (match-pattern robots-pattern input-data)))
    (when (not (= (peg:end rec) (string-length input-data)))
      (error "Failed to parse input"))
    (map robot-config->robot (peg:tree rec))))

(define grid-width 101)
(define grid-height 103)

(define (move-robot bot time-step)
  (robot
    (euclidean-remainder (+ (robot-px bot) (* (robot-vx bot) time-step)) grid-width)
    (euclidean-remainder (+ (robot-py bot) (* (robot-vy bot) time-step)) grid-height)
    (robot-vx bot)
    (robot-vy bot)))

(define (count-robots-in-quadrants bots)
  (let ((quadrant-width (/ (- grid-width 1) 2))
        (quadrant-height (/ (- grid-height 1) 2))
        (counts (make-vector 4 0)))
    (for-each
      (lambda (bot)
        (let ((quadrant-number
                (cond
                  ((and (> (robot-px bot) quadrant-width) (> (robot-py bot) quadrant-height))
                   1)
                  ((and (< (robot-px bot) quadrant-width) (> (robot-py bot) quadrant-height))
                   2)
                  ((and (< (robot-px bot) quadrant-width) (< (robot-py bot) quadrant-height))
                   3)
                  ((and (> (robot-px bot) quadrant-width) (< (robot-py bot) quadrant-height))
                   4))))
          (when (number? quadrant-number)
            (vector-set! counts (- quadrant-number 1)
                         (+ (vector-ref counts (- quadrant-number 1)) 1)))))
      bots)
    counts))

(define (part-1 input-data)
  (apply *
         (vector->list (count-robots-in-quadrants
                         (map (lambda (bot) (move-robot bot 100))
                              (parse-input input-data))))))

(display (part-1 input-data))
(newline)

; Part 2

(define (draw-ascii-pbm-image port grid)
  (let ((image-height (car (array-dimensions grid)))
        (image-width (cadr (array-dimensions grid))))
    (display "P1\n" port)
    (display image-width port)
    (display " " port)
    (display image-height port)
    (display "\n" port)
    (for-each
      (lambda (y)
        (for-each
          (lambda (x)
            (if (>= (array-ref grid y x) 1)
              (display 1 port)
              (display 0 port)))
          (iota image-width))
        (display "\n" port))
      (iota image-height))))

(define (draw-situation bots)
  (let ((grid (make-typed-array 'u8 0 grid-height grid-width)))
    (for-each
      (lambda (bot)
        (array-set! grid
                    (+ (array-ref grid (robot-py bot) (robot-px bot)) 1)
                    (robot-py bot)
                    (robot-px bot)))
      bots)
    grid))

(define (draw-situation-image! bots time-step)
  (call-with-output-file
    (format #f "image_~d.pbm" time-step)
    (lambda (port)
      (draw-ascii-pbm-image
        port
        (draw-situation
          (map (lambda (bot) (move-robot bot time-step))
               bots))))))

(define (count-downstairs-neighbors bots)
  (let ((positions (make-hash-table (length bots))))
    (for-each (lambda (bot)
                (hash-set! positions (cons (robot-px bot) (robot-py bot)) #t))
              bots)
    (length
      (filter
        (lambda (bot) (hash-ref positions (cons (robot-px bot) (+ (robot-py bot) 1)) #f))
        bots))))

(define (first-index-of-value value vec)
  (let loop ((i 0))
    (if (< i (vector-length vec))
      (if (equal? (vector-ref vec i) value)
        i
        (loop (+ i 1)))
      #f)))

; A tree always has a trunk.

(define (part-2 input-data)
  (let* ((bots (parse-input input-data))
         (downstairs-neighbors
           (map (lambda (time-step)
                  (count-downstairs-neighbors
                    (map (lambda (bot) (move-robot bot time-step)) bots)))
                (iota 100000)))
         (christmas-tree-time-step
           (first-index-of-value
             (apply max downstairs-neighbors)
             (list->vector downstairs-neighbors))))
    (when #f
      (draw-situation-image! bots christmas-tree-time-step))
    christmas-tree-time-step))

(display (part-2 input-data))
(newline)
