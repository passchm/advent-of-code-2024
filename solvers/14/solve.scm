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

(define (move-robot bot duration)
  (robot
    (euclidean-remainder (+ (robot-px bot) (* (robot-vx bot) duration)) grid-width)
    (euclidean-remainder (+ (robot-py bot) (* (robot-vy bot) duration)) grid-height)
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

(define (part-2 input-data)
  '())

(display (part-2 input-data))
(newline)
