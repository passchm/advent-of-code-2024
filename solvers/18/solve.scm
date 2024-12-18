(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 q) #:select (make-q enq! deq! q-empty?)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define grid-size 71)

(define (parse-input input-data)
  (map (lambda (nums) (cons (car nums) (cadr nums)))
       (map (lambda (line)
              (map string->number (string-split line #\,)))
            (string-split
              (string-trim-right input-data #\linefeed)
              #\linefeed))))

(define (bfs-parents root-node discover-neighbor-nodes)
  (let ((Q (make-q))
        (explored (make-hash-table))
        (parents (make-hash-table)))
    (hash-set! explored root-node #t)
    (enq! Q root-node)
    (let loop ()
      (if (q-empty? Q)
        #f
        (let* ((node (deq! Q))
               (neighbor-nodes (discover-neighbor-nodes node)))
          (for-each
            (lambda (neighbor-node)
              (when (not (hash-ref explored neighbor-node))
                (hash-set! explored neighbor-node #t)
                (hash-set! parents neighbor-node node)
                (enq! Q neighbor-node)))
            neighbor-nodes)
          (loop))))
    parents))

(define (bfs-path root-node goal-node discover-neighbor-nodes)
  (let ((parents (bfs-parents root-node discover-neighbor-nodes)))
    (if (equal? root-node goal-node)
      (list root-node)
      (if (not (hash-ref parents goal-node))
        #f
        (let loop ((path '()) (current-node goal-node))
          (if (equal? current-node root-node)
            (cons root-node path)
            (loop (cons current-node path) (hash-ref parents current-node))))))))

(define (build-corrupted-grid corrupted-coordinates)
  (let ((grid (make-array #f grid-size grid-size)))
    (for-each (lambda (coord) (array-set! grid #t (cdr coord) (car coord)))
              corrupted-coordinates)
    grid))

(define (possible-moves corrupted-grid coord)
  (filter
    pair?
    (list
      ; up
      (if (and (array-in-bounds? corrupted-grid (1- (cdr coord)) (car coord))
               (not (array-ref corrupted-grid (1- (cdr coord)) (car coord))))
        (cons (car coord) (1- (cdr coord)))
        '())
      ; down
      (if (and (array-in-bounds? corrupted-grid (1+ (cdr coord)) (car coord))
               (not (array-ref corrupted-grid (1+ (cdr coord)) (car coord))))
        (cons (car coord) (1+ (cdr coord)))
        '())
      ; left
      (if (and (array-in-bounds? corrupted-grid (cdr coord) (1- (car coord)))
               (not (array-ref corrupted-grid (cdr coord) (1- (car coord)))))
        (cons (1- (car coord)) (cdr coord))
        '())
      ; right
      (if (and (array-in-bounds? corrupted-grid (cdr coord) (1+ (car coord)))
               (not (array-ref corrupted-grid (cdr coord) (1+ (car coord)))))
        (cons (1+ (car coord)) (cdr coord))
        '()))))

(define (part-1 input-data)
  (let ((corrupted-grid
          (build-corrupted-grid
            (list-head (parse-input input-data) 1024)))
        (start-coord '(0 . 0))
        (end-coord (cons (1- grid-size) (1- grid-size))))
    (1- (length (bfs-path
                  start-coord
                  end-coord
                  (lambda (coord) (possible-moves corrupted-grid coord)))))))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  (let* ((corrupted-bytes (parse-input input-data))
         (corrupted-grid
           (build-corrupted-grid
             (list-head corrupted-bytes 1024)))
         (start-coord '(0 . 0))
         (end-coord (cons (1- grid-size) (1- grid-size))))
    (let loop ((corrupted-bytes (list-tail corrupted-bytes 1024)))
      (if (null? corrupted-bytes)
        #f
        (begin
          (array-set! corrupted-grid #t (cdr (car corrupted-bytes)) (car (car corrupted-bytes)))
          (if (not (bfs-path
                     start-coord
                     end-coord
                     (lambda (coord) (possible-moves corrupted-grid coord))))
            (format #f "~a,~a" (caar corrupted-bytes) (cdar corrupted-bytes))
            (loop (cdr corrupted-bytes))))))))

(display (part-2 input-data))
(newline)
