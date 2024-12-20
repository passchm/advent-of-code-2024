(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((ice-9 q) #:select (make-q enq! deq! q-empty?)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (list->array
    2
    (map string->list
         (string-split
           (string-trim-right input-data #\linefeed)
           #\newline))))

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

(define (bfs-path root-node goal-node discover-neighbor-nodes)
  (let ((Q (make-q))
        (explored (make-hash-table))
        (parents (make-hash-table)))
    (hash-set! explored root-node #t)
    (enq! Q root-node)
    (let loop ()
      (if (q-empty? Q)
        #f
        (let ((current-node (deq! Q)))
          (if (equal? current-node goal-node)
            #t
            (begin
              (for-each
                (lambda (neighbor-node)
                  (when (not (hash-ref explored neighbor-node))
                    (hash-set! explored neighbor-node #t)
                    (hash-set! parents neighbor-node current-node)
                    (enq! Q neighbor-node)))
                (discover-neighbor-nodes current-node))
              (loop))))))
    (if (equal? root-node goal-node)
      (list root-node)
      (if (not (hash-ref parents goal-node))
        #f
        (let loop ((path '()) (current-node goal-node))
          (if (equal? current-node root-node)
            (cons root-node path)
            (loop (cons current-node path)
                  (hash-ref parents current-node))))))))

(define (neighbor-tiles grid tile)
  (filter (lambda (neighbor-tile)
            (array-in-bounds? grid (car neighbor-tile) (cdr neighbor-tile)))
          (map (lambda (dir) (cons (+ (car tile) (car dir))
                                   (+ (cdr tile) (cdr dir))))
               '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))))

(define (create-distances-grid grid path)
  (let ((distances-grid (apply make-array +inf.0 (array-shape grid))))
    (for-each (lambda (tile distance) (array-set! distances-grid distance (car tile) (cdr tile)))
              path
              (reverse (iota (length path))))
    distances-grid))

(define (manhattan-distance tile-1 tile-2)
  (+
    (abs (- (car tile-2) (car tile-1)))
    (abs (- (cdr tile-2) (cdr tile-1)))))

(define (calculate-cheat-savings distances-grid cheat-start cheat-end max-cheat-distance)
  (let ((start-dist (array-ref distances-grid (car cheat-start) (cdr cheat-start)))
        (end-dist (array-ref distances-grid (car cheat-end) (cdr cheat-end)))
        (cheat-dist (manhattan-distance cheat-start cheat-end)))
    (if (<= cheat-dist max-cheat-distance)
      (- start-dist (+ cheat-dist end-dist))
      -inf.0)))

(define (find-cheat-savings grid original-path max-cheat-distance)
  (let ((distances-grid (create-distances-grid grid original-path)))
    (let loop ((cheat-start (car original-path)) (path-rest (cdr original-path)) (savings '()))
      (if (null? path-rest)
        savings
        (loop
          (car path-rest)
          (cdr path-rest)
          (append
            (filter (lambda (score) (> score 0))
                    (map (lambda (cheat-end)
                           (calculate-cheat-savings distances-grid cheat-start cheat-end max-cheat-distance))
                         path-rest))
            savings))))))

; The race track is a non-branching ("unicursal") maze!

(define (find-cheats input-data max-cheat-distance)
  (let* ((grid (parse-input input-data))
         (start-tile (car (find-in-grid grid (lambda (c) (eqv? c #\S)))))
         (end-tile (car (find-in-grid grid (lambda (c) (eqv? c #\E))))))
    (array-set! grid #\. (car start-tile) (cdr start-tile))
    (array-set! grid #\. (car end-tile) (cdr end-tile))
    (let ((original-path
            (bfs-path
              start-tile
              end-tile
              (lambda (tile)
                (filter (lambda (neigh)
                          (eqv? (array-ref grid (car neigh) (cdr neigh)) #\.))
                        (neighbor-tiles grid tile))))))
      (length
        (filter (lambda (n) (and n (>= n 100)))
                (find-cheat-savings grid original-path max-cheat-distance))))))

(define (part-1 input-data)
  (find-cheats input-data 2))

(display (part-1 input-data))
(newline)

; Part 2

(define (part-2 input-data)
  (find-cheats input-data 20))

(display (part-2 input-data))
(newline)
