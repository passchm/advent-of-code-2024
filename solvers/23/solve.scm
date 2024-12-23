(use-modules ((ice-9 textual-ports) #:select (get-string-all)))
(use-modules ((srfi srfi-1) #:select (any lset-intersection delete-duplicates)))

(define input-data
  (call-with-input-file "input.txt" get-string-all))

; Part 1

(define (parse-input input-data)
  (map (lambda (p) (cons (car p) (cadr p)))
       (map (lambda (line)
              (string-split line #\-))
            (string-split
              (string-trim-right input-data #\linefeed)
              #\linefeed))))

(define (build-connections-map connections)
  (let ((computers (make-hash-table)))
    (for-each (lambda (conn)
                (hash-set! computers
                           (car conn)
                           (sort-list
                             (cons (cdr conn)
                                   (hash-ref computers (car conn) '()))
                             string<?))
                (hash-set! computers
                           (cdr conn)
                           (sort-list
                             (cons (car conn)
                                   (hash-ref computers (cdr conn) '()))
                             string<?)))
              connections)
    computers))

(define (find-groups-with-computer computer connections-map)
  (let ((direct-conns (hash-ref connections-map computer '())))
    (map (lambda (triangles) (sort-list triangles string<?))
         (apply append
                (map (lambda (direct-computer)
                       (map (lambda (other-computer)
                              (list computer direct-computer other-computer))
                            (lset-intersection
                              string=?
                              (hash-ref connections-map direct-computer '())
                              direct-conns)))
                     direct-conns)))))

(define (find-computer-groups computers connections-map)
  (delete-duplicates
    (apply append
           (map (lambda (computer)
                  (find-groups-with-computer computer connections-map))
                computers))))

(define (part-1 input-data)
  (let* ((connections-map (build-connections-map (parse-input input-data)))
         (computers (sort-list
                      (hash-map->list (lambda (key value) key) connections-map)
                      string<?))
         (triangles (find-computer-groups computers connections-map)))
    (length
      (filter (lambda (triangle)
                (any (lambda (name) (string-prefix? "t" name)) triangle))
              triangles))))

(display (part-1 input-data))
(newline)

; Part 2

(define (max-occurrences lst)
  (let ((counters (make-hash-table)))
    (for-each
      (lambda (element)
        (hash-set! counters element (1+ (hash-ref counters element 0))))
      lst)
    (let ((max-count
            (apply max (hash-map->list (lambda (key value) value) counters))))
      (cons max-count
            (filter (lambda (element) (= (hash-ref counters element 0) max-count))
                    (hash-map->list (lambda (key value) key) counters))))))

(define (find-small-groups computer connections-map)
  (let ((direct-conns (hash-ref connections-map computer '())))
    (filter (lambda (conns) (> (length conns) 1))
            (map (lambda (direct-computer)
                   (sort-list
                     (cons computer
                           (cons
                             direct-computer
                             (lset-intersection
                               string=?
                               direct-conns
                               (hash-ref connections-map direct-computer '()))))
                     string<?))
                 direct-conns))))

; https://en.wikipedia.org/wiki/Clique_(graph_theory)

(define (part-2 input-data)
  (let* ((connections-map (build-connections-map (parse-input input-data)))
         (computers (sort-list
                      (hash-map->list (lambda (key value) key) connections-map)
                      string<?)))
    (string-join
      (sort-list
        (cadr
          (max-occurrences
            (apply append
                   (map (lambda (computer)
                          (find-small-groups computer connections-map))
                        computers))))
        string<?)
      ",")))

(display (part-2 input-data))
(newline)
