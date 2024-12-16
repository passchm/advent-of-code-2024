(define-module (dijkstra)
               #:export (dijkstra-find-path)
               #:use-module ((srfi srfi-9 gnu) #:select (define-immutable-record-type))
               #:use-module ((ice-9 receive) #:select (receive)))

(define-immutable-record-type
  <edge>
  (edge destination weight)
  edge?
  (destination edge-destination)
  (weight edge-weight))

(define (make-priority-queue)
  (list))

(define (pq-insert pq node priority)
  (define (helper lst acc)
    (cond
      ((null? lst) (reverse (cons (cons node priority) acc)))
      ((<= priority (cdar lst)) (append (reverse acc) (cons (cons node priority) lst)))
      (else (helper (cdr lst) (cons (car lst) acc)))))
  (helper pq '()))

; Dijkstra's algorithm with dynamic edge discovery
(define (dijkstra start-node discover-edges)
  (let ((pq (make-priority-queue))
        (distances (make-hash-table))
        (previous (make-hash-table)))

    (set! pq (pq-insert pq start-node 0))
    (hash-set! distances start-node 0)

    (define (explore-node! node)
      (let loop ((edges (map (lambda (e) (edge (cdr e) (car e))) (discover-edges node))))
        (if (null? edges)
          '()
          (let* ((current-edge (car edges))
                 (alt (+ (hash-ref distances node +inf.0) (edge-weight current-edge))))
            (when (< alt (hash-ref distances (edge-destination current-edge) +inf.0))
              (hash-set! previous (edge-destination current-edge) node)
              (hash-set! distances (edge-destination current-edge) alt)
              (set! pq (pq-insert pq (edge-destination current-edge) alt)))
            (loop (cdr edges))))))

    (while (not (null? pq))
           (let ((best-node (caar pq)))
             (set! pq (cdr pq))
             (explore-node! best-node)))

    (values distances previous)))

(define (dijkstra-find-path start-node end-node discover-edges)
  (receive (distances previous)
           (dijkstra start-node discover-edges)
           (let loop ((current-node end-node) (path (list end-node)))
             (if (equal? current-node start-node)
               path
               (let ((previous-node (hash-ref previous current-node #f)))
                 (if (not previous-node)
                   '()
                   (loop previous-node (cons previous-node path))))))))
