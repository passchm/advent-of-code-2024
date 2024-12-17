(define-module (dijkstra)
               #:export (dijkstra-find-best-paths)
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
                 (alt (+ (hash-ref distances node) (edge-weight current-edge))))
            (if (or (not (hash-ref distances (edge-destination current-edge)))
                    (< alt (hash-ref distances (edge-destination current-edge))))
              (begin
                ; The edge destination node has no associated distance yet,
                ; or it can be reached more quickly via the current node.
                (hash-set! previous (edge-destination current-edge) (list node))
                (hash-set! distances (edge-destination current-edge) alt)
                (set! pq (pq-insert pq (edge-destination current-edge) alt)))
              (when (= alt (hash-ref distances (edge-destination current-edge)))
                ; The edge destination node can be reached via the current node
                ; at the same distance as is associated currently.
                ; This means that there is more than one path to reach the edge
                ; destination node with the same distance from the start node.
                (when (not (hash-ref previous (edge-destination current-edge)))
                  (error "Edge destination node is unknown but it has an associated distance"))
                (hash-set! previous
                           (edge-destination current-edge)
                           (cons node (hash-ref previous (edge-destination current-edge) '())))))
            (loop (cdr edges))))))

    (while (not (null? pq))
           (let ((best-node (caar pq)))
             (set! pq (cdr pq))
             (explore-node! best-node)))

    (values previous distances)))

(define (track-best-paths current-node start-node previous)
  ; Returns a list of the best paths from the current node to the start node.
  (if (equal? current-node start-node)
    (list (list current-node))
    (let ((previous-nodes (hash-ref previous current-node)))
      (if (not previous-nodes)
        '()
        (apply
          append
          (map-in-order
            (lambda (previous-node)
              (map-in-order
                (lambda (previous-path) (cons current-node previous-path))
                (track-best-paths previous-node start-node previous)))
            previous-nodes))))))

(define (dijkstra-find-best-paths start-node end-node discover-edges)
  (receive (previous distances)
           (dijkstra start-node discover-edges)
           (values
             (track-best-paths end-node start-node previous)
             distances)))
