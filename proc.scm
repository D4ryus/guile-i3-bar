(define-module (guile-i3-bar proc)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (read-proc-diskstats!
            read-proc-meminfo!
            read-proc-net-dev!
            read-proc-stat!))

(define read-proc-net-dev!
  (let* ((mapping (list
                   'received-bytes 'received-packets 'received-errors 'received-drop
                   'received-fifo 'received-frame 'received-compressed 'received-multicast
                   'transmitted-bytes 'transmitted-packets 'transmitted-errors 'transmitted-drop
                   'transmitted-fifo 'transmitted-colls 'transmitted-carrier 'transmitted-compressed))
         (device-rgx (make-regexp (format #f "^ *([a-zA-Z0-9-]+):~{~a~}$"
                                          (map (lambda (_) (identity " +([0-9]+)"))
                                               mapping)))))
    (lambda ()
      (call-with-input-file "/proc/net/dev"
        (lambda (port)
          ;; first two lines are header
          (read-line port)
          (read-line port)
          (let loop ((line (read-line port))
                     (result (list)))
            (if (eof-object? line)
                (reverse result)
                (let ((match (regexp-exec device-rgx line)))
                  (if (not match)
                      (error "Line does not match regex:" line)
                      (loop (read-line port)
                            (cons (cons (string->lispified-symbol
                                         (match:substring match 1))
                                        (map cons mapping
                                             (map string->number
                                                  (cdr (match-substrings match)))))
                                  result)))))))))))

(define read-proc-diskstats!
  (let* ((unused-disk-rgx (make-regexp "^ *[0-9]+ +[0-9]+ +[a-zA-Z0-9-]+( +0)+$"))
         (mapping (list 'reads-completed 'reads-merged 'sectors-read 'ms-spent-reading
                        'writes-completed 'writes-merged 'sectors-written 'ms-spent-writing
                        'io-in-progress 'ms-spent-io 'weighted-spent-io
                        'discards-completed 'discards-merged 'sectors-discarded 'ms-spent-discarding))
         (disk-rgx (make-regexp (format #f "^ *[0-9]+ +[0-9]+ +([a-zA-Z0-9-]+)~{~a~}$"
                                        (map (lambda (_) (identity " +([0-9]+)"))
                                             mapping))))
         (partition-rgx (make-regexp (format #f "^ *[0-9]+ +[0-9]+ +([a-zA-Z0-9-]+)([0-9]+)~{~a~}$"
                                             (map (lambda (_) (identity " +([0-9]+)"))
                                                  mapping)))))
    (lambda ()
      (call-with-input-file "/proc/diskstats"
        (lambda (port)
          (let loop ((line (read-line port))
                     (result (list)))
            (if (eof-object? line)
                (reverse result)
                (if (regexp-exec unused-disk-rgx line)
                    (loop (read-line port) result)
                    (let ((match (regexp-exec disk-rgx line)))
                      (if (not match)
                          (error "No matching regex found for" line)
                          (let ((partition-match (regexp-exec partition-rgx line)))
                            (if (and partition-match
                                     (not (null? result))
                                     (string-prefix? (symbol->string (caar result))
                                                     (match:substring match 1)))
                                (begin
                                  (assoc-set! (cdar result) 'partitions
                                              (append (get (cdar result) 'partitions)
                                                      (list
                                                       (map cons (cons 'nr mapping)
                                                            (map string->number
                                                                 (cdr (match-substrings match)))))))
                                  (loop (read-line port)
                                        result))
                                (loop (read-line port)
                                      (cons (cons (string->lispified-symbol (match:substring match 1))
                                                  (list (cons 'total
                                                              (map cons
                                                                   mapping
                                                                   (map string->number
                                                                        (cdr (match-substrings match)))))
                                                        (cons 'partitions '())))
                                            result))))))))))))))

(define read-proc-stat!
  (let* ((mapping '(user nice system idle iowait irq softirq steal guest guest-nice))
         (rgx `((cpu . ,(make-regexp (format #f "^cpu *~{~a~}$"
                                             (map (lambda (_) (identity " ([0-9]+)"))
                                                  mapping))))
                (cores . ,(make-regexp (format #f "^cpu([0-9]+)~{~a~}$"
                                               (map (lambda (_) (identity " +([0-9]+)"))
                                                    mapping))))
                (intr . ,(make-regexp "^intr +([0-9 ]+)$"))
                (ctxt . ,(make-regexp "^ctxt +([0-9]+)$"))
                (btime . ,(make-regexp "^btime +([0-9]+)$"))
                (processes . ,(make-regexp "^processes +([0-9]+)$"))
                (procs-running . ,(make-regexp "^procs_running +([0-9]+)$"))
                (procs-blocked . ,(make-regexp "^procs_blocked +([0-9]+)$"))
                (softirq . ,(make-regexp (format #f "^softirq *~{~a~}$"
                                                 (map (lambda (_) (identity " ([0-9]+)"))
                                                      (cons 'all mapping))))))))
    (lambda ()
      (call-with-input-file "/proc/stat"
        (lambda (port)
          (let loop ((line (read-line port))
                     (result (list))
                     (cores (list)))
            (if (eof-object? line)
                (append (reverse result)
                        (list (cons 'cores (reverse cores))))
                (let loop-rgx ((regex rgx))
                  (if (null? regex)
                      (error "No matching regex found for" line)
                      (let ((name (caar regex))
                            (match (regexp-exec (cdar regex) line)))
                        (if (not match)
                            (loop-rgx (cdr regex))
                            (case name
                              ((cpu softirq)
                               (loop (read-line port)
                                     (cons (cons name
                                                 (map cons
                                                      (if (equal? name 'softirq)
                                                          (cons 'all mapping)
                                                          mapping)
                                                      (map string->number
                                                           (match-substrings match))))
                                           result)
                                     cores))
                              ((cores)
                               (loop (read-line port)
                                     result
                                     (cons (map cons
                                                (cons 'nr mapping)
                                                (map string->number
                                                     (match-substrings match)))
                                           cores)))
                              ((intr)
                               (loop (read-line port)
                                     (cons (cons name
                                                 (map string->number
                                                      (string-split (match:substring match 1)
                                                                    #\Space)))
                                           result)
                                     cores))
                              (else
                               (loop (read-line port)
                                     (cons (cons name
                                                 (string->number
                                                  (match:substring match 1)))
                                           result)
                                     cores))))))))))))))

(define read-proc-meminfo!
  (let ((rgx-size (make-regexp "^([a-zA-Z0-9()_^:]+): +([0-9]+) ([a-zA-Z]+)$"))
        (rgx      (make-regexp "^([a-zA-Z0-9()_^:]+): +([0-9]+)$")))
    (lambda ()
      (call-with-input-file "/proc/meminfo"
        (lambda (port)
          (let loop ((line (read-line port))
                     (result (list)))
            (if (eof-object? line)
                (reverse result)
                (let* ((m (or (regexp-exec rgx-size line)
                              (regexp-exec rgx line)
                              (error "Neither regex matches for" line)))
                       (count (string->number (match:substring m 2)))
                       (unit (and (= (match:count m) 4)
                                  (match:substring m 3))))
                  (when unit
                    (cond
                     ((string=? unit "kB")
                      (set! count (* count 1000)))
                     (#t (error "Unknown unit" unit))))
                  (loop (read-line port)
                        (cons (cons (string->lispified-symbol (match:substring m 1))
                                    count)
                              result))))))))))
