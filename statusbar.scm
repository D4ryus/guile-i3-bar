(use-modules (srfi srfi-1)
             (ice-9 threads)
             (ice-9 textual-ports)
             (ice-9 regex)
             (ice-9 threads)
             (ice-9 format)
             (ice-9 pretty-print)
             (system repl server)
             (json))

(define stdout (current-output-port))

(define (match-substrings match)
  (let loop ((nr 1)
             (substrings (list)))
    (if (>= nr (match:count match))
        (reverse substrings)
        (loop (+ nr 1)
              (cons (match:substring match nr)
                    substrings)))))

(define (string->lispified-symbol str)
  (let ((res (list))
        (first? #t))
    (string-for-each
     (lambda (c) (cond
                  ((char-upper-case? c)
                   (unless first?
                     (set! res (cons #\- res)))
                   (set! res
                     (cons (char-downcase c)
                           res)))
                  ((or (char=? c #\_)
                       (char=? c #\())
                   (set! res (cons #\- res)))
                  ((char=? c #\)))
                  (#t (set! res (cons c res))))
             (set! first? #f))
     str)
    (string->symbol (apply string (reverse res)))))

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (accumulate op
                  (op init (car sequence))
                  (cdr sequence))))

(define read-proc-meminfo!
  (let ((rgx-size (make-regexp "^([a-zA-Z0-9()_^:]+): +([0-9]+) ([a-zA-Z]+)$"))
        (rgx      (make-regexp "^([a-zA-Z0-9()_^:]+): +([0-9]+)$")))
    (lambda ()
      (call-with-input-file "/proc/meminfo"
        (lambda (port)
          (let loop ((line (get-line port))
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
                  (loop (get-line port)
                        (cons (cons (string->lispified-symbol (match:substring m 1))
                                    count)
                              result))))))))))

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
          (let loop ((line (get-line port))
                     (result (list)))
            (if (eof-object? line)
                (reverse result)
                (if (regexp-exec unused-disk-rgx line)
                    (loop (get-line port) result)
                    (let ((match (regexp-exec disk-rgx line)))
                      (if (not match)
                          (error "No matching regex found for" line)
                          (let ((partition-match (regexp-exec partition-rgx line)))
                            (if (and partition-match
                                     (not (null? result))
                                     (string-prefix? (symbol->string (caar result))
                                                     (match:substring match 1)))
                                (begin
                                  (assv-set! (cdar result) 'partitions
                                             (append (get (cdar result) 'partitions)
                                                     (list
                                                      (map cons (cons 'nr mapping)
                                                           (map string->number
                                                                (cdr (match-substrings match)))))))
                                  (loop (get-line port)
                                        result))
                                (loop (get-line port)
                                      (cons (cons (string->lispified-symbol (match:substring match 1))
                                                  (list (cons 'total
                                                              (map cons
                                                                   mapping
                                                                   (map string->number
                                                                        (cdr (match-substrings match)))))
                                                        (cons 'partitions '())))
                                            result))))))))))))))

(define read-proc-net-dev!
  (let* ((mapping (list
                   'received-bytes 'received-packets 'received-errors 'received-drop
                   'received-fifo 'received-frame 'received-compressed 'received-multicast
                   'transmitted-bytes 'transmitted-packets 'transmitted-errors 'transmitted-drop
                   'transmitted-fifo 'transmitted-colls 'transmitted-carrier 'transmitted-compressed))
         (device-rgx (make-regexp (format #f "^ *([a-zA-Z0-9]+):~{~a~}$"
                                          (map (lambda (_) (identity " +([0-9]+)"))
                                               mapping)))))
    (lambda ()
      (call-with-input-file "/proc/net/dev"
        (lambda (port)
          ;; first two lines are header
          (get-line port)
          (get-line port)
          (let loop ((line (get-line port))
                     (result (list)))
            (if (eof-object? line)
                (reverse result)
                (let ((match (regexp-exec device-rgx line)))
                  (if (not match)
                      (error "Line does not match regex:" line)
                      (loop (get-line port)
                            (cons (cons (string->lispified-symbol
                                         (match:substring match 1))
                                        (map cons mapping
                                             (map string->number
                                                  (cdr (match-substrings match)))))
                                  result)))))))))))

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
          (let loop ((line (get-line port))
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
                               (loop (get-line port)
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
                               (loop (get-line port)
                                     result
                                     (cons (map cons
                                                (cons 'nr mapping)
                                                (map string->number
                                                     (match-substrings match)))
                                           cores)))
                              ((intr)
                               (loop (get-line port)
                                     (cons (cons name
                                                 (map string->number
                                                      (string-split (match:substring match 1)
                                                                    #\Space)))
                                           result)
                                     cores))
                              (else
                               (loop (get-line port)
                                     (cons (cons name
                                                 (string->number
                                                  (match:substring match 1)))
                                           result)
                                     cores))))))))))))))

(define (format-memory mem-info)
  (let* ((mem-total (get mem-info 'mem-total))
         (mem-available (get mem-info 'mem-available))
         (mem-used (- mem-total mem-available)))
    (apply format #f
           (if (> (/ mem-used mem-total) 0.9)
               "<span foreground=\"#da1000\">~4d</span>mb"
               "~4dmb")
           (list (ash mem-used -20)))))

(define (difference x y)
  (cond
   ((and (null? x) (null? y))
    '())
   ((null? x)
    (difference y y))
   ((null? y)
    (difference x x))
   ((and (pair? x)
         (or (pair? y)
             (error "x is pair while y is:" y)))
    (cons (difference (car x) (car y))
          (difference (cdr x) (cdr y))))
   ((and (symbol? x)
         (or (symbol? y)
             (error "x is symbol while y is:" y)))
    (if (not (equal? x y))
        (error "symbols x and y differ:" x y)
        x))
   ((and (number? x) (number? y))
    (- x y))
   (#t (error "expected x and y to be a number:" x y))))

(define (get alist . keys)
  (let ((cur (cdr (assv (car keys) alist))))
    (if (null? (cdr keys))
        cur
        (apply get cur (cdr keys)))))

(define (accumulate-alist op init alist keys)
  (accumulate (lambda (acc x)
                (unless (pair? x)
                  (error "element in alist is not a pair:" x))
                (op acc
                    (if (find (lambda (el) (equal? (car x) el))
                              keys)
                        (cdr x)
                        0)))
              init
              alist))

(define (cpu-usage cpu-stats)
  (let* ((all '(user nice system idle iowait irq softirq steal))
         (idle '(idle iowait))
         (total-cpu-time (accumulate-alist + 0 cpu-stats all))
         (idle-cpu-time (accumulate-alist + 0 cpu-stats idle)))
    (floor (* 100 (/ (- total-cpu-time idle-cpu-time)
                     total-cpu-time)))))

(define cpus-used!
  (let ((cache (read-proc-stat!)))
    (lambda ()
      (let* ((new (read-proc-stat!))
             (diff (difference new cache)))
        (set! cache new)
        (append (list (cpu-usage (get diff 'cpu)))
                (map cpu-usage (get diff 'cores)))))))

(define disk-used!
  (let ((cache (read-proc-diskstats!)))
    (lambda ()
      (let* ((new (read-proc-diskstats!))
             (diff (difference new cache)))
        (set! cache new)
        (let loop ((disks diff)
                   (result (list)))
          (if (null? disks)
              (reverse result)
              (loop (cdr disks)
                    (let* ((disk (car disks))
                           (disk-name (car disk))
                           (disk-total (get (cdr disk) 'total))
                           (read-bytes (* 512 (get disk-total 'sectors-read)))
                           (written-bytes (* 512 (get disk-total 'sectors-written))))
                      (cons (list disk-name read-bytes written-bytes)
                            result)))))))))

(define net-used!
  (let ((cache (read-proc-net-dev!)))
    (lambda ()
      (let* ((new (read-proc-net-dev!))
             (diff (difference new cache)))
        (set! cache new)
        (let loop ((devices diff)
                   (result (list)))
          (if (null? devices)
              (reverse result)
              (loop (cdr devices)
                    (let ((device (car devices)))
                      (cons (list (car device)
                                  (get (cdr device) 'received-bytes)
                                  (get (cdr device) 'transmitted-bytes))
                            result)))))))))

(define (format-cpu cpu)
  (apply format #f
         (if (> cpu 95)
             "<span foreground=\"#da1000\">~a</span>"
             "~a")
         (list (cond
                ((> cpu 90) "█")
                ((> cpu 80) "▇")
                ((> cpu 70) "▆")
                ((> cpu 60) "▅")
                ((> cpu 50) "▄")
                ((> cpu 40) "▃")
                ((> cpu 30) "▂")
                (else "▁")))))

(define (string->color-hash s)
  (let ((hash (string-hash s)))
    (apply string-append "#"
           (map (lambda (num) (format #f "~2,'0x" num))
                (list (ash (logand #xff0000 hash) -16)
                      (ash (logand #xff00 hash) -8)
                      (logand #xff hash))))))

(define (format-bar value max)
  (let ((percent (if (= max 0)
                     0
                     (* (/ value max) 100))))
    (apply format #f
           "<span foreground=\"~a\">~a</span>"
           (cond
            ((> percent 90) (list "#FF0000" "█"))
            ((> percent 80) (list "#FFAA00" "▇"))
            ((> percent 70) (list "#FFFF00" "▆"))
            ((> percent 60) (list "#CCCC00" "▅"))
            ((> percent 50) (list "#AAAA00" "▄"))
            ((> percent 40) (list "#AAFF00" "▃"))
            ((> percent 25) (list "#00AA00" "▂"))
            ((> percent  0) (list "#00FF00" "▁"))
            (else           (list "#FFFFFF" "▁"))))))

(define* (i3-block port full-text #:key short-text color background
                   border border-top border-right border-bottom border-left
                   min-width align urgent? name instance
                   (separator? #f) separator-block-width
                   (markup "pango")
                   (last? #f))
  (scm->json (filter (compose not unspecified?)
                     (list (cons "full_text" full-text)
                           (when short-text
                             (cons "short_text" short-text))
                           (when color
                             (cons "color" color))
                           (when background
                             (cons "background" background))
                           (when border
                             (cons "border" border))
                           (when border-top
                             (cons "border_top" border-top))
                           (when border-right
                             (cons "border_right" border-right))
                           (when border-bottom
                             (cons "border_bottom" border-bottom))
                           (when border-left
                             (cons "border_left" border-left))
                           (when min-width
                             (cons "min_width" min-width))
                           (when align
                             (cons "align" align))
                           (cons "urgent" urgent?)
                           (when name
                             (cons "name" name))
                           (when instance
                             (cons "instance" instance))
                           (cons "separator" separator?)
                           (when separator-block-width
                             (cons "separator_block_width" separator-block-width))
                           (when markup
                             (cons "markup" markup))))
             port)
  (when (not last?)
    (format port ",")))

(define* (update! #:optional (port stdout))
  (with-output-to-port port
    (lambda ()
      (format port ",[")
      (let ((fmt-triple (lambda (max name r w)
                          (format #f "~a ~a~a"
                                  name
                                  (format-bar r max)
                                  (format-bar w max)))))
        (map (lambda (net)
               (i3-block port
                         (apply fmt-triple (ash 1 23) net)
                         #:name "net"
                         #:instance (car net)
                         #:color "#C26DE8"))
             (net-used!))
        (map (lambda (disk)
               (i3-block port
                         (apply fmt-triple (ash 1 27) disk)
                         #:name "disk"
                         #:instance (car disk)
                         #:color "#7885FF"))
             (disk-used!))
        (i3-block port
                  (format-memory (read-proc-meminfo!))
                  #:name "mem"
                  #:color "#AAAA00")
        (i3-block port
                  (apply string-append (map format-cpu (cdr (cpus-used!))))
                  #:name "cpu"
                  #:color "#4AFFCD")
        (i3-block port
                  (strftime "%a, %e.%m.%Y"
                            (localtime (current-time)))
                  #:name "date"
                  #:color "#AFFFFF")
        (i3-block port
                  (strftime "%H:%M"
                            (localtime (current-time)))
                  #:name "time"
                  #:color "#FFFFFF"
                  #:last? #t))
      (format port "]~%")
      (force-output port))))

(define running #t)

(define (main-loop)
  (sleep 1)
  (when running
    (catch #t
      (lambda ()
        (update!))
      (lambda (key . args)
        (format stdout "Error ~a: ~a~%"
                key args)
        (sleep 9)))
    (main-loop)))

(define* (init #:key (version 1) (stop-signal 10) (cont-signal 12) (click-events #f) (spawn-server? #f))
  (when spawn-server?
    (let ((path "/tmp/guile-statusbar"))
      (when (file-exists? path)
        (delete-file path))
      (spawn-server (make-unix-domain-server-socket #:path path))))
  (scm->json (list (cons "version" version)
                   (cons "stop_signal" stop-signal)
                   (cons "cont_signal" cont-signal)
                   (cons "click_events" click-events))
             stdout)
  (format stdout "~%[[]~%")
  (main-loop))

(init #:spawn-server? #t)
