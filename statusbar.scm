(use-modules (ice-9 format)
             (ice-9 pretty-print)
             (ice-9 rdelim)
             (ice-9 receive)
             (ice-9 regex)
             (ice-9 threads)
             (json)
             (oop goops)
             (oop goops describe)
             (system repl server)
             (srfi srfi-1))

(define stdout (current-output-port))
(define stdin (current-input-port))
(define main-thread (current-thread))
(define main-loop-error #f)

;; --- helper

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

(define (get alist . keys)
  (let ((cur (assoc (car keys) alist)))
    (if (not cur)
        #f
        (begin
          (set! cur (cdr cur))
          (if (null? (cdr keys))
              cur
              (apply get cur (cdr keys)))))))

(define (match-substrings match)
  (let loop ((nr 1)
             (substrings (list)))
    (if (>= nr (match:count match))
        (reverse substrings)
        (loop (+ nr 1)
              (cons (match:substring match nr)
                    substrings)))))

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (accumulate op
                  (op init (car sequence))
                  (cdr sequence))))

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

(define format-size
  (let ((xb (ash 1 53)) ;; 8xb
        (tb (ash 1 43)) ;; 8tb
        (gb (ash 1 33)) ;; 8gb
        (mb (ash 1 23)) ;; 8mb
        (kb (ash 1 13)));; 8kb
    (lambda (size)
      (format #f "<span foreground=\"#FFFFFF\">~a</span>"
              (cond
               ((> size xb) (format #f "~4dpb" (ash size -50)))
               ((> size tb) (format #f "~4dtb" (ash size -40)))
               ((> size gb) (format #f "~4dgb" (ash size -30)))
               ((> size mb) (format #f "~4dmb" (ash size -20)))
               ((> size kb) (format #f "~4dkb" (ash size -10)))
               (#t          (format #f "~4db " size)))))))

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

;; --- proc

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

;; --- click event handling

(define (make-click-event name instance)
  (when (string? name)
    (set! name (string->lispified-symbol name)))
  (when (string? instance)
    (set! instance (string->lispified-symbol instance)))
  (cons name instance))

(define (click-event-name click-event)
  (car click-event))

(define (click-event-instance click-event)
  (cdr click-event))

(define* (read-click-event #:optional (port stdin))
  (let ((event (json-string->scm (read-line port))))
    (make-click-event (get event "name")
                      (get event "instance"))))

(define* (click-event-pending? #:optional (port stdin))
  (char-ready? port))

(define clicked (list))

(define-method (clicked? (name <symbol>) instance)
  (unless (or (is-a? instance <boolean>)
              (is-a? instance <symbol>))
    (error "Instance neither boolean nor symbol" instance))
  (let loop ((list clicked))
    (if (null? list)
        #f
        (if (and (equal? name (click-event-name (car list)))
                 (or (not instance)
                     (equal? instance (click-event-instance (car list)))))
            #t
            (loop (cdr list))))))

(define-method (clicked? (name <symbol>))
  (clicked? name #f))

(define (add-click-event click-event)
  (set! clicked (append (list click-event)
                        clicked)))

(define (delete-click-event click-event)
  (set! clicked (delete click-event clicked)))

(define* (process-click-events #:optional (port stdin))
  ;; Format is:
  ;; [\n{ ... }\n,{ ... }\n ...
  ;; So if we read a [ it means we just starded processing events
  (when (equal? (read-char port) #\[)
    (read-line port))
  (let ((click-event (read-click-event port)))
    (if (member click-event clicked)
        (delete-click-event click-event)
        (add-click-event click-event)))
  (raise SIGUSR2)
  (process-click-events port))

;; --- classes

(define (current-tick)
  (let* ((time (gettimeofday))
         (sec (car time))
         (micro-sec (cdr time)))
    (floor (+ (* sec 1000)
              (/ micro-sec 1000)))))

(define-class <obj> ()
  (time #:init-value 0)
  (diff #:init-value 0)
  (old-data #:init-value #f)
  (data #:init-value #f)
  (name #:init-form (error "Name required")
        #:init-keyword #:name)
  (color #:init-form (error "Color required")
         #:init-keyword #:color)
  (instances #:init-value (list)))

(define-generic fetch)
(define-generic adjust)
(define-generic fmt)

(define-method (clicked? (obj <obj>))
  (clicked? (slot-ref obj 'name) #f))

(define-method (update (obj <obj>))
  (slot-set! obj 'old-data (slot-ref obj 'data))
  (slot-set! obj 'data (fetch obj))
  (let* ((last (slot-ref obj 'time))
         (current (current-tick))
         (diff (/ (- current last) 1000)))
    (slot-set! obj 'time current)
    (slot-set! obj 'diff (if (= diff 0) 1 diff))))

(define-method (adjust (obj <obj>) (diff <number>))
  (error "Implement adjust for" obj))

(define-method (fmt (obj <obj>) (clicked <boolean>))
  (string-join
   (map (lambda (instance)
          (receive (fmt . args)
              (fmt instance (clicked? instance))
            (apply i3-block fmt
                   (append
                    (list #:name (slot-ref obj 'name)
                          #:color (slot-ref obj 'color)
                          #:instance (slot-ref instance 'id))
                    args))))
        (slot-ref obj 'instances))
   ","))

(define-class <instance> ()
  (obj #:init-form (error "<obj> required")
       #:init-keyword #:obj)
  (id #:init-form (error "Id required")
      #:init-keyword #:id))

(define-method (clicked? (obj <instance>))
  (clicked? (slot-ref (slot-ref obj 'obj) 'name)
            (slot-ref obj 'id)))

(define-method (fmt (obj <instance>) (clicked? <boolean>))
  (error "Implement fmt for" obj))

;; --- net

(define-class <net> (<obj>)
  used)

(define-class <interface> (<instance>)
  (received #:init-keyword #:received)
  (transmitted #:init-keyword #:transmitted))

(define-method (fetch (obj <net>))
  (read-proc-net-dev!))

(define-method (adjust (obj <net>) (diff <number>))
  (let* ((cache (slot-ref obj 'old-data))
         (new (slot-ref obj 'data))
         (data (difference new cache)))
    (let loop ((devices data)
               (result (list)))
      (if (null? devices)
          (reverse result)
          (loop (cdr devices)
                (let ((device (car devices)))
                  (cons (make <interface>
                          #:obj obj
                          #:id (car device)
                          #:received (round
                                      (/ (get (cdr device) 'received-bytes)
                                         diff))
                          #:transmitted (round
                                         (/ (get (cdr device) 'transmitted-bytes)
                                            diff)))
                        result)))))))

(define-method (fmt (obj <interface>) (clicked? <boolean>))
  (let ((max (ash 1 23))
        (rec (slot-ref obj 'received))
        (trans (slot-ref obj 'transmitted)))
    (values
     (format #f "~a ~a~a~a~a"
             (slot-ref obj 'id)
             (if clicked? (format-size rec) "")
             (format-bar rec max)
             (format-bar trans max)
             (if clicked? (format-size trans) ""))
     #:border (if clicked? "#777777" #f))))

;; --- disk

(define-class <disks> (<obj>))

(define-class <disk> (<instance>)
  (read #:init-keyword #:read)
  (written #:init-keyword #:written))

(define-method (fetch (obj <disks>))
  (read-proc-diskstats!))

(define-method (adjust (obj <disks>) (diff <number>))
  (let* ((cache (slot-ref obj 'old-data))
         (new (slot-ref obj 'data))
         (data (difference new cache)))
    (let loop ((disks data)
               (result (list)))
      (if (null? disks)
          (reverse result)
          (loop (cdr disks)
                (let* ((disk (car disks))
                       (disk-total (get (cdr disk) 'total)))
                  (cons (make <disk>
                          #:obj obj
                          #:id (car disk)
                          #:read (round (/ (* 512 (get disk-total 'sectors-read))
                                           diff))
                          #:written (round (/ (* 512 (get disk-total 'sectors-written))
                                              diff)))
                        result)))))))

(define-method (fmt (obj <disk>) (clicked? <boolean>))
  (let ((max (ash 1 27))
        (read (slot-ref obj 'read))
        (written (slot-ref obj 'written)))
    (values
     (format #f "~a ~a~a~a~a"
             (slot-ref obj 'id)
             (if clicked? (format-size read) "")
             (format-bar read max)
             (format-bar written max)
             (if clicked? (format-size written) ""))
     #:border (if clicked? "#777777" #f))))

;; --- cpu

(define-class <cpu> (<obj>))

(define-class <core> (<instance>)
  (percent #:init-keyword #:percent)
  (last? #:init-keyword #:last?
         #:init-value #f))

(define-method (fetch (obj <cpu>))
  (read-proc-stat!))

(define-method (adjust (obj <cpu>) (diff <number>))
  (let ((cpu-usage (lambda (cpu-stats)
                     (let* ((all '(user nice system idle iowait irq softirq steal))
                            (idle '(idle iowait))
                            (total-cpu-time (accumulate-alist + 0 cpu-stats all))
                            (idle-cpu-time (accumulate-alist + 0 cpu-stats idle)))
                       (floor (* 100 (/ (- total-cpu-time idle-cpu-time)
                                        total-cpu-time))))))
        (data (difference (slot-ref obj 'data) (slot-ref obj 'old-data))))
    (let loop ((cores (map cpu-usage (get data 'cores)))
               (i 0)
               (result (list)))
      (if (null? cores)
          (reverse result)
          (loop (cdr cores)
                (+ i 1)
                (cons (make <core>
                        #:obj obj
                        #:id (string->lispified-symbol
                              (format #f "core-~a" i))
                        #:percent (car cores)
                        #:last? (null? (cdr cores)))
                      result))))))

(define-method (fmt (obj <core>) (clicked? <boolean>))
  (let ((percent (slot-ref obj 'percent)))
    (values (apply format #f
                   (if (> percent 95)
                       "<span foreground=\"#DA1000\">~a</span>"
                       "~a")
                   (list (cond
                          ((> percent 90) "█")
                          ((> percent 80) "▇")
                          ((> percent 70) "▆")
                          ((> percent 60) "▅")
                          ((> percent 50) "▄")
                          ((> percent 40) "▃")
                          ((> percent 30) "▂")
                          (else "▁"))))
            #:separator-block-width (if (slot-ref obj 'last?) #f 1))))

;; --- mem

(define-class <mem> (<obj>)
  total
  used)

(define-method (fetch (obj <mem>))
  (read-proc-meminfo!))

(define-method (adjust (obj <mem>) (diff <number>))
  (let* ((mem-info (slot-ref obj 'data))
         (mem-total (get mem-info 'mem-total)))
    (slot-set! obj 'total mem-total)
    (slot-set! obj 'used (- mem-total (get mem-info 'mem-available)))
    (list)))

(define-method (fmt (obj <mem>) (clicked? <boolean>))
  (let ((mem-used (slot-ref obj 'used))
        (mem-total (slot-ref obj 'total)))
    (i3-block (apply format #f
                     (if (> (/ mem-used mem-total) 0.9)
                         "<span foreground=\"#DA1000\">~4d~a</span>mb"
                         "~4d~amb")
                     (list (ash mem-used -20)
                           (if clicked?
                               (format #f "/~4d" (ash mem-total -20))
                               "")))
              #:name (slot-ref obj 'name)
              #:color (slot-ref obj 'color)
              #:border (if clicked? "#777777" #f))))

;; --- battery

(define-class <bat> (<obj>)
  (path #:init-value "/sys/class/power_supply/BAT0"
        #:init-keyword #:path)
  percent
  status)

(define-method (fetch (obj <bat>))
  (let* ((path (slot-ref obj 'path))
         (status (string-append path "/status"))
         (full (string-append path "/energy_full"))
         (now (string-append path "/energy_now")))
    (map (lambda (file convert)
           (call-with-input-file file
             (lambda (port)
               (convert (read-line port)))))
         (list status full now)
         (list identity string->number string->number))))

(define-method (adjust (obj <bat>) (diff <number>))
  (apply (lambda (status full now)
           (slot-set! obj 'status status)
           (slot-set! obj 'percent
                      (round (* 100 (/ now (if (= 0 full) 1 full))))))
         (slot-ref obj 'data))
  (list))

(define-method (fmt (obj <bat>) (clicked? <boolean>))
  (i3-block (if clicked?
                ;; XXX red when low
                (format #f "~a ~a%"
                        (slot-ref obj 'status)
                        (slot-ref obj 'percent))
                (format #f "~a%"
                        (slot-ref obj 'percent)))
            #:name (slot-ref obj 'name)
            #:color (slot-ref obj 'color)))

;; --- time

(define-class <time> (<obj>))

(define-method (fetch (obj <time>))
  (slot-set! obj 'data (current-time)))

(define-method (adjust (obj <time>) (diff <number>))
  (list))

(define-method (fmt (obj <time>) (clicked? <boolean>))
  (i3-block (strftime (if clicked?
                          "%a, %e.%m.%Y %H:%M"
                          "%H:%M")
                      (localtime (slot-ref obj 'data)))
            #:name (slot-ref obj 'name)
            #:color (slot-ref obj 'color)))

;; ---

(define* (i3-block full-text #:key short-text color background
                   border (border-top 0) (border-right 0) (border-bottom 2) (border-left 0)
                   min-width align urgent? name instance
                   (separator? #f) separator-block-width
                   (markup "pango"))
  (scm->json-string
   (filter (compose not unspecified?)
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
                   (cons "markup" markup))))))

(define* (print! #:optional (port stdout))
  (with-output-to-port port
    (lambda ()
      (format port ",[")
      (format port "~a" (string-join (map (lambda (obj)
                                            (fmt obj (clicked? obj)))
                                          objs)
                                     ","))
      (format port "]~%")
      (force-output port))))

(define running #t)

(define (main-loop)
  (let loop ((sleep 1000000))
    (when (> sleep 0)
      (loop (usleep sleep))))
  (when running
    (map update objs)
    (map (lambda (obj)
           (slot-set! obj 'instances
                      (adjust obj (slot-ref obj 'diff))))
         objs)
    (print!))
  (main-loop))

(define* (init #:key (version 1) (stop-signal SIGUSR1) (cont-signal SIGCONT)
               (click-events #f) (spawn-server? #f))
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
  (map update objs)

  (main-loop)
  (catch #t
    (lambda () (main-loop))
    (lambda (key . parameters)
      (set! main-loop-error (cons key parameters))
      ;; Sleep so that one can attach via geiser and check what
      ;; error was thrown
      (sleep 999)))
  (format stdout "]~%"))

(define (signal-handler signal)
  (cond
    ((= signal SIGUSR1) (set! running #f))
    ((= signal SIGCONT) (set! running #t))
    ((= signal SIGUSR2) (print!))))

(define objs
  (list (make <net>   #:name 'net   #:color "#D66563")
        (make <disks> #:name 'disks #:color "#9895FA")
        (make <mem>   #:name 'mem   #:color "#E8D900")
        (make <cpu>   #:name 'cpu   #:color "#4AFFCD")
        (make <bat>   #:name 'bat   #:color "#FFAAFF")
        (make <time>  #:name 'time  #:color "#FFFFFF")))

(sigaction SIGUSR1 signal-handler)
(sigaction SIGCONT signal-handler)
(sigaction SIGUSR2 signal-handler)
(call-with-new-thread
 (lambda ()
   (process-click-events stdin)))
(init #:spawn-server? #t #:click-events #t)
