(define-module (guile-i3-bar main)
  #:export (main)
  #:use-module (guile-i3-bar battery)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar cpu)
  #:use-module (guile-i3-bar disks)
  #:use-module (guile-i3-bar mem)
  #:use-module (guile-i3-bar misc)
  #:use-module (guile-i3-bar net)
  #:use-module (guile-i3-bar proc)
  #:use-module (guile-i3-bar spotify)
  #:use-module (guile-i3-bar time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (srfi srfi-1)
  #:use-module (system repl server))

(define stdout (current-output-port))
(define stdin (current-input-port))
(define main-thread (current-thread))
(define main-loop-error #f)
(define event-pending? #f)
(define events (list))
(define events-mutex (make-mutex))

(define (push-event event)
  (with-mutex events-mutex
    (set! events (cons event events))))

(define (pop-event)
  (with-mutex events-mutex
    (if (null? events)
        #f
        (let ((event (car events)))
          (set! events (cdr events))
          event))))

(define (handle-events)
  (let loop ((event (pop-event)))
    (when event
      (let ((name (string->symbol (get event "name")))
            (instance (get event "instance")))
        (when instance
          (set! instance (string->symbol instance)))
        (map (lambda (obj)
               (when (equal? (slot-ref obj 'name) name)
                 (if (not instance)
                     (when (on-event obj event)
                       (update obj #t))
                     (let ((inst (get-instance obj instance)))
                       (when (and inst (on-event inst event))
                         (update obj #t))))))
             objs)
        (loop (pop-event))))))

(define running #t)

(define (main-loop)
  (let loop ((sleep 1000000))
    (when (> sleep 0)
      (when event-pending?
        (handle-events)
        (print! stdout objs)
        (set! event-pending? #f))
      (loop (usleep sleep))))
  (when running
    (update! objs)
    (print! stdout objs))
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
  (update! objs #f)

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
    ((= signal SIGUSR2) (set! event-pending? #t))))

(define objs
  (list (make <spotify> #:name 'spotify #:color "#6AE368")
        (make <net>     #:name 'net     #:color "#D66563")
        (make <disks>   #:name 'disks   #:color "#9895FA")
        (make <mem>     #:name 'mem     #:color "#E8D900")
        (make <cpu>     #:name 'cpu     #:color "#4AFFCD")
        (make <battery> #:name 'battery #:color "#FFAAFF")
        (make <time>    #:name 'time    #:color "#FFFFFF")))

(define* (process-click-events port)
  ;; Format is:
  ;; [\n{ ... }\n,{ ... }\n ...
  ;; So if we read a [ it means we just starded processing events
  (when (equal? (read-char port) #\[)
    (read-line port))
  (push-event (json-string->scm (read-line port)))
  (raise SIGUSR2)
  (process-click-events port))

(define (main)
  (sigaction SIGUSR1 signal-handler)
  (sigaction SIGCONT signal-handler)
  (sigaction SIGUSR2 signal-handler)
  (call-with-new-thread
   (lambda ()
     (let loop ()
       (catch #t
         (lambda () (process-click-events stdin))
         (lambda (key . parameters)
           (set! main-loop-error (cons key parameters))
           ;; Sleep so that one can attach via geiser and check what
           ;; error was thrown
           (sleep 999)
           (loop))))))

  (init #:spawn-server? #t #:click-events #t))
