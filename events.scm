(define-module (guile-i3-bar events)
  #:use-module (oop goops)
  #:use-module (json)
  #:use-module (ice-9 rdelim)
  #:use-module (guile-i3-bar misc)
  #:export (clicked?
            process-click-events))

(define clicked (list))

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

(define* (read-click-event port)
  (let ((event (json-string->scm (read-line port))))
    (make-click-event (get event "name")
                      (get event "instance"))))

(define* (click-event-pending? port)
  (char-ready? port))

(define-generic clicked?)

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

(define* (process-click-events port)
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
