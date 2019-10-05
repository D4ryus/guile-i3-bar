(define-module (guile-i3-bar battery)
  #:export (<battery>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 rdelim)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1))

(define-class <battery> (<toggleable> <obj>)
  (exists? #:init-value #f)
  (path #:init-value "/sys/class/power_supply/BAT0"
        #:init-keyword #:path)
  percent
  status)

(define-method (fetch (obj <battery>))
  (let* ((path (slot-ref obj 'path))
         (status (string-append path "/status"))
         (full (string-append path "/energy_full"))
         (now (string-append path "/energy_now")))
    (if (any (compose not file-exists?)
             (list status full now))
        #f
        (map (lambda (file convert)
               (call-with-input-file file
                 (lambda (port)
                   (convert (read-line port)))))
             (list status full now)
             (list identity string->number string->number)))))

(define-method (process (obj <battery>) (diff <number>))
  (let ((data (slot-ref obj 'data)))
    (if (not data)
        (slot-set! obj 'exists? #f)
        (apply (lambda (status full now)
                 (slot-set! obj 'exists? #t)
                 (slot-set! obj 'status status)
                 (slot-set! obj 'percent
                            (round (* 100 (/ now (if (= 0 full) 1 full))))))
               data))
    (list)))

(define-method (fmt (obj <battery>))
  (if (not (slot-ref obj 'exists?))
      #f
      (if (toggled? obj)
          ;; XXX red when low
          (format #f "~a ~a%"
                  (slot-ref obj 'status)
                  (slot-ref obj 'percent))
          (format #f "~a%"
                  (slot-ref obj 'percent)))))
