(define-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:export (<obj>
            <instance>
            <toggleable>
            fetch
            fmt
            get-instance
            on-event
            print!
            process
            toggled?
            update
            update-slots
            update!))

(define-method (update-slots obj . slots)
  (let loop ((slots slots))
    (when (not (null? slots))
      (slot-set! obj (first slots) (second slots))
      (loop (cddr slots)))
    obj))

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
(define-generic process)
(define-generic fmt)
(define-generic on-event)

(define-method (update (obj <obj>) (process? <boolean>))
  (slot-set! obj 'old-data (slot-ref obj 'data))
  (slot-set! obj 'data (fetch obj))
  (let* ((last (slot-ref obj 'time))
         (current (current-tick))
         (diff (/ (- current last) 1000)))
    (when (= diff 0)
      (set! diff 1))
    (slot-set! obj 'time current)
    (slot-set! obj 'diff diff)
    (when process?
      (slot-set! obj 'instances (process obj diff)))))

(define-method (process (obj <obj>) (diff <number>))
  (list))

(define-method (fmt (obj <obj>))
  #f)

(define-method (print (obj <obj>))
  (let ((to-i3-obj
         (lambda* (#:optional (instance #f))
           (receive (text . args)
               (fmt (or instance obj))
             (if (not text)
                 #f
                 (apply i3-block text
                        (append
                         (list #:name (slot-ref obj 'name)
                               #:color (slot-ref obj 'color)
                               #:instance (and instance (slot-ref instance 'id)))
                         args)))))))
    (let ((list (delete #f
                        (append (list (to-i3-obj))
                                (map to-i3-obj (slot-ref obj 'instances))))))
      (if (null? list)
          #f
          (string-join list ",")))))

(define-method (on-event (obj <obj>) (event <list>))
  #f)

(define-method (get-instance (obj <obj>) (id <symbol>))
  (find (lambda (instance)
          (equal? (slot-ref instance 'id)
                  id))
        (slot-ref obj 'instances)))

(define-method (get-instance (obj <obj>) (id <symbol>) (instance <class>))
  (or (get-instance obj id)
      (make instance
        #:id id
        #:obj obj)))

(define-class <instance> ()
  (obj #:init-form (error "<obj> required")
       #:init-keyword #:obj)
  (id #:init-form (error "Id required")
      #:init-keyword #:id))

(define-method (fmt (obj <instance>))
  (error "Implement fmt for" obj))

(define* (update! objs #:optional (process? #t))
  (map (lambda (obj)
         (update obj process?))
       objs))

(define (print! port objs)
  (with-output-to-port port
    (lambda ()
      (format port ",[~a]~%"
              (string-join (delete #f (map print objs)) ","))
      (force-output port))))

(define-method (on-event (obj <instance>) (event <list>))
  #f)

(define-class <toggleable> ()
  (toggled? #:init-value #f))

(define-method (on-event (obj <toggleable>) (event <list>))
  (slot-set! obj 'toggled?
             (not (slot-ref obj 'toggled?)))
  #f)

(define-method (toggled? (obj <toggleable>))
  (slot-ref obj 'toggled?))
