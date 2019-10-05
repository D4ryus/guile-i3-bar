(define-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar events)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:export (<obj>
            <instance>
            process
            fetch
            fmt
            unclick
            update!
            print!))

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

(define-method (clicked? (obj <obj>))
  (clicked? (slot-ref obj 'name) #f))

(define-method (unclick (obj <obj>))
  (delete-click-event
   (make-click-event (slot-ref obj 'name) #f)))

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

(define-method (fmt (obj <obj>) (clicked? <boolean>))
  #f)

(define-method (print (obj <obj>))
  (let ((to-i3-obj
         (lambda* (#:optional (instance #f))
           (receive (text . args)
               (fmt (or instance obj) (clicked? (or instance obj)))
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

(define-class <instance> ()
  (obj #:init-form (error "<obj> required")
       #:init-keyword #:obj)
  (id #:init-form (error "Id required")
      #:init-keyword #:id))

(define-method (clicked? (obj <instance>))
  (clicked? (slot-ref (slot-ref obj 'obj) 'name)
            (slot-ref obj 'id)))

(define-method (unclick (obj <instance>))
  (delete-click-event
   (make-click-event (slot-ref (slot-ref obj 'obj) 'name)
                     (slot-ref obj 'id))))

(define-method (fmt (obj <instance>) (clicked? <boolean>))
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
