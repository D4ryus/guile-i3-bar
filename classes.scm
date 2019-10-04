(define-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar events)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:export (<obj>
            <instance>
            adjust
            fetch
            fmt
            update))

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
