(define-module (guile-i3-bar time)
  #:export (<time>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (oop goops))

(define-class <time> (<obj>))

(define-method (fetch (obj <time>))
  (slot-set! obj 'data (current-time)))

(define-method (fmt (obj <time>) (clicked? <boolean>))
  (strftime (if clicked?
                "%a, %e.%m.%Y %H:%M"
                "%H:%M")
            (localtime (slot-ref obj 'data))))
