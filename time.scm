(define-module (guile-i3-bar time)
  #:export (<time>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (oop goops))

(define-class <time> (<toggleable> <obj>))

(define-method (fetch (obj <time>))
  (current-time))

(define-method (fmt (obj <time>))
  (values
   (strftime (if (toggled? obj)
                 "%a, %e.%m.%Y %H:%M"
                 "%H:%M")
             (localtime (slot-ref obj 'data)))
   #:border (if (toggled? obj)
                (slot-ref obj 'color)
                #f)))
