(define-module (guile-i3-bar mem)
  #:export (<mem>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (guile-i3-bar proc)
  #:use-module (ice-9 receive)
  #:use-module (oop goops))

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
