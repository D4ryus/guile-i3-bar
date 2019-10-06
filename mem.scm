(define-module (guile-i3-bar mem)
  #:export (<mem>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (guile-i3-bar proc)
  #:use-module (ice-9 receive)
  #:use-module (oop goops))

(define-class <mem> (<toggleable> <obj>)
  (used #:init-value 0)
  (mem-total #:init-value 1)
  (mem-free #:init-value 0)
  (cached #:init-value 0)
  (active #:init-value 0)
  (inactive #:init-value 0))

(define-method (fetch (obj <mem>))
  (read-proc-meminfo!))

(define-method (process (obj <mem>) (diff <number>))
  (let* ((mem-info (slot-ref obj 'data)))
    (map (lambda (id)
           (slot-set! obj id (ash (get mem-info id) -20)))
         (list 'mem-free
               'mem-total
               'cached
               'active
               'inactive))
    (slot-set! obj 'used (- (slot-ref obj 'mem-total)
                            (ash (get mem-info 'mem-available) -20)))
    (list)))

(define-method (fmt (obj <mem>))
  (let ((mem-used (slot-ref obj 'used))
        (mem-total (slot-ref obj 'mem-total)))
    (values
     (if (toggled? obj)
         (apply format #f
                "total: ~amb, free: ~amb, used: ~amb, cached: ~amb, active: ~amb, inactive: ~amb"
                (map (lambda (slot) (slot-ref obj slot))
                     '(mem-total mem-free used cached active inactive)))
         (apply format #f
                (if (> (/ mem-used mem-total) 0.9)
                    "<span foreground=\"#DA1000\">~4d</span>mb"
                    "~4dmb")
                (list mem-used)))
     #:border (if (toggled? obj) (slot-ref obj 'color) #f))))
