(define-module (guile-i3-bar net)
  #:export (<net>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (guile-i3-bar proc)
  #:use-module (oop goops))

(define-class <net> (<obj>)
  used)

(define-class <interface> (<instance>)
  (received #:init-keyword #:received)
  (transmitted #:init-keyword #:transmitted))

(define-method (fetch (obj <net>))
  (read-proc-net-dev!))

(define-method (process (obj <net>) (diff <number>))
  (let* ((cache (slot-ref obj 'old-data))
         (new (slot-ref obj 'data))
         (data (difference new cache)))
    (let loop ((devices data)
               (result (list)))
      (if (null? devices)
          (reverse result)
          (loop (cdr devices)
                (let ((device (car devices)))
                  (cons (make <interface>
                          #:obj obj
                          #:id (car device)
                          #:received (round
                                      (/ (get (cdr device) 'received-bytes)
                                         diff))
                          #:transmitted (round
                                         (/ (get (cdr device) 'transmitted-bytes)
                                            diff)))
                        result)))))))

(define-method (fmt (obj <interface>) (clicked? <boolean>))
  (let ((max (ash 1 23))
        (rec (slot-ref obj 'received))
        (trans (slot-ref obj 'transmitted)))
    (values
     (format #f "~a ~a~a~a~a"
             (slot-ref obj 'id)
             (if clicked? (string-append (format-size rec) " ") "")
             (format-bar rec max)
             (format-bar trans max)
             (if clicked? (string-append " " (format-size trans)) ""))
     #:border (if clicked? "#777777" #f))))
