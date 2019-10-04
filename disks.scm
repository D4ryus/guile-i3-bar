(define-module (guile-i3-bar disks)
  #:export (<disks>)
  #:use-module (oop goops)
  #:use-module (guile-i3-bar proc)
  #:use-module (guile-i3-bar misc)
  #:use-module (guile-i3-bar classes))

(define-class <disks> (<obj>))

(define-class <disk> (<instance>)
  (read #:init-keyword #:read)
  (written #:init-keyword #:written))

(define-method (fetch (obj <disks>))
  (read-proc-diskstats!))

(define-method (adjust (obj <disks>) (diff <number>))
  (let* ((cache (slot-ref obj 'old-data))
         (new (slot-ref obj 'data))
         (data (difference new cache)))
    (let loop ((disks data)
               (result (list)))
      (if (null? disks)
          (reverse result)
          (loop (cdr disks)
                (let* ((disk (car disks))
                       (disk-total (get (cdr disk) 'total)))
                  (cons (make <disk>
                          #:obj obj
                          #:id (car disk)
                          #:read (round (/ (* 512 (get disk-total 'sectors-read))
                                           diff))
                          #:written (round (/ (* 512 (get disk-total 'sectors-written))
                                              diff)))
                        result)))))))

(define-method (fmt (obj <disk>) (clicked? <boolean>))
  (let ((max (ash 1 27))
        (read (slot-ref obj 'read))
        (written (slot-ref obj 'written)))
    (values
     (format #f "~a ~a~a~a~a"
             (slot-ref obj 'id)
             (if clicked? (string-append (format-size read) " ") "")
             (format-bar read max)
             (format-bar written max)
             (if clicked? (string-append " " (format-size written)) ""))
     #:border (if clicked? "#777777" #f))))
