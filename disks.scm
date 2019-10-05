(define-module (guile-i3-bar disks)
  #:export (<disks>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (guile-i3-bar proc)
  #:use-module (oop goops))

(define-class <disks> (<obj>))

(define-class <disk> (<toggleable> <instance>)
  read
  written)

(define-method (fetch (obj <disks>))
  (read-proc-diskstats!))

(define-method (process (obj <disks>) (diff <number>))
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
                  (cons (update-slots (get-instance obj (car disk) <disk>)
                                      'read (round (/ (* 512 (get disk-total 'sectors-read))
                                                      diff))
                                      'written (round (/ (* 512 (get disk-total 'sectors-written))
                                                         diff)))
                        result)))))))

(define-method (fmt (obj <disk>))
  (let ((max (ash 1 27))
        (read (slot-ref obj 'read))
        (written (slot-ref obj 'written)))
    (values
     (format #f "~a ~a~a~a~a"
             (slot-ref obj 'id)
             (if (toggled? obj) (string-append (format-size read) " ") "")
             (format-bar read max)
             (format-bar written max)
             (if (toggled? obj) (string-append " " (format-size written)) ""))
     #:border (if (toggled? obj) "#777777" #f))))
