(define-module (guile-i3-bar cpu)
  #:export (<cpu>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (guile-i3-bar proc)
  #:use-module (oop goops))

(define-class <cpu> (<obj>))

(define-class <core> (<instance>)
  percent
  last?)

(define-method (fetch (obj <cpu>))
  (read-proc-stat!))

(define-method (process (obj <cpu>) (diff <number>))
  (let ((cpu-usage (lambda (cpu-stats)
                     (let* ((all '(user nice system idle iowait irq softirq steal))
                            (idle '(idle iowait))
                            (total-cpu-time (accumulate-alist + 0 cpu-stats all))
                            (idle-cpu-time (accumulate-alist + 0 cpu-stats idle)))
                       (floor (* 100 (/ (- total-cpu-time idle-cpu-time)
                                        total-cpu-time))))))
        (data (difference (slot-ref obj 'data) (slot-ref obj 'old-data))))
    (let loop ((cores (map cpu-usage (get data 'cores)))
               (i 0)
               (result (list)))
      (if (null? cores)
          (reverse result)
          (loop (cdr cores)
                (+ i 1)
                (cons
                 (update-slots (get-instance obj
                                             (string->lispified-symbol (format #f "core-~a" i))
                                             <core>)
                               'percent (car cores)
                               'last? (null? (cdr cores)))
                 result))))))

(define-method (fmt (obj <core>))
  (let ((percent (slot-ref obj 'percent)))
    (values (apply format #f
                   (if (> percent 95)
                       "<span foreground=\"#DA1000\">~a</span>"
                       "~a")
                   (list (cond
                          ((> percent 90) "█")
                          ((> percent 80) "▇")
                          ((> percent 70) "▆")
                          ((> percent 60) "▅")
                          ((> percent 50) "▄")
                          ((> percent 40) "▃")
                          ((> percent 30) "▂")
                          (else "▁"))))
            #:separator-block-width (if (slot-ref obj 'last?) #f 1))))
