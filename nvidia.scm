(define-module (guile-i3-bar nvidia)
  #:export (<nvidia>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (oop goops))

(define (run-nvidia-settings!)
  (run-cmd!
   (string-join
    '("nvidia-settings" "-t" "-q" "GPUUtilization"))))

(define (nvidia-stats!)
  (let ((rgx (make-regexp "^graphics=([0-9]+), memory=([0-9]+), video=[0-9]+, PCIe=[0-9]+$")))
    (let loop ((lines (run-nvidia-settings!)))
      (if (null? lines)
          #f
          (let ((match (regexp-exec rgx (car lines))))
            (and match
                 (list
                  (cons 'usage (string->number (match:substring match 1)))
                  (cons 'memory (string->number (match:substring match 2))))))))))

(define-class <nvidia> (<obj>))

(define-class <usage> (<instance>)
  usage)

(define-class <memory> (<instance>)
  memory)

(define-method (fetch (obj <nvidia>))
  (nvidia-stats!))

(define-method (process (obj <nvidia>) (diff <number>))
  (let ((data (slot-ref obj 'data)))
    (if (not data)
        (list)
        (list
         (update-slots (get-instance obj 'usage <usage>)
                       'usage (get data 'usage))
         (update-slots (get-instance obj 'memory <memory>)
                       'memory (get data 'memory))))))

(define-method (fmt (obj <usage>))
  (values (format-bar (slot-ref obj 'usage) 100)
          #:separator-block-width 1))

(define-method (fmt (obj <memory>))
  (format-bar (slot-ref obj 'memory) 100))
