(define-module (guile-i3-bar timer)
  #:export (<timer>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-18)) ;; threads

(define-class <timer> (<obj>)
  (seconds #:init-value 0)
  (state #:init-value #:idle)
  (done-at #:init-value 0))

(define-method (fetch (obj <timer>))
  (when (eq? #:running (slot-ref obj 'state))
    (let ((new-value (- (slot-ref obj 'done-at)
                        (car (current-time)))))
      (slot-set! obj 'seconds
                 (if (<= new-value 0)
                     (begin (slot-set! obj 'state #:done)
                            0)
                     new-value)))))

(define-method (fmt (obj <timer>))
  (let ((seconds (slot-ref obj 'seconds)))
    (values
     (if (eq? #:done (slot-ref obj 'state))
         (format #f "<span foreground=\"~a\">~a</span>"
                 (if (= 0 (logand #x01 (car (current-time))))
                     "#FF0000"
                     "#00FF00")
                 "DONE!")
         (format #f "~2,'0d:~2,'0d"
                 (floor/ seconds 60)
                 (modulo seconds 60))))))

(define-method (on-event (obj <timer>) (event <list>))
  (let ((mods (array->list (assoc-ref event "modifiers")))
        (button (assoc-ref event "button")))
    (define (pressed? modifier)
      (member modifier mods))
    (case button
      ((4 5)
       (let* ((op (case button
                    ((4) +)
                    ((5) -)))
              (new-value (op (slot-ref obj 'seconds)
                             (cond
                              ((and (pressed? "Shift")
                                    (pressed? "Control")
                                    (pressed? "Mod1")) ;; alt
                               (* 10 60))
                              ((and (pressed? "Shift")
                                    (pressed? "Control"))
                               (* 5 60))
                              ((pressed? "Mod1") ;; alt
                               60)
                              ((pressed? "Shift")
                               30)
                              ((pressed? "Control")
                               10)
                              (else 1)))))
         (slot-set! obj 'seconds
                    (if (< new-value 0)
                        0
                        new-value))))
      ((1)
       (slot-set! obj 'state
                  (case (slot-ref obj 'state)
                    ((#:running) #:idle)
                    ((#:idle)
                     (begin
                       (slot-set! obj 'done-at
                                  (+ (slot-ref obj 'seconds)
                                     (car (current-time))))
                       #:running))
                    ((#:done) #:idle))))
      ((3)
       (begin
         (slot-set! obj 'state #:idle)
         (slot-set! obj 'seconds 0))))))
