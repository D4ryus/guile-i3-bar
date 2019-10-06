(define-module (guile-i3-bar sound)
  #:export (<sound>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 regex)
  #:use-module (oop goops))

(define (run-amixer-cmd! cmd)
  (run-cmd! (string-join
             (list "amixer -c 0 -M -D default"
                   (case cmd
                     ((read) "get Master")
                     ((toggle) "set Master toggle")
                     ((increase) "set Master 10%+")
                     ((decrease) "set Master 10%-")
                     (else (error "Unexpected amixer cmd" cmd)))))))

(define (read-amixer-info!)
  (let ((rgx (make-regexp "^  Front (Left|Right): Playback [0-9]+ \\[(100|[0-9][0-9]|0)%\\] \\[(on|off)\\]$")))
    (let loop ((lines (run-amixer-cmd! 'read)))
      (if (null? lines)
          #f
          (let ((match (regexp-exec rgx (car lines))))
            (if match
                (list
                 (cons 'volume (string->number (match:substring match 2)))
                 (cons 'muted?
                       (cond
                        ((equal? "on" (match:substring match 3)) #f)
                        ((equal? "off" (match:substring match 3)) #t))))
                (loop (cdr lines))))))))

(define-class <sound> (<obj>))

(define-class <mute> (<instance>)
  muted?)
(define-class <volume> (<instance>)
  volume)

(define-method (fetch (obj <sound>))
  (read-amixer-info!))

(define-method (process (obj <sound>) (diff <number>))
  (let ((data (slot-ref obj 'data)))
    (if (not data)
        (list)
        (list
         (update-slots (get-instance obj 'mute <mute>)
                       'muted? (get data 'muted?))
         (update-slots (get-instance obj 'volume <volume>)
                       'volume (get data 'volume))))))

(define-method (fmt (obj <mute>))
  (if (slot-ref obj 'muted?)
      "<span foreground=\"#DA1000\">🔇</span>"
      "🔊"))

(define-method (on-event (obj <mute>) (event <list>))
  (run-amixer-cmd! 'toggle)
  #t)

(define-method (fmt (obj <volume>))
  (let ((vol (slot-ref obj 'volume)))
    (apply format #f "~a<span foreground=\"#777777\">~a</span>"
           (cond
            ((=  vol 100) '("━━━━━━━━━━" ""))
            ((>= vol  90) '("━━━━━━━━━" "━"))
            ((>= vol  80) '("━━━━━━━━" "━━"))
            ((>= vol  70) '("━━━━━━━" "━━━"))
            ((>= vol  60) '("━━━━━━" "━━━━"))
            ((>= vol  50) '("━━━━━" "━━━━━"))
            ((>= vol  40) '("━━━━" "━━━━━━"))
            ((>= vol  30) '("━━━" "━━━━━━━"))
            ((>= vol  20) '("━━" "━━━━━━━━"))
            ((>= vol  10) '("━" "━━━━━━━━━"))
            (else        '("" "━━━━━━━━━━"))))))

(define-method (on-event (obj <volume>) (event <list>))
  (case (get event "button")
    ((4) (run-amixer-cmd! 'increase))
    ((5) (run-amixer-cmd! 'decrease)))
  #t)
