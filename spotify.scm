(define-module (guile-i3-bar spotify)
  #:export (<spotify>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (oop goops))

(define dbus-dest-spotify "org.mpris.MediaPlayer2.spotify")
(define dbus-dest-player "org.mpris.MediaPlayer2.Player")
(define dbus-object-path "/org/mpris/MediaPlayer2")
(define dbus-event "org.freedesktop.DBus.Properties.PropertiesChanged")

;; XXX We need to wait a bit before pulling Song info, otherwise we
;; XXX get info about the last song
(define sleep-hack 50000)

(define (parse-metadata str)
  (let* ((delim-rgx "<('|\\['|\"|\\[\")")
         (artist-rgx (make-regexp (format #f "'xesam:artist': ~a" delim-rgx)))
         (title-rgx (make-regexp (format #f "'xesam:title': ~a" delim-rgx)))
         (album-rgx (make-regexp (format #f "'xesam:album': ~a" delim-rgx))))
    (let ((artist (regexp-exec artist-rgx str))
          (title (regexp-exec title-rgx str))
          (album (regexp-exec album-rgx str)))
      (when (and artist title album)
        (list
         (cons 'artist (parse-string str (- (match:end artist) 1)))
         (cons 'title  (parse-string str (- (match:end title) 1)))
         (cons 'album  (parse-string str (- (match:end album) 1))))))))

(define (parse-playback-status str)
  (let* ((rgx (make-regexp "^\\(<'(Playing|Paused)'>,\\)"))
         (m (regexp-exec rgx str)))
    (if (not m)
        #f
        (cons 'playback-status
              (string->symbol (string-downcase (match:substring m 1)))))))

(define (run-spotify-cmd! cmd)
  (run-cmd!
   (string-join
    (list "gdbus" "call" "--session"
          "--dest" dbus-dest-spotify
          "--object-path" dbus-object-path
          "--method"
          (case cmd
            ((next previous play pause)
             (string-append "org.mpris.MediaPlayer2.Player."
                            (string-capitalize
                             (symbol->string cmd))))
            ((metadata playback-status)
             (string-join
              (list
               "org.freedesktop.DBus.Properties.Get"
               "org.mpris.MediaPlayer2.Player"
               (case cmd
                 ((metadata) "Metadata")
                 ((playback-status) "PlaybackStatus")))))
            (else (error "Invalid spotify cmd" cmd)))))))

(define-class <spotify> (<obj>))

(define-method (fetch (obj <spotify>))
  (let ((md (run-spotify-cmd! 'metadata))
        (ps (run-spotify-cmd! 'playback-status)))
    (and (not (null? md))
         (not (null? ps))
         (let ((p-md (parse-metadata (car md)))
               (p-ps (parse-playback-status (car ps))))
           (and p-md
                p-ps
                (cons p-ps
                      p-md))))))

(define-class <info> (<toggleable> <instance>)
  artist
  album
  title)

(define-class <prev> (<instance>))
(define-class <play-pause> (<instance>)
  playback-status)

(define-class <next> (<instance>))

(define-method (process (obj <spotify>) (diff <number>))
  (let ((data (slot-ref obj 'data)))
    (if (not data)
        (list)
        (list (get-instance obj 'prev <prev>)
              (update-slots (get-instance obj 'play-pause <play-pause>)
                            'playback-status (get data 'playback-status))
              (get-instance obj 'next <next>)
              (update-slots (get-instance obj 'info <info>)
                            'artist (get data 'artist)
                            'album (get data 'album)
                            'title (get data 'title))))))

(define-method (on-event (obj <prev>) (event <list>))
  (run-spotify-cmd! 'previous)
  (usleep sleep-hack)
  #t)

(define-method (fmt (obj <prev>))
  "⏮")

(define-method (on-event (obj <play-pause>) (event <list>))
  (let ((status (slot-ref obj 'playback-status)))
    (run-spotify-cmd! (case status
                        ((paused) 'play)
                        ((playing) 'pause)))
    (set! status
      (case status
        ((paused) 'playing)
        ((playing) 'paused)))
    (slot-set! obj 'playback-status status)
    #f))

(define-method (update (obj <play-pause>)))

(define-method (fmt (obj <play-pause>))
  (case (slot-ref obj 'playback-status)
    ((paused) "<span foreground=\"#DA1000\">⏵</span>")
    ((playing) "⏸")))

(define-method (on-event (obj <next>) (event <list>))
  (run-spotify-cmd! 'next)
  (usleep sleep-hack)
  #t)

(define-method (fmt (obj <next>))
  "⏭")

(define-method (fmt (obj <info>))
  (values
   (if (toggled? obj)
       (format #f "~a - ~a - ~a"
               (slot-ref obj 'artist)
               (slot-ref obj 'album)
               (slot-ref obj 'title))
       (format #f "~a" (slot-ref obj 'title)))
   #:markup #f))
