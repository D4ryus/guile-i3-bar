(define-module (guile-i3-bar spotify)
  #:export (<spotify>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (oop goops))

;; XXX We need to wait a bit before pulling Song info, otherwise we
;; XXX get info about the last song
(define sleep-hack 50000)

(define (run-spotify-cmd! cmd)
  (run-cmd!
   (string-join
    (list "qdbus"
          "org.mpris.MediaPlayer2.spotify"
          "/org/mpris/MediaPlayer2"
          (string-append "org.mpris.MediaPlayer2.Player."
                         cmd)))))

(define (read-spotify-playback-status!)
  (let* ((output (run-spotify-cmd! "PlaybackStatus"))
         (rgx (make-regexp "^(Playing|Paused)$"))
         (match (and (not (null? output))
                     (regexp-exec rgx (car output)))))
    (if (not match)
        #f
        (cons 'playback-status
              (string->symbol (string-downcase (match:substring match 1)))))))

(define (read-spotify-info!)
  (let* ((output (run-spotify-cmd! "Metadata"))
         (match (lambda (string type rgx)
                  (let ((match (regexp-exec rgx string)))
                    (and match (cons type (match:substring match 1)))))))
    (if (null? output)
        #f
        (delete
         #f
         (map (lambda (line)
                ;; We are looking for lines matching:
                ;; xesam:album: Some album name
                ;; xesam:artist: Some artist
                ;; xesam:title: Some great Title
                (or (match line 'artist (make-regexp "^xesam:artist: (.*)$"))
                    (match line 'album  (make-regexp "^xesam:album: (.*)$"))
                    (match line 'title  (make-regexp "^xesam:title: (.*)$"))))
              output)))))

(define-class <spotify> (<obj>))

(define-method (fetch (obj <spotify>))
  (let ((info (read-spotify-info!)))
    (if (not info)
        #f
        (let ((playback-status (read-spotify-playback-status!)))
          (if (not playback-status)
              #f
              (append (list playback-status)
                      info))))))

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
  (run-spotify-cmd! "Previous")
  (usleep sleep-hack)
  #t)

(define-method (fmt (obj <prev>))
  "⏮")

(define-method (on-event (obj <play-pause>) (event <list>))
  (let ((status (slot-ref obj 'playback-status)))
    (run-spotify-cmd! (case status
                        ((paused) "Play")
                        ((playing) "Pause")))
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
  (run-spotify-cmd! "Next")
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
