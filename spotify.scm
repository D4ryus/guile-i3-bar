(define-module (guile-i3-bar spotify)
  #:export (<spotify>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (oop goops))

(define (run-spotify-cmd! cmd)
  (let* ((pipe (open-input-pipe
                (string-join (list "qdbus"
                                   "org.mpris.MediaPlayer2.spotify"
                                   "/org/mpris/MediaPlayer2"
                                   (string-append "org.mpris.MediaPlayer2.Player."
                                                  cmd))))))
    (let loop ((line (read-line pipe))
               (result (list)))
      (if (eof-object? line)
          (begin
            (close-pipe pipe)
            (reverse result))
          (loop (read-line pipe)
                (cons line result))))))

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

(define-class <spotify> (<obj>)
  (running? #:init-value #f))

(define-method (fetch (obj <spotify>))
  (let ((info (read-spotify-info!)))
    (if (not info)
        #f
        (let ((playback-status (read-spotify-playback-status!)))
          (if (not playback-status)
              #f
              (append (list playback-status)
                      info))))))

(define-class <sp-info> (<instance>)
  (artist #:init-keyword #:artist)
  (album  #:init-keyword #:album)
  (title  #:init-keyword #:title))

(define-class <sp-prev> (<instance>))
(define-class <sp-play-pause> (<instance>)
  (playback-status #:init-keyword #:playback-status))

(define-class <sp-next> (<instance>))

(define-method (process (obj <spotify>) (diff <number>))
  (let ((data (slot-ref obj 'data)))
    (if (not data)
        (begin
          (slot-set! obj 'running? #f)
          (list))
        (begin
          (slot-set! obj 'running? #t)
          (list (make <sp-prev>
                  #:id 'sp-prev
                  #:obj obj)
                (make <sp-play-pause>
                  #:id 'play-pause
                  #:obj obj
                  #:playback-status (get data 'playback-status))
                (make <sp-next>
                  #:id 'sp-next
                  #:obj obj)
                (make <sp-info>
                  #:id 'info
                  #:obj obj
                  #:artist (get data 'artist)
                  #:album (get data 'album)
                  #:title (get data 'title)))))))

(define-method (fmt (obj <sp-prev>) (clicked? <boolean>))
  (when clicked?
    (unclick obj)
    (run-spotify-cmd! "Previous"))
  "⏮")

(define-method (fmt (obj <sp-play-pause>) (clicked? <boolean>))
  (let ((status (slot-ref obj 'playback-status)))
    (when clicked?
      (unclick obj)
      (run-spotify-cmd! (case status
                          ((paused) "Play")
                          ((playing) "Pause")))
      (set! status
        (case status
          ((paused) 'playing)
          ((playing) 'paused)))
      (slot-set! obj 'playback-status status))
    (case status
      ((paused) "<span foreground=\"#DA1000\">⏵</span>")
      ((playing) "⏸"))))

(define-method (fmt (obj <sp-next>) (clicked? <boolean>))
  (when clicked?
    (unclick obj)
    (run-spotify-cmd! "Next"))
  "⏭")

(define-method (fmt (obj <sp-info>) (clicked? <boolean>))
  (values
   (if clicked?
       (format #f "~a - ~a - ~a"
               (slot-ref obj 'artist)
               (slot-ref obj 'album)
               (slot-ref obj 'title))
       (format #f "~a" (slot-ref obj 'title)))
   #:markup #f))
