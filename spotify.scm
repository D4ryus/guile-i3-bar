(define-module (guile-i3-bar spotify)
  #:export (<spotify>)
  #:use-module (guile-i3-bar classes)
  #:use-module (guile-i3-bar misc)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (oop goops))

(define (read-spotify!)
  (let* ((pipe (open-input-pipe
                (string-join (list "qdbus"
                                   "org.mpris.MediaPlayer2.spotify"
                                   "/org/mpris/MediaPlayer2"
                                   "org.mpris.MediaPlayer2.Player.Metadata"))))
         (rgx `((artist album title)
                ;; We are looking for lines matching:
                ;; xesam:album: Some album name
                ;; xesam:artist: Some artist
                ;; xesam:title: Some great Title
                (,(make-regexp "^xesam:artist: (.*)$")
                 ,(make-regexp "^xesam:album: (.*)$")
                 ,(make-regexp "^xesam:title: (.*)$"))))
         (result (let loop ((line (read-line pipe))
                            (res (list)))
                   (if (eof-object? line)
                       (reverse res)
                       (let ((match #f))
                         (apply map
                                (lambda (type rgx)
                                  (let ((m (regexp-exec rgx line)))
                                    (when m
                                      (set! match
                                        (cons type (match:substring m 1))))))
                                rgx)
                         (loop (read-line pipe)
                               (if match
                                   (cons match res)
                                   res)))))))
    (close-pipe pipe)
    result))

(define-class <spotify> (<obj>)
  (running? #:init-value #f)
  artist
  album
  title)

(define-method (fetch (obj <spotify>))
  (read-spotify!))

(define-method (process (obj <spotify>) (diff <number>))
  (let ((data (slot-ref obj 'data)))
    (if (null? data)
        (slot-set! obj 'running? #f)
        (begin
          (slot-set! obj 'running? #t)
          (slot-set! obj 'artist (get data 'artist))
          (slot-set! obj 'album (get data 'album))
          (slot-set! obj 'title (get data 'title))))
    (list)))

(define-method (fmt (obj <spotify>) (clicked? <boolean>))
  (if (not (slot-ref obj 'running?))
      #f
      (values
       (if clicked?
           (format #f "~a - ~a - ~a"
                   (slot-ref obj 'artist)
                   (slot-ref obj 'album)
                   (slot-ref obj 'title))
           (format #f "~a" (slot-ref obj 'title)))
       #:markup #f)))
