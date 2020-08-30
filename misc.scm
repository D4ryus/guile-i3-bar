(define-module (guile-i3-bar misc)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-18)
  #:export (accumulate
            accumulate-alist
            call-with-mutex
            current-tick
            colorize
            difference
            format-bar
            format-size
            get
            i3-block
            match-substrings
            parse-string
            run-cmd!
            string->lispified-symbol))

(define (string->lispified-symbol str)
  (let ((res (list))
        (first? #t))
    (string-for-each
     (lambda (c) (cond
                  ((char-upper-case? c)
                   (unless first?
                     (set! res (cons #\- res)))
                   (set! res
                     (cons (char-downcase c)
                           res)))
                  ((or (char=? c #\_)
                       (char=? c #\())
                   (set! res (cons #\- res)))
                  ((char=? c #\)))
                  (#t (set! res (cons c res))))
             (set! first? #f))
     str)
    (string->symbol (apply string (reverse res)))))

(define (get alist . keys)
  (let ((cur (assoc (car keys) alist)))
    (if (not cur)
        #f
        (begin
          (set! cur (cdr cur))
          (if (null? (cdr keys))
              cur
              (apply get cur (cdr keys)))))))

(define (match-substrings match)
  (let loop ((nr 1)
             (substrings (list)))
    (if (>= nr (match:count match))
        (reverse substrings)
        (loop (+ nr 1)
              (cons (match:substring match nr)
                    substrings)))))

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (accumulate op
                  (op init (car sequence))
                  (cdr sequence))))

(define (difference x y)
  (cond
   ((and (null? x) (null? y))
    '())
   ((null? x)
    (difference y y))
   ((null? y)
    (difference x x))
   ((and (pair? x)
         (or (pair? y)
             (error "x is pair while y is:" y)))
    (cons (difference (car x) (car y))
          (difference (cdr x) (cdr y))))
   ((and (symbol? x)
         (or (symbol? y)
             (error "x is symbol while y is:" y)))
    (if (not (equal? x y))
        (error "symbols x and y differ:" x y)
        x))
   ((and (number? x) (number? y))
    (- x y))
   (#t (error "expected x and y to be a number:" x y))))

(define (accumulate-alist op init alist keys)
  (accumulate (lambda (acc x)
                (unless (pair? x)
                  (error "element in alist is not a pair:" x))
                (op acc
                    (if (find (lambda (el) (equal? (car x) el))
                              keys)
                        (cdr x)
                        0)))
              init
              alist))

(define (current-tick)
  (let* ((time (gettimeofday))
         (sec (car time))
         (micro-sec (cdr time)))
    (floor (+ (* sec 1000)
              (/ micro-sec 1000)))))

(define (colorize color text)
  (format #f "<span foreground=\"~a\">~a</span>"
          color text))

(define format-size
  (let ((xb (ash 1 53))  ;; 8xb
        (tb (ash 1 43))  ;; 8tb
        (gb (ash 1 33))  ;; 8gb
        (mb (ash 1 23))  ;; 8mb
        (kb (ash 1 13))) ;; 8kb
    (lambda (size)
      (colorize "#FFFFFF"
                (cond
                 ((> size xb) (format #f "~4dpb" (ash size -50)))
                 ((> size tb) (format #f "~4dtb" (ash size -40)))
                 ((> size gb) (format #f "~4dgb" (ash size -30)))
                 ((> size mb) (format #f "~4dmb" (ash size -20)))
                 ((> size kb) (format #f "~4dkb" (ash size -10)))
                 (#t          (format #f "~4db " size)))))))

(define (format-bar value max)
  (let ((percent (if (= max 0)
                     0
                     (* (/ value max) 100))))
    (apply colorize
           (cond
            ((> percent 90) (list "#FF0000" "█"))
            ((> percent 80) (list "#FFAA00" "▇"))
            ((> percent 70) (list "#FFFF00" "▆"))
            ((> percent 60) (list "#CCCC00" "▅"))
            ((> percent 50) (list "#AAAA00" "▄"))
            ((> percent 40) (list "#AAFF00" "▃"))
            ((> percent 25) (list "#00AA00" "▂"))
            ((> percent  0) (list "#00FF00" "▁"))
            (else           (list "#FFFFFF" "▁"))))))

(define* (i3-block full-text #:key short-text color background
                   border (border-top 0) (border-right 0) (border-bottom 1) (border-left 0)
                   min-width align urgent? name instance
                   (separator? #f) separator-block-width
                   (markup "pango"))
  (scm->json-string
   (filter (compose not unspecified?)
           (list (cons "full_text" full-text)
                 (when short-text
                   (cons "short_text" short-text))
                 (when color
                   (cons "color" color))
                 (when background
                   (cons "background" background))
                 (when border
                   (cons "border" border))
                 (when border-top
                   (cons "border_top" border-top))
                 (when border-right
                   (cons "border_right" border-right))
                 (when border-bottom
                   (cons "border_bottom" border-bottom))
                 (when border-left
                   (cons "border_left" border-left))
                 (when min-width
                   (cons "min_width" min-width))
                 (when align
                   (cons "align" align))
                 (cons "urgent" urgent?)
                 (when name
                   (cons "name" name))
                 (when instance
                   (cons "instance" instance))
                 (cons "separator" separator?)
                 (when separator-block-width
                   (cons "separator_block_width" separator-block-width))
                 (when markup
                   (cons "markup" markup))))))

(define (run-cmd! cmd)
  (let* ((pipe (open-input-pipe cmd)))
    (let loop ((line (read-line pipe))
               (result (list)))
      (if (eof-object? line)
          (begin
            (close-pipe pipe)
            (reverse result))
          (loop (read-line pipe)
                (cons line result))))))

(define (parse-string str pos)
  (let ((delim (string-ref str pos))
        (len (string-length str)))
    (let loop ((from (+ 1 pos))
               (result (list)))
      (if (>= from len)
          #f
          (let ((end (string-index str delim from)))
            (if (not end)
                #f
                (if (not (eqv? #\\ (string-ref str (- end 1))))
                    (apply string-append
                           (reverse (cons (substring str from end) result)))
                    (let ((part (string-copy (substring str from end))))
                      (string-set! part (- (string-length part) 1) delim)
                      (loop (+ 1 end)
                            (cons part result))))))))))


(define (call-with-mutex mutex thunk)
  (dynamic-wind
    (lambda () (mutex-lock! mutex))
    thunk
    (lambda () (mutex-unlock! mutex))))
