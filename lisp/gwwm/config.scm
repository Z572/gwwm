(define-module (gwwm config)
  #:use-module (oop goops)
  #:export (gwwm
            config-borderpx
            config-sloppyfocus?
            load-init-file
            get-xdg-config-home
            init-file))

(define* (getenv* nam #:optional fallback)
  (or (getenv nam) fallback))

(define (get-xdg-config-home)
  (getenv* "XDG_CONFIG_HOME"
           (string-append (getenv "HOME") "/.config")))

(define (init-file)
  (string-append
   (get-xdg-config-home)
   "/gwwm/init.scm"))

(define (load-init-file)
  (let ((init-file (init-file)))

    (if (file-exists? init-file)
        (save-module-excursion
         (lambda ()
           (primitive-load
            init-file)))
        (warn (string-append  "initfile not found:" init-file )))))

(define-class <gwwm-config> ()
  (borderpx #:init-value 1 #:init-keyword #:borderpx #:accessor config-borderpx)
  (sloppyfocus? #:init-value #t #:init-keyword #:sloppyfocus? #:accessor config-sloppyfocus?)
  (rootcolor)
  (tags)
  (rules)
  (xkb-rules))

(define-syntax-rule (gwwm (init value) ...)
  (let ((init-keywords
         (map slot-definition-init-keyword (class-slots <gwwm-config>))))
    (if (and-map (lambda (m)
                   (or (member  m init-keywords)
                       (error "unknow init-keyword!: ~s"
                              (list (keyword->symbol m)))))
                 (list (symbol->keyword 'init) ...))
        (let* ((init value) ...)

          (apply make <gwwm-config> (append (list (symbol->keyword 'init) value) ...))))))
