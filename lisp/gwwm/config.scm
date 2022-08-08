(define-module (gwwm config)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export (gwwm
            config-borderpx
            config-sloppyfocus?
            config-xkb-rules
            config-lockfullscreen?
            load-init-file
            get-xdg-config-home
            init-file
            make-xkb-rules))

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

(define-class <xkb-rules> ()
  (rules #:init-keyword #:rules #:getter xkb-rults-names-rules #:init-value "")
  (model #:init-keyword #:model #:getter xkb-rults-names-model)
  (layout #:init-keyword #:layout #:getter xkb-rults-names-layout)
  (variant #:init-keyword #:variant #:getter xkb-rults-names-variant)
  (options #:init-keyword #:options #:getter xkb-rults-names-options))

(define* (make-xkb-rules
          #:optional
          (layout "")
          (variant "")
          #:key (model "")
          (options '()))
  (make <xkb-rules>
    #:layout layout
    #:variant variant
    #:model model
    #:options (string-join options ",")))

(define-class <gwwm-config> ()
  (borderpx #:init-value 1 #:init-keyword #:borderpx #:accessor config-borderpx)
  (sloppyfocus? #:init-value #t #:init-keyword #:sloppyfocus? #:accessor config-sloppyfocus?)
  (rootcolor)
  (tags)
  (rules)
  (xkb-rules #:init-value (make <xkb-rules>) #:init-keyword #:xkb-rules #:accessor config-xkb-rules )
  (lockfullscreen? #:init-value #t #:init-keyword #:lockfullscreen? #:accessor config-lockfullscreen? ))

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

(define (g-config) ((@@ (gwwm) gwwm-config)) )
(define-public (gwwm-borderpx)
  (pk 'abc (g-config))
  (config-borderpx (g-config)))
(define-public (gwwm-sloppyfocus?)
  (pk 'abc (g-config))
  (config-sloppyfocus? (g-config)))
