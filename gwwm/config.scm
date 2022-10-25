(define-module (gwwm config)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (gwwm utils)
  #:use-module (gwwm i18n)
  #:use-module (gwwm color)
  #:export (gwwm
            config-borderpx
            config-sloppyfocus?
            config-xkb-rules
            config-lockfullscreen?
            config-bordercolor
            config-focuscolor
            config-fullscreenbg
            config-cursor-normal-image
            load-init-file
            init-file
            make-xkb-rules
            gwwm-default-bordercolor
            gwwm-default-focuscolor))

(define (init-file)
  "return init file."
  (string-append
   (get-xdg-config-home)
   "/gwwm/init.scm"))

(define (load-init-file)
  (let ((init-file (init-file)))
    (if (file-exists? init-file)
        (save-module-excursion
         (lambda ()
           (primitive-load init-file)))
        (begin (warn (string-append (G_ "initfile not found:") init-file ))
               (make <gwwm-config>)))))

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

(define gwwm-default-bordercolor (make-color 255/2 255/2 255/2 255))
(define gwwm-default-focuscolor (make-color 255 0 0 255))
(define-class <gwwm-config> ()
  (borderpx #:init-value 1 #:init-keyword #:borderpx #:accessor config-borderpx)
  (sloppyfocus? #:init-value #t #:init-keyword #:sloppyfocus? #:accessor config-sloppyfocus?)
  (rootcolor)
  (tags)
  (rules)
  (cursor-normal-image #:init-value "right_ptr"
                       #:init-keyword #:cursor-normal-image
                       #:accessor config-cursor-normal-image)
  (fullscreenbg #:init-value (make-color 0 0 0 1)
                #:init-keyword #:fullscreenbg
                #:accessor config-fullscreenbg)
  (xkb-rules #:init-value (make-xkb-rules) #:init-keyword #:xkb-rules #:accessor config-xkb-rules )
  (lockfullscreen? #:init-value #t #:init-keyword #:lockfullscreen? #:accessor config-lockfullscreen? )
  (bordercolor #:init-value gwwm-default-bordercolor #:init-keyword
               #:bordercolor #:accessor config-bordercolor )
  (focuscolor #:init-value gwwm-default-focuscolor #:init-keyword
              #:focuscolor #:accessor config-focuscolor))

(define-syntax-rule (gwwm (init value) ...)
  (let ((init-keywords
         (map slot-definition-init-keyword (class-slots <gwwm-config>))))
    (if (and-map (lambda (m)
                   (or (member  m init-keywords)
                       (error (G_ "unknow init-keyword!: ~s")
                              (list (keyword->symbol m)))))
                 (list (symbol->keyword 'init) ...))
        (let* ((init value) ...)

          (apply make <gwwm-config> (append (list (symbol->keyword 'init) value) ...))))))

(define (g-config) ((@@ (gwwm) gwwm-config)) )
(define-public (gwwm-borderpx)
  (config-borderpx (g-config)))
(define-public (gwwm-sloppyfocus?)
  (config-sloppyfocus? (g-config)))
