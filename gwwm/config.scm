(define-module (gwwm config)
  #:use-module (xkbcommon xkbcommon)
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
            config-cursor-normal-image
            config-repeat-rate
            config-enable-xwayland?
            load-init-file
            init-file
            make-xkb-rules
            gwwm-borderpx
            gwwm-default-bordercolor
            gwwm-sloppyfocus?
            gwwm-default-focuscolor
            g-config))

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

(define* (make-xkb-rules
          #:optional
          (layout #f)
          (variant #f)
          (rules #f)
          #:key
          (model "")
          (options '()))
  (make <xkb-rule-names>
    #:model model
    #:layout layout
    #:variant variant
    #:rules rules
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
  (xkb-rules #:init-value (make-xkb-rules) #:init-keyword #:xkb-rules #:accessor config-xkb-rules )
  (lockfullscreen? #:init-value #t #:init-keyword #:lockfullscreen? #:accessor config-lockfullscreen? )
  (bordercolor #:init-value gwwm-default-bordercolor #:init-keyword
               #:bordercolor #:accessor config-bordercolor )
  (focuscolor #:init-value gwwm-default-focuscolor #:init-keyword
              #:focuscolor #:accessor config-focuscolor)
  (repeat-rate #:init-value 25
               #:init-keyword #:repeat-rate
               #:accessor config-repeat-rate)
  (enable-xwayland? #:init-value #f
                    #:init-keyword #:enable-xwayland?
                    #:accessor config-enable-xwayland?))

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
(define (gwwm-borderpx)
  (config-borderpx (g-config)))
(define (gwwm-sloppyfocus?)
  (config-sloppyfocus? (g-config)))
