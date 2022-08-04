(define-module (gwwm config)
  #:use-module (oop goops)
  #:export (gwwm
            config-borderpx
            config-sloppyfocus?))

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
