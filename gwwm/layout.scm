(define-module (gwwm layout)
  #:use-module (oop goops)
  #:export (<layout>
            layout-name
            layout-describe
            layout-procedure))
(define-class <layout> ()
  (symbol #:init-value #f
          #:init-keyword #:symbol
          #:getter layout-symbol)
  (describe #:init-value ""
            #:init-keyword #:describe
            #:getter layout-describe)
  (procedure #:init-value #f
             #:init-keyword #:procedure
             #:accessor layout-procedure))

(define-method (equal? (layout1 <layout>) (layout2 <layout>))
  (and (equal? (layout-symbol layout1) (layout-symbol layout2))
       (equal? (layout-describe layout1) (layout-describe layout2))
       (equal? (layout-procedure layout1) (layout-procedure layout2))))
