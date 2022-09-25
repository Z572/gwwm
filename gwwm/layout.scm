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
