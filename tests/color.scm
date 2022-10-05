(define-module (tests color)
  #:use-module (gwwm color)
  #:use-module (srfi srfi-64))

(test-group "make-color"
  (test-equal "number"
    (make-color 255 0 0 0)
    (make-color #xff000000))
  (test-equal "number 2"
    (make-color 255 0 0)
    (make-color #xff0000))
  (test-equal "string"
    (make-color 255 0 0 0)
    (make-color "#ff000000"))
  (test-equal "string 2"
    (make-color 255 0 0 0)
    (make-color "#ff0000"))

  (test-equal "short string"
    (make-color #xff000000)
    (make-color "#f000"))
  (test-equal "short string 2"
    (make-color #xff0000)
    (make-color "#f00"))
  (test-error "unmatch format string"
              #t
              (make-color "#f0000")))
