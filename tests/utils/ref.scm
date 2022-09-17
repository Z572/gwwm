(define-module (tests utils ref)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:use-module (gwwm utils ref))

(test-group "ref"
  (test-equal "ref: vector"
    1
    (ref #(1 2) 0))
  (test-equal "ref: string"
    #\r
    (ref "ref" 0))
  (test-equal "ref: list"
    1
    (ref (list 1) 0))
  (test-equal "ref: pair"
    'car
    (ref (cons 'car 'cdr) 0))
  (test-equal "ref: pair: car"
    'car
    (ref (cons 'car 'cdr) 'car))
  (test-equal "ref: pair: cdr"
    'cdr
    (ref (cons 'car 'cdr) 'cdr)))
