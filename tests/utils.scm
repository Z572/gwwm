(define-module (tests utils)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:use-module (gwwm utils))

(test-group "environment variable"
  (test-equal "save-environment-excursion"
    (begin (setenv "gwwm_test_environment" "1")
           (save-environment-excursion (setenv "gwwm_test_environment" "0") )
           (getenv "gwwm_test_environment"))
    (getenv "gwwm_test_environment"))
  (test-equal "getenv*: no fallback"
    "1"
    (with-env (("GWWM_TEST_GETENV_WITHOUT_FALLBACK" "1"))
              (getenv* "GWWM_TEST_GETENV_WITHOUT_FALLBACK")))
  (test-equal "getenv*: have fallback"
    "fallback"
    (getenv* "GWWM_TEST_GETENV_WITH_FALLBACK" "fallback"))
  (let ((xdg-home "/xdg-config-home"))
    (test-equal "get-xdg-config-home"
      xdg-home
      (with-env (("XDG_CONFIG_HOME" xdg-home))
                (get-xdg-config-home)))))

(test-group "let-slots"
  (define-class <test-class> ()
    (one #:init-keyword #:one #:getter .one #:setter set-one!)
    (two #:init-keyword #:two #:getter .two #:setter set-two!))
  (let* ((n (random 200))
         (obj (make <test-class> #:one n)))
    (test-equal "let-slots: one"
      n (let-slots obj (one) one))
    (let ((n (random 200)))
      (test-equal "let-slots: change name"
        n (let-slots obj (one) (set! one n) one))
      (test-equal "let-slots: get change name"
        n (let-slots obj ((one one2)) one2)))


    (let ((n (random 200)))
      (test-equal "let-slots: get change name"
        n (begin (let-slots obj ((one one2)) (set! one2 n) one2)
                 (.one obj)))
      (test-error "let-slots: slot name is not variable"
                  #t
                  (let-slots obj ((one one2)) (set! one n) )))

    (test-error "let-slots: unbound "
                #t
                (let-slots obj (two) two))

    (define-method (set-one! (obj <test-class>) (var <boolean>) )
      (if var
          (next-method)
          (error "no!")))
    (test-error "let-slots: care method" #t (let-slots obj (one) (set! one #f)))))
