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
