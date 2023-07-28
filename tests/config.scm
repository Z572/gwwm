(define-module (tests config)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-189)
  #:use-module (gwwm utils srfi-215)
  #:use-module (gwwm config))

(current-log-callback
 (let ((p (current-error-port)))
   (lambda (msg)
     (let ((msg2 msg))
       (format p "[~a]| ~a | "
               (cdr (assq 'SEVERITY msg))
               (cdr (assq 'MESSAGE msg)))
       (set! msg2 (assoc-remove! (assoc-remove! msg2 'SEVERITY) 'MESSAGE))
       (for-each (lambda (a)
                   (display (car a) p)
                   (display ":" p)
                   (display (object->string(cdr a)) p)
                   (display " " p))
                 msg2)
       (newline p)))))
(define-config-option enable-debug? #t
  "a"
  #:conv (lambda (o) (->bool o)))

(test-group "get-option-value"
  (test-equal "no exists"
    (nothing)
    (get-option-value b))
  (test-equal "get"
    (just #t)
    (get-option-value enable-debug?))
  (test-equal "parameterize"
    #t
    (begin
      (parameterize ((enable-debug? 1))
        (enable-debug?)))))
