(define-module (wlroots util log)
  #:use-module (wlroots utils)
  #:use-module (system foreign)
  #:export (wlr-log-init wlr-log-code->name wlr-log-name->code))

(define-enumeration wlr-log-code->name wlr-log-name->code
  (silent 0)
  (error 1)
  (info 2)
  (debug 3)
  (importance-last 4))

(define-wlr-procedure (wlr-log-init verbosity #:optional (callback #f))
  (void "wlr_log_init" (list int '*))
  (% (wlr-log-name->code verbosity)
     (if callback (procedure->pointer
                   void
                   (lambda (a b c) (callback (wlr-log-code->name a) (pointer->string b) c))
                   (list int '* '*))
         %null-pointer)))
