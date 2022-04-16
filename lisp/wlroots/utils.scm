(define-module (wlroots utils)
  #:use-module (system foreign)
  #:use-module (wlroots config)
  #:export (ffi:int wlr->pointer wlr->procedure)
  #:export-syntax (define-wlr-procedure define-enumeration))
(define ffi:int int)
(define (wlr->pointer name)
  (dynamic-func name (dynamic-link %libwlroots)))
(define (wlr->procedure return name params)
  (let ((ptr (wlr->pointer name)))
    (pointer->procedure return ptr params)))
(define-syntax define-wlr-procedure
  (lambda (x)
    (syntax-case x ()
      ((_ (name args ...) (return-type cname arg-types) body ...)
       (with-syntax ((% (datum->syntax x '%)))
         #'(begin
             (define name
               (let ((% (wlr->procedure return-type cname arg-types)))
                 (lambda* (args ...)
                   body ...)))))))))

;;; copy define from (system vm dwarf) module
(define-syntax-rule (define-enumeration code->name name->code
                      (tag value) ...)
  (begin
    (define code->name
      (let ((table (make-hash-table)))
        (hashv-set! table value 'tag)
        ...
        (lambda (v)
          (hashv-ref table v v))))
    (define name->code
      (let ((table (make-hash-table)))
        (hashv-set! table 'tag value)
        ...
        (lambda (v)
          (hashv-ref table v v))))))
