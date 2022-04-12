(define-module (wlroots types)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  ;; #:export (<wlr-type> )
  #:export-syntax ( define-wlr-types-class
                    define-wlr-types-class-public))

(define-class <wlr-type> ()
  (pointer #:accessor .pointer #:init-keyword #:pointer))
(define-method (= (f <wlr-type>) (l <wlr-type>))
  (= (pointer-address (.pointer f))
     (pointer-address (.pointer l))))

(define-syntax define-wlr-types-class
  (lambda (x)
    (syntax-case x ()
      ((_ name)
       (let ((symbol (syntax->datum #'name))
             (identifier (cut datum->syntax #'name <>)))
         (with-syntax ((rtd (identifier (symbol-append '< symbol '>)))
                       (wrap (identifier (symbol-append 'wrap- symbol )))
                       (unwrap (identifier (symbol-append 'unwrap- symbol))))
           #`(begin
               (define-class rtd (<wlr-type>)
                 (pointer #:accessor .pointer #:init-keyword #:pointer))
               (define (wrap p)
                 (make rtd #:pointer p))
               (define (unwrap o)
                 (.pointer o)))))))))

(define-syntax define-wlr-types-class-public
  (lambda (x)
    (syntax-case x ()
      ((_ name)
       (let ((symbol (syntax->datum #'name))
             (identifier (cut datum->syntax #'name <>)))
         (with-syntax ((rtd (identifier (symbol-append '< symbol '>)))
                       (wrap (identifier (symbol-append 'wrap- symbol )))
                       (unwrap (identifier (symbol-append 'unwrap- symbol))))
           #`(begin
               (define-wlr-types-class name)
               (export wrap)
               (export unwrap))))))))
