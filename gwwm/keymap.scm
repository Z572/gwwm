(define-module (gwwm keymap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (gwwm utils)
  #:export (make-keymap
            keymap-set
            .modify-keys
            .key
            .keys
            kbd*
            define-modify-key
            parse-modify-key
            <keymap>
            <key>)
  #:export-syntax (kbd))


(define-class <key> ()
  (modify-keys #:init-value '()
               #:init-keyword #:m
               #:accessor .modify-keys)
  (key #:init-value 0
       #:init-keyword #:k
       #:accessor .key))

(define-class <keymap> ()
  (keys #:init-value '() #:accessor .keys))

(define (make-keymap)
  (make <keymap>))
(define-method (equal? (key1 <key>) (key2 <key>))
  (and (equal? (.modify-keys key1) (.modify-keys key2))
       (equal? (.key key1) (.key key2))))

(define-method (write (key <key>) port)
  (format port "#<<key> m:~a k:~a>"
          (.modify-keys key )
          (.key key)))

(define-once %modify-keys
  (make-hash-table))

(define (define-modify-key symbol value)
  (hash-set! %modify-keys symbol value))

(define* (parse-modify-key m #:optional (error-when-no-found? #t))
  (let ((o (hash-ref %modify-keys m)))
    (cond ((symbol? o) (parse-modify-key o))
          ((number? o) o)
          (else (if error-when-no-found?
                    (scm-error 'wrong-type-arg 'parse-modify-key "unknow mk!: ~S"
                               (list o)
                               (list o))
                    #f)))))

(define-syntax-rule (kbd . rest)
  (apply kbd* 'rest))

(define kbd*
  (case-lambda
    ((kl)
     (let ((k mk (car+cdr (reverse kl))))
       (list (make <key> #:m mk #:k (->symbol k)))))
    ((kl . rest)
     (append (kbd* kl) (apply kbd* rest)))))

(define-method (keymap-set o (ks <list>) d)
  (if (> (length ks) 1)
      (warn "for now, gwwm not support multi key define, ignore others."))
  (keymap-set o (car ks) d))

(define-method (keymap-set (keymap <keymap>)
                           (key <key>)
                           (definition <procedure>))
  (->bool (or (and=> (find-key-l key keymap)
                     (lambda (l) (set-cdr! l (list definition)) #t))
              (set! (.keys keymap)
                    (cons (list key definition)
                          (.keys keymap))))))

(define-method (keymap-set (keymap <keymap>)
                           (key <key>)
                           (f <boolean>) )
  (and=> (and (not f)
              (find-key-l key keymap))
         (lambda (a)
           (->bool (set! (.keys keymap)
                         (delete a (.keys keymap)))))))

(define-method (find-key-l (key <key>) (keymap <keymap>))
  (find (match-lambda
          ((k _)
           (equal? k key)))
        (.keys keymap)))

(define (find-key-command key keymap)
  (and=> (find-key-l key keymap)
         second))
