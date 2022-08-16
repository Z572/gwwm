(define-module (gwwm keymap)
  #:use-module (srfi srfi-1)
  #:use-module (wlroots types keyboard)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (gwwm utils)
  #:export (global-keymap .modify-keys .key .keys
                          keymap-set
                          kbd* global-keymap
                          keymap-global-set)
  #:export-syntax (kbd))

(define SHIFT WLR_MODIFIER_SHIFT )
(define CAPS  WLR_MODIFIER_CAPS )
(define CTRL  WLR_MODIFIER_CTRL )
(define ALT   WLR_MODIFIER_ALT  )
(define MOD2  WLR_MODIFIER_MOD2 )
(define MOD3  WLR_MODIFIER_MOD3 )
(define LOGO  WLR_MODIFIER_LOGO )
(define MOD5  WLR_MODIFIER_MOD5 )

(define-class <key> ()
  (modify-keys #:init-value 0
               #:init-keyword #:m
               #:accessor .modify-keys)
  (key #:init-value 0
       #:init-keyword #:k
       #:accessor .key))

(define-class <keymap> ()
  (keys #:init-value '() #:accessor .keys))

(define-method (equal? (key1 <key>) (key2 <key>))
  (and (equal? (.modify-keys key1) (.modify-keys key2))
       (equal? (.key key1) (.key key2))))

(define-method (write (key <key>) port)
  (format port "#<<key> m:~a k:~a>"
          (.modify-keys key )
          (.key key)))

(define %modify-keys
  (let ((t (make-hash-table)))
    (hash-set! t 'C CTRL)
    (hash-set! t 's LOGO)
    (hash-set! t 'S SHIFT)
    (hash-set! t 'M ALT)
    t))

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
  (or (and=> (find-key-l key keymap)
             (lambda (l) (set-cdr! l (list definition)) #t))
      (set! (.keys keymap) (cons (list key definition) (.keys keymap)))))

(define-method (keymap-set (keymap <keymap>)
                           (key <key>)
                           (f <boolean>) )
  (and=> (and (not f)
              (find-key-l key keymap))
         (lambda (a) (delete! a (.key keymap)))))

(define-method (find-key-l (key <key>) (keymap <keymap>))
  (find (match-lambda
          ((k _)
           (equal? k key)))
        (.keys keymap)))

(define (find-key-command key keymap)
  (and=> (find-key-l key keymap)
         second))

(define global-keymap
  (make-parameter (make <keymap>)))

(define (keymap-global-set key command)
  (keymap-set (global-keymap) key command))
