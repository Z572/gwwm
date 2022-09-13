(define-module (tests keymap)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:use-module (gwwm keymap))

(define SHIFT 1 )
(define CAPS  2)
(define CTRL  3)
(define ALT   4)
(define MOD2  5)
(define MOD3  6)
(define LOGO  7)
(define MOD5  8)

(define (%modify-keys)
  (@@ (gwwm keymap) %modify-keys))
(test-group "keymap"
  (test-assert "define-modify-key"
    (begin (define-modify-key 'C CTRL)
           (equal? (hash-ref (%modify-keys) 'C) CTRL)))
  (define-modify-key 's LOGO)
  (define-modify-key 'S SHIFT)
  (define-modify-key 'M ALT)
  (test-equal "kbd*: simple key"
    (kbd* `(C h))
    (list (make <key> #:m `(C) #:k 'h)))
  (test-equal "kbd*: multi key"
    (kbd* `(C h) `(C f))
    (list (make <key> #:m `(C) #:k 'h)
          (make <key> #:m `(C) #:k 'f)))
  (test-equal "kbd: simple key"
    (kbd (C h))
    (kbd* `(C h)))
  (test-equal "kbd: multi key"
    (kbd (C h) (C x))
    (kbd* `(C h) `(C x))))
