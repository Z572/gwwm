;;; Copyright (C) 2022 Zheng Junjie <873216071@qq.com>
;;; simple srfi-123, use goops, not sure bug.

(define-module (gwwm utils ref)
  #:use-module (oop goops)
  #:use-module (srfi srfi-17)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-111)
  #:export (ref ref* ~ ))

(define-accessor ref)
(define-accessor ref*)

(define ~ ref*)
(define set (setter ref))
(define set* (setter ref*))

(define-method (ref o field)
  (cond ((record? o)
         ((record-accessor
           (record-type-descriptor o)
           field)
          o))))

(define-method (set o field value)
  (cond ((record? o)
         ((record-modifier
           (record-type-descriptor o)
           field)
          o value))))

(define-method (ref (o <vector>) field)
  (vector-ref o field))

(define-method (set (o <vector>) field obj)
  (vector-set! o field obj))

(define-method (ref (o <uvec>) field)
  (s16vector-ref o field))

(define-method (ref (o <bytevector>) field)
  (bytevector-u8-ref o field))

(define-method (set (o <bytevector>) field value)
  (bytevector-u8-set! o field value))

(define-method (ref* o field)
  (ref o field))
(define-method (ref* o field . field+)
  (apply ref* (ref o field) field+))

(define-method (ref (o <string>) field)
  (string-ref o field))

(define-method (set (o <string>) field value)
  (string-set! o field value))

(define-method (ref (o <hashtable>) field default)
  (hash-ref o field default))

(define-method (set (o <hashtable>) field value)
  (hash-set! o field value))

(define-method (ref (o <list>) field)
  (list-ref o field))

(define-method (set (o <list>) field value)
  (list-set! o field value))

(define-method (ref (o <pair>) (field <symbol>))
  (case field
    ((car) (car o))
    ((cdr) (cdr o))))

(define-method (set (o <pair>) (field <symbol>) value)
  (case field
    ((car) (set-car! o value))
    ((cdr) (set-cdr! o value))))

(define-method (ref (o <pair>) field)
  (list-ref o field))

(define-method (set (o <pair>) field value)
  (list-set! o field value))

(define-method (ref (o <object>) (field <symbol>))
  (let* ((c (class-of o))
         (def (class-slot-definition c field))
         (getter (or (slot-definition-getter def)
                     (slot-definition-accessor def)
                     (lambda (o) (slot-ref o field)))))
    (getter o)))

(define-method (ref (o <character-set>) field)
  (char-set-ref o field))

(define-method (set (o <object>) (field <symbol>) item)
  (let* ((c (class-of o))
         (def (class-slot-definition c field))
         (s (or (slot-definition-setter def)
                (and=> (slot-definition-accessor def)
                       (lambda (sett)
                         (lambda (o item)
                           ((setter sett) o item))))
                (lambda (o sl) (slot-set! o field sl)))))
    (s o item)))
