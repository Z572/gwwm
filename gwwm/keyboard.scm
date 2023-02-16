(define-module (gwwm keyboard)
  #:use-module (oop goops)
  #:use-module (wlroots types input-device)
  #:use-module (ice-9 q)
  #:use-module (gwwm listener)
  #:use-module (gwwm hooks)
  #:use-module (wlroots types keyboard)
  #:use-module (bytestructure-class)
  #:use-module (bytestructures guile)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (keyboard-list
            <gwwm-keyboard>
            keyboard-rate
            keyboard-delay
            keyboard-set-repeat-info
            .device))

(define-class <gwwm-keyboard> ()
  (device #:init-keyword #:device #:accessor .device)
  (rate #:allocation #:virtual
        #:slot-ref (lambda (o)
                     (let ((k (wlr-keyboard-from-input-device
                               (slot-ref o 'device))))
                       (bytestructure-ref
                        (get-bytestructure k)
                        'repeat-info
                        'rate)))
        #:slot-set! (lambda (o value)
                      (let ((k (wlr-keyboard-from-input-device
                                (slot-ref o 'device))))
                        (wlr-keyboard-set-repeat-info
                         k value (slot-ref o 'delay))))
        #:accessor keyboard-rate)
  (delay #:allocation #:virtual
         #:slot-ref (lambda (o)
                      (let ((k (wlr-keyboard-from-input-device
                                (slot-ref o 'device))))
                        (bytestructure-ref
                         (get-bytestructure k)
                         'repeat-info
                         'delay)))
         #:slot-set! (lambda (o value)
                       (let ((k (wlr-keyboard-from-input-device
                                 (slot-ref o 'device))))
                         (wlr-keyboard-set-repeat-info
                          k (slot-ref o 'rate) value)))
         #:accessor keyboard-delay)
  #:metaclass <redefinable-class>)

(define-method (keyboard-set-repeat-info (k <gwwm-keyboard>) rate delay)
  (wlr-keyboard-set-repeat-info
   (wlr-keyboard-from-input-device (.device k)) rate delay))

(define-once %keyboards (make-q))

(define (add-keyboard keyboard)
  (q-push! %keyboards keyboard))

(define (remove-keyboard keyboard)
  (q-remove! %keyboards keyboard))

(define (keyboard-list)
  (car %keyboards))

(define-method (write (o <gwwm-keyboard>) port)
  (format port "#<<gwwm-keyboard> ~S>"
          (.name (.device o))))

(define-method (initialize (o <gwwm-keyboard>) args)
  (let ((obj (next-method)))
    (add-keyboard obj)
    (add-listen (.device obj) 'destroy
                (lambda (listener data)
                  (run-hook cleanup-keyboard-hook obj)
                  (remove-keyboard obj)))))
