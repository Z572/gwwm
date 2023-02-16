(define-module (gwwm pointer)
  #:use-module (oop goops)
  #:use-module (gwwm listener)
  #:use-module (wlroots types input-device)
  #:use-module (wlroots types pointer)
  #:use-module (wlroots backend libinput)
  #:use-module (bytestructure-class)
  #:use-module (libinput)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-189)
  #:use-module (ice-9 q)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (pointer-list
            <gwwm-pointer>
            pointer-disable-while-typing?
            .device))

(define (get-disable-while-typing? o)
  (let ((device (slot-ref o 'device)))
    (if (wlr-input-device-is-libinput device)
        (let* ((libinput-device (wlr-libinput-get-device-handle device)))
          (if (libinput-device-config-dwt-is-available libinput-device)
              (right
               (not (zero? (%libinput-config-dwt-state-enum->number
                            (libinput-device-config-dwt-get-enabled libinput-device)))))
              (left "disable-while-typing? is unavailable")))
        (left "device is not libinput"))))
(define (set-disable-while-typing? o v)
  (let ((device (slot-ref o 'device)))
    (and-let* (((wlr-input-device-is-libinput device))
               (libinput-device (wlr-libinput-get-device-handle device))
               ((libinput-device-config-dwt-is-available libinput-device)))
      (libinput-device-config-dwt-set-enabled
       libinput-device
       (case v
         ((#t) 'LIBINPUT_CONFIG_DWT_ENABLED)
         ((#f) 'LIBINPUT_CONFIG_DWT_DISABLED)
         ((reset) (libinput-device-config-dwt-get-default-enabled libinput-device)))))))

(define-class <gwwm-pointer> ()
  (device #:init-keyword #:device #:accessor .device)
  (disable-while-typing?
   #:accessor pointer-disable-while-typing?
   #:allocation #:virtual
   #:slot-ref get-disable-while-typing?
   #:slot-set! set-disable-while-typing?)
  #:metaclass <redefinable-class>)

(define-once %pointers (make-q))
(define (pointer-list)
  (car %pointers))
(define (add-pointer o)
  (q-push! %pointers o))
(define (remove-pointer o)
  (q-remove! %pointers o))
(define-method (initialize (o <gwwm-pointer>) args)
  (let ((obj (next-method)))
    (add-pointer obj)
    (add-listen (.device obj) 'destroy
                (lambda (listener data)
                  (remove-pointer obj)))))
