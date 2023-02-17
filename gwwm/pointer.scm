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
            pointer-left-handed?
            pointer-natural-scroll?
            .device))

(define (has-natural-scroll? device)
  (truth->either
   (libinput-device-config-scroll-has-natural-scroll device)
   "device is not has natural-scroll"))

(define (left-handed-is-available? device)
  (truth->either
   (libinput-device-config-left-handed-is-available device)
   "left-handed is unavailable"))
(define (dwt-is-available? libinput-device)
  (truth->either
   (libinput-device-config-dwt-is-available libinput-device)
   "disable-while-typing? is unavailable"))

(define-class <gwwm-pointer> ()
  (device #:init-keyword #:device #:accessor .device)
  (disable-while-typing?
   #:accessor pointer-disable-while-typing?
   #:allocation #:virtual
   #:slot-ref (lambda (o)
                (either-let* ((libinput-device (get-libinput-device o))
                              ((dwt-is-available? libinput-device)))
                  (not (zero? (%libinput-config-dwt-state-enum->number
                               (libinput-device-config-dwt-get-enabled libinput-device))))))
   #:slot-set! (lambda (o v)
                 (either-let* ((v (or (and (either? v) v) (right v)))
                               (libinput-device (get-libinput-device o))
                               ((dwt-is-available? libinput-device)))
                   (libinput-device-config-dwt-set-enabled
                    libinput-device
                    (case v
                      ((#t) 'LIBINPUT_CONFIG_DWT_ENABLED)
                      ((#f) 'LIBINPUT_CONFIG_DWT_DISABLED)
                      ((reset) (libinput-device-config-dwt-get-default-enabled libinput-device)))))))
  (left-handed?
   #:accessor pointer-left-handed?
   #:allocation #:virtual
   #:slot-ref (lambda (o)
                (either-let* ((device (get-libinput-device o))
                              ((left-handed-is-available? device)))
                  (not (zero? (libinput-device-config-left-handed-get device)))))
   #:slot-set! (lambda (o v)
                 (either-let* ((v (or (and (either? v) v) (right v)))
                               (device (get-libinput-device o))
                               ((left-handed-is-available? device)))
                   (libinput-device-config-left-handed-set
                    device
                    (case v
                      ((#t) 1)
                      ((#f) 0)
                      ((reset) (libinput-device-config-left-handed-get-default device)))))))
  (natural-scroll?
   #:accessor pointer-natural-scroll?
   #:allocation #:virtual
   #:slot-ref (lambda (o)
                (either-let* ((device (get-libinput-device o))
                              ((has-natural-scroll? device)))
                  (not (zero? (libinput-device-config-scroll-get-natural-scroll-enabled
                               device)))))
   #:slot-set! (lambda (o v)
                 (either-let* ((v (or (and (either? v) v) (right v)))
                               (device (get-libinput-device o))
                               ((has-natural-scroll? device)))
                   (libinput-device-config-scroll-set-natural-scroll-enabled
                    device
                    (case v
                      ((#t) 1)
                      ((#f) 0)
                      ((reset)
                       (libinput-device-config-scroll-get-default-natural-scroll-enabled
                        device)))))))
  #:metaclass <redefinable-class>)

(define-method (get-libinput-device (p <gwwm-pointer>))
  (let ((device (slot-ref p 'device)))
    (if (wlr-input-device-is-libinput device)
        (right (wlr-libinput-get-device-handle device))
        (left "device is not libinput"))))

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
