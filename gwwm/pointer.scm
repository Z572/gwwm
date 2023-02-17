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
  #:use-module (ice-9 format)
  #:use-module (ice-9 q)
  #:duplicates (merge-accessors merge-generics replace warn-override-core warn last)
  #:export (pointer-list
            <gwwm-pointer>
            pointer-disable-while-typing?
            pointer-left-handed?
            pointer-middle-emulation?
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

(define (middle-emulation-is-available? device)
  (truth->either
   (libinput-device-config-middle-emulation-is-available device)
   "middle emulation is unavailable"))

(define (s-ref-f has? get handle)
  (lambda (o)
    (either-let* ((device (get-libinput-device o))
                  ((has? device)))
      (handle (get device)))))

(define (s-set-f has? set handle)
  (lambda (o v)
    (either-let* ((v (or (and (either? v) v) (right v)))
                  (device (get-libinput-device o))
                  ((has? device)))
      (set
       device
       (handle v device)))))

(define-inlinable (not-zero? v)
  (not (zero? v)))
(define-class <gwwm-pointer> ()
  (device #:init-keyword #:device #:accessor .device)
  (disable-while-typing?
   #:accessor pointer-disable-while-typing?
   #:allocation #:virtual
   #:slot-ref
   (s-ref-f dwt-is-available?
            libinput-device-config-dwt-get-enabled
            (compose not-zero?
                     %libinput-config-dwt-state-enum->number))
   #:slot-set!
   (s-set-f dwt-is-available?
            libinput-device-config-dwt-set-enabled
            (lambda (v d)(case v
                           ((#t) 'LIBINPUT_CONFIG_DWT_ENABLED)
                           ((#f) 'LIBINPUT_CONFIG_DWT_DISABLED)
                           ((reset) (libinput-device-config-dwt-get-default-enabled d))))))
  (left-handed?
   #:accessor pointer-left-handed?
   #:allocation #:virtual
   #:slot-ref
   (s-ref-f left-handed-is-available?
            libinput-device-config-left-handed-get
            not-zero?)
   #:slot-set!
   (s-set-f left-handed-is-available?
            libinput-device-config-left-handed-set
            (lambda (v device)
              (case v
                ((#t) 1)
                ((#f) 0)
                ((reset) (libinput-device-config-left-handed-get-default device))))))
  (middle-emulation?
   #:accessor pointer-middle-emulation?
   #:allocation #:virtual
   #:slot-ref
   (s-ref-f
    middle-emulation-is-available?
    libinput-device-config-middle-emulation-get-enabled
    (compose not-zero?
             %libinput-config-middle-emulation-state-enum->number))
   #:slot-set!
   (s-set-f
    middle-emulation-is-available?
    libinput-device-config-middle-emulation-get-enabled
    (lambda (v d)
      (case v
        ((#t) 1)
        ((#f) 0)
        ((reset) libinput-device-config-middle-emulation-get-default-enabled d)) )))
  (natural-scroll?
   #:accessor pointer-natural-scroll?
   #:allocation #:virtual
   #:slot-ref
   (s-ref-f has-natural-scroll?
            libinput-device-config-scroll-get-natural-scroll-enabled
            not-zero?)
   #:slot-set!
   (s-set-f has-natural-scroll?
            libinput-device-config-scroll-set-natural-scroll-enabled
            (lambda (v d)
              (case v
                ((#t) 1)
                ((#f) 0)
                ((reset)
                 (libinput-device-config-scroll-get-default-natural-scroll-enabled
                  d))))))
  #:metaclass <redefinable-class>)

(define-method (write (o <gwwm-pointer>) port)
  (format port "#<~s ~S ~x>"
          (class-name (class-of o))
          (.name (.device o))
          (object-address o)))

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
