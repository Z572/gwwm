(define-module (wlroots types pointer)
  #:use-module (wlroots types)
  #:use-module (wlroots types input-device)
  ;; #:use-module ((system foreign) #:select )
  #:use-module (bytestructures guile)
  #:export (wrap-wlr-event-pointer-motion
            unwrap-wlr-event-pointer-motion
            wrap-wlr-event-pointer-axis
            unwrap-wlr-event-pointer-axis
            %wlr-event-pointer-motion-struct
            %wlr-event-pointer-axis-struct))
(define-wlr-types-class wlr-event-pointer-motion)
(define %wlr-event-pointer-motion-struct
  (bs:struct `((device ,(bs:pointer %wlr-input-device-struct))
               (time-msec ,uint32)
               (delta-x ,double)
               (delta-y ,double)
               (unaccel-dx ,double)
               (unaccel-dy ,double))))

(define-wlr-types-class wlr-event-pointer-axis)
(define %wlr-event-pointer-axis-struct
  (bs:struct `((device ,(bs:pointer %wlr-input-device-struct))
               (time-msec ,uint32)
               (source ,int)
               (orientation ,int)
               (delta ,double)
               (delta-discrete ,int32))))
