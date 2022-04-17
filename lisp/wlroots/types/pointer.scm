(define-module (wlroots types pointer)
  #:use-module (wlroots types)
  #:use-module (wlroots types input-device)
  ;; #:use-module ((system foreign) #:select )
  #:use-module (bytestructures guile)
  #:export (wrap-wlr-event-pointer-motion
            unwrap-wlr-event-pointer-motion
            %wlr-event-pointer-motion-struct)
  )
(define-wlr-types-class wlr-event-pointer-motion)
(define %wlr-event-pointer-motion-struct
  (bs:struct `((device ,(bs:pointer %wlr-input-device-struct))
               (time-msec ,uint32)
               (delta-x ,double)
               (delta-y ,double)
               (unaccel-dx ,double)
               (unaccel-dy ,double))))
