(define-module (gwwm commands)
  #:use-module (wlroots backend session)
  #:use-module (gwwm client)
  #:export (spawn chvt togglefullscreen))

(define* (togglefullscreen #:optional (client (current-client)))
  (client-toggle-fullscreen client))

(define (spawn program . args)
  (when (= (primitive-fork) 0)
    (dup2 (port->fdes (current-error-port))
          (port->fdes (current-output-port)))
    (setsid)
    (apply execlp program program args)))

(define (chvt num)
  (wlr-session-change-vt
   (wlr-backend-get-session ((@@ (gwwm) gwwm-backend)))
   num))
