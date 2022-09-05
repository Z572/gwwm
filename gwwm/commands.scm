(define-module (gwwm commands)
  #:use-module (wlroots backend session)
  #:use-module (wayland display)
  #:use-module (gwwm client)
  #:export (spawn
            chvt
            killclient
            togglefullscreen
            togglefloating
            gwwm-quit))

(define* (togglefullscreen #:optional (client (current-client)))
  (when client
    ((@@ (gwwm) %setfullscreen) client (not (client-fullscreen? client)))))

(define (togglefloating)
  ((@@ (gwwm) togglefloating)))
(define (spawn program . args)
  (when (= (primitive-fork) 0)
    (dup2 (port->fdes (current-error-port))
          (port->fdes (current-output-port)))
    (setsid)
    (apply execlp program program args)))

(define* (killclient #:optional (client (current-client)))
  (and=> client client-send-close))
(define (chvt num)
  (wlr-session-change-vt
   (wlr-backend-get-session ((@@ (gwwm) gwwm-backend)))
   num))

(define (gwwm-quit)
  (wl-display-terminate ((@@ (gwwm) gwwm-display))))
