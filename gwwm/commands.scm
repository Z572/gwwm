(define-module (gwwm commands)
  #:use-module (wlroots backend session)
  #:use-module (wlroots types scene)
  #:use-module (wayland display)
  #:use-module (srfi srfi-26)
  #:use-module (gwwm monitor)
  #:use-module (gwwm layout)
  #:use-module (gwwm client)
  #:export (spawn
            chvt
            killclient
            togglefullscreen
            togglefloating
            focusclient
            focustop
            gwwm-quit
            arrange))

(define (arrange m)
  (for-each
   (lambda (c)
     (when (and (client-monitor c))
       (wlr-scene-node-set-enabled
        (client-scene c) (visibleon c (client-monitor c )))))
   (client-list))
  (and=> (layout-procedure
          (list-ref (monitor-layouts m) (monitor-sellt m))) (cut <> m))
  ((@@ (gwwm) %motionnotify) 0))

(define* (togglefullscreen #:optional (client (current-client)))
  (when client
    ((@@ (gwwm) %setfullscreen) client (not (client-fullscreen? client)))))

(define (togglefloating)
  ((@@ (gwwm) togglefloating)))

(define (focusclient client lift)
  ((@@ (gwwm) focusclient) client lift))

(define (focustop monitor)
  ((@@ (gwwm) focustop) monitor))

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
