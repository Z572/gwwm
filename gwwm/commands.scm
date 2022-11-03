(define-module (gwwm commands)
  #:use-module (wlroots backend session)
  #:use-module (wlroots types scene)
  #:use-module (wayland display)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 control)
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
            setlayout
            arrange
            tag))

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

(define* (tag int #:optional (client (current-client)))
  (when client
    (set! (client-tags client) int)
    (focusclient (focustop (current-monitor)) #t)
    (arrange (current-monitor))))

(define* (togglefullscreen #:optional (client (current-client)))
  (when client
    ((@@ (gwwm client) %setfullscreen) client (not (client-fullscreen? client)))))

(define* (setlayout layout #:optional (m (current-monitor)))
  (unless (equal? (list-ref (monitor-layouts m) (monitor-sellt m)) layout)
    (set! (monitor-sellt m) (logxor (monitor-sellt m)))
    (list-set! (monitor-layouts m) (monitor-sellt m) layout)
    (arrange m)))

(define* (togglefloating #:optional (client (current-client)))
  (when (and client (not (client-fullscreen? client)))
    ((@@ (gwwm) %setfloating)
     client
     (not (client-floating? client)))))

(define (focusclient client lift)
  ((@@ (gwwm) focusclient) client lift))

(define (focustop monitor)
  (let/ec return
    (for-each (lambda (c)
                (when (visibleon c monitor)
                  (return c)))
              (car (%fstack)))
    #f))

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
