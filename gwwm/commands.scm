(define-module (gwwm commands)
  #:use-module (wlroots backend session)
  #:use-module (wlroots types scene)
  #:use-module (wayland display)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 control)
  #:use-module (gwwm config)
  #:use-module (gwwm monitor)
  #:use-module (gwwm layout)
  #:use-module (gwwm client)
  #:export (spawn
            chvt
            killclient
            togglefullscreen
            togglefloating
            toggletag
            focusclient
            focustop
            gwwm-quit
            setlayout
            arrange
            focusstack
            tag))

(define (arrange m)
  (for-each
   (lambda (c)
     (when (and (client-monitor c))
       (wlr-scene-node-set-enabled
        (client-scene c) (visibleon c (client-monitor c )))))
   (client-list))
  (and=> (list-ref (monitor-layouts m) (monitor-sellt m))
         (lambda (lay)
           (and=> (layout-procedure lay)
                  (cut <> m))))
  ((@@ (gwwm) %motionnotify) 0))

(define* (tag int #:optional (client (current-client)))
  (when client
    (set! (client-tags client) int)
    (focusclient (focustop (current-monitor)) #t)
    (arrange (current-monitor))))

(define* (togglefullscreen #:optional (client (current-client)))
  (when client
    ((@@ (gwwm client) client-do-set-fullscreen)
     client (not (client-fullscreen? client)))))

(define* (setlayout layout #:optional (m (current-monitor)))
  (unless (equal? (list-ref (monitor-layouts m) (monitor-sellt m)) layout)
    (set! (monitor-sellt m) (logxor (monitor-sellt m)))
    (list-set! (monitor-layouts m) (monitor-sellt m) layout)
    (arrange m)))

(define* (toggletag tag #:optional (client (current-client)))
  (when client
    (set! (client-tags client) tag)
    (focusclient (focustop (current-monitor)) #t)
    (arrange (current-monitor))))

(define* (togglefloating #:optional (client (current-client)))
  (when (and client (not (client-fullscreen? client)))
    (client-do-set-floating
     client
     (not (client-floating? client)))))

(define (focusclient client lift)
  ((@@ (gwwm) focusclient) client lift))

(define* (focusstack bool)
  (let ((c (current-client))
        (m (current-monitor))
        (c-l (client-list)))
    (unless (or (not c)
                (<= (length c-l) 1)
                (and (client-fullscreen? c)
                     (config-lockfullscreen? ((@@ (gwwm) gwwm-config)) )))
      (and=> (let/ec return
               (for-each (lambda (o)
                           (if (equal? c o)
                               (return #f)
                               (when (visibleon o m)
                                 (return o))))
                         ((if bool identity reverse)
                          (cdr (member c (append c-l c-l))))))
             (cut focusclient <> #t)))))

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
