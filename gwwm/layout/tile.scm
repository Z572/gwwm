(define-module (gwwm layout tile)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (wlroots util box)
  #:use-module (util572 box)
  #:use-module (gwwm monitor)
  #:use-module (gwwm client)
  #:use-module (gwwm layout)
  #:export (tile-layout))
(define (tile m)
  (and-let* ((clients (filter (lambda (c)
                                (and (visibleon c m)
                                     (not (client-floating? c))
                                     (not (client-fullscreen? c))))
                              (client-list)))
             (l (length clients))
             ((not (zero? l)))
             (mfact (monitor-mfact m))
             (nmaster (monitor-nmaster m))
             (window-box (monitor-window-area m))
             (b (if (< nmaster l)
                    (round (* mfact (box-width window-box)))
                    (box-width window-box))))
    (let ((nmaster-clients other-clients (split-at clients (min nmaster l))))
      (match (split-box window-box b 'y)
        ((nmaster-box other-box)
         (let ((p (lambda (cls box)
                    (unless (zero? (length cls))
                      (for-each (lambda (c box)
                                  (client-resize c box))
                                cls
                                (split-box/n
                                 box
                                 (round
                                  (/ (box-height window-box)
                                     (length cls))) 'x))))))
           (p nmaster-clients nmaster-box)
           (p other-clients (if (zero? nmaster)
                                window-box
                                other-box))))))))

(define tile-layout
  (make <layout>
    #:symbol "[t]"
    #:procedure tile))
