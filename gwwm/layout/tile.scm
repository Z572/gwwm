(define-module (gwwm layout tile)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (wlroots util box)
  #:use-module (util572 box)
  #:use-module (gwwm monitor)
  #:use-module (gwwm client)
  #:use-module (gwwm layout)
  #:export (tile-layout))
(define (tile m)
  (and-let* ((mfact (monitor-mfact m))
             (nmaster (monitor-nmaster m))

             (clients (filter (lambda (c)
                                (and (visibleon c m)
                                     (not (client-floating? c))
                                     (not (client-fullscreen? c))))
                              (client-list)))
             (l (length clients))
             ((not (zero? l))))
    (call-with-values (lambda () (split-at clients (min nmaster l)))
      (lambda (nm others)
        (let* ((nmaster-width (if (zero? (length others))
                                  (box-width (monitor-window-area m))
                                  (* (box-width (monitor-window-area m)) mfact)))
               (nmaster-height (round (/ (box-height (monitor-window-area m)) (length nm))))
               (nmaster-boxs (map (lambda (y) (make-wlr-box 0 y nmaster-width nmaster-height))
                                  (iota (length nm)
                                        0
                                        (round (/ (box-height (monitor-window-area m))
                                                  (length nm)))))))
          (for-each (lambda (c box)
                      (client-resize c box)) clients
                      (if (zero? (length others))
                          nmaster-boxs
                          (append nmaster-boxs
                                  (let* ((other-height (round (/ (box-height (monitor-window-area m))
                                                                 (length others))))
                                         (other-width (- (box-width (monitor-window-area m)) nmaster-width)))
                                    (map (lambda (y) (make-wlr-box
                                                      nmaster-width
                                                      y
                                                      other-width
                                                      other-height ))
                                         (iota (length others) 0 other-height)))))))))))
(define tile-layout
  (make <layout>
    #:symbol "[t]"
    #:procedure tile))
