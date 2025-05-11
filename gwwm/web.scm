(define-module (gwwm web)
  #:use-module ((web server) #:prefix web:)
  #:autoload (web response) (build-response)
  #:use-module (sxml simple)
  #:use-module (gwwm)
  #:use-module (gwwm client)

  #:use-module (wlroots types output-layout)
  #:use-module (util572 box)
  #:use-module (wlroots util box)
  #:use-module (gwwm i18n)
  #:use-module (gwwm keyboard)
  #:use-module (gwwm touch)
  #:use-module (gwwm pointer)
  #:use-module (gwwm keymap)
  #:use-module (gwwm layout tile)
  #:use-module (gwwm layout)
  #:use-module (gwwm listener)
  #:use-module (gwwm monitor)
  #:export (run-debug-web-server))

(define (templatize title body)
  `(html (head (title ,title))
         (body ,@body)))
(define* (respond #:optional body #:key
                  (status 200)
                  (title "Hello hello!")
                  (doctype "<!DOCTYPE html>\n")
                  (content-type-params '((charset . "utf-8")))
                  (content-type 'text/html)
                  (extra-headers '())
                  (sxml (and body (templatize title body))))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port)
            (if sxml
                (begin
                  (if doctype (display doctype port))
                  (sxml->xml sxml port))))))

(define (web-build-monitor)
  (map (lambda (x)
         `(ol
           (li ,(monitor-name x))
           (li ,(object->string
                 (monitor-window-area x)))))
       (monitor-list)))
(define (web-build-client)
  (map (lambda (x)
         `(ol
           (li ,(string-append
                 "appid: "
                 (object->string
                  (client-appid x))))
           (li ,(string-append
                 "title: "
                 (object->string
                  (client-title x))))
           (li ,(string-append
                 "geom: "
                 (object->string
                  (client-geom x))))
           (li ,(string-append
                 "monitor: "
                 (object->string
                  (client-monitor x))))))
       (client-list)))

(define (web-build-variables)
  `((p ,(string-append "current-monitor: "
                       (object->string (current-monitor))))
    (p ,(string-append "current-client: "
                       (object->string (current-client))))
    (p ,(string-append "entire-layout-box: "
                       (object->string (entire-layout-box))))
    (p ,(string-append "layout-box: "
                       (object->string
                        (wlr-output-layout-get-box
                         (gwwm-output-layout)))))))

(define (web-handler request request-body)
  (respond
   (list (web-build-variables)
         (web-build-monitor)
         (web-build-client)
         (let* ((box (wlr-output-layout-get-box (gwwm-output-layout))))
           `(svg (@ (width ,(box-width box))
                    (height ,(box-height box))
                    (xmlns "http://www.w3.org/2000/svg"))
                 ,@(map (lambda (m)
                          (define cb (monitor-window-area m))
                          (define x (box-x cb))
                          (define y (box-y cb))
                          `((g (rect (@ (x ,x)
                                        (y ,y)
                                        (width ,(box-width cb))
                                        (height ,(box-height cb))
                                        (fill "#cc0000")
                                        (fill-opacity 0.1)
                                        (stroke "black")
                                        (stroke-width 0.5)))
                               (text (@ (x ,x)
                                        (y ,(+ 12 y))
                                        (fill "red"))

                                     ,(monitor-name m)))))
                        (monitor-list))
                 ,@(map (lambda (c)
                          (define cb (client-geom c))
                          (define x (box-x cb))
                          (define y (box-y cb))
                          `((g
                             (rect (@ (x ,x)
                                      (y ,y)
                                      (width ,(box-width cb))
                                      (height ,(box-height cb))
                                      (stroke "blue")
                                      (fill-opacity 0.9)
                                      (fill "#cccccc")
                                      (stroke-width
                                       ,(client-border-width c))))
                             (text (@ (x ,(+ 36 x))
                                      (y ,(+ 24 y))
                                      (fill "black"))
                                   ,(string-append
                                     "title: "
                                     (object->string (client-title c))
                                     " "
                                     "appid:"
                                     (object->string (client-appid c)))))))
                        (client-list))
                 )))
   #:title "gwwm web"))
(define (run-debug-web-server)
  (web:run-server (lambda (request request-body)
                    (web-handler request request-body))))
