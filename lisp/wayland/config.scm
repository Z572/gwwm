(define-module (wayland config)
  #:export (%wayland-libdir
            %libwayland-client
            %libwayland-server
            %libwayland-cursor))
(define %wayland-libdir "/gnu/store/pf4ahy3r4m1cdl06znir8x6698ylvdjr-wayland-1.18.0/lib/")
(define %libwayland-server (string-append %wayland-libdir "libwayland-server.so"))
(define %libwayland-client (string-append %wayland-libdir "libwayland-client.so"))
(define %libwayland-cursor (string-append %wayland-libdir "libwayland-cursor.so"))
