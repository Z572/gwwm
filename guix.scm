(use-modules
 (guix utils) (guix packages)
 ((guix licenses) #:prefix license:)
 (gnu packages xorg)
 (guix download)
 (guix git-download)
 (guix gexp)
 (gnu packages gl)
 (gnu packages xdisorg)
 (guix build-system gnu)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages wm)
 (gnu packages freedesktop))

(define %srcdir
  (dirname (current-filename)))

(define libdrm-next
  (package
    (inherit libdrm)
    (name "libdrm")
    (version "2.4.110")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0dwpry9m5l27dlhq48j4bsiqwm0247cxdqwv3b7ddmkynk2f9kpf"))))))
(define wayland-next
  (package
    (inherit wayland)
    (name "wayland")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wayland.freedesktop.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "09c7rpbwavjg4y16mrfa57gk5ix6rnzpvlnv1wp7fnbh9hak985q"))))))
(define wayland-protocols-next
  (package
    (inherit wayland-protocols)
    (name "wayland-protocols")
    (version "1.25")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "0q0laxdvf8p8b7ks2cbpqf6q0rwrjycqrp8pf8rxm86hk5qhzzzi"))))
    (inputs
     (modify-inputs (package-inputs wayland-protocols)
                    (replace "wayland" wayland-next)))))
(define wlroots-next
  (package
    (inherit wlroots)
    (name "wlroots")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00s73nhi3sc48l426jdlqwpclg41kx1hv0yk4yxhbzw19gqpfm1h"))))
    (arguments (substitute-keyword-arguments (package-arguments wlroots)
                 ((#:configure-flags flags ''())
                  `(cons "-Dbackends=['drm','libinput','x11']" ,flags))))

    (propagated-inputs
     (modify-inputs (package-propagated-inputs wlroots)
                    (prepend libdrm-next libglvnd xcb-util-renderutil)
                    (replace "wayland" wayland-next)
                    (replace "wayland-protocols" wayland-protocols-next)))))
(package
  (name "gwwm")
  (version "0.1")
  (source (local-file "." "gwwm-checkout"
                      #:recursive? #t
                      #:select? (git-predicate %srcdir)))
  (build-system gnu-build-system)
  (arguments `(#:make-flags '("GUILE_AUTO_COMPILE=0")))
  (native-inputs
   (list autoconf automake
         pkg-config
         texinfo))
  (inputs (list guile-3.0 wlroots-next))
  (synopsis "")
  (description "")
  (home-page "")
  (license license:gpl3+))
