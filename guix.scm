(use-modules
 (guix utils) (guix packages)
 ((guix licenses) #:prefix license:)
 (gnu packages xorg)
 (guix download)
 (guix git-download)
 (gnu packages gettext)
 (guix gexp)
 (gnu packages gl)
 (gnu packages xdisorg)
 (guix build-system gnu)
 (gnu packages bash)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages gtk)
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
    (version "2.4.114")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/mesa/drm")
             (commit (string-append "libdrm-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vxfha0anvxf2i1lirh0m35avkv9rdmhd5c3s6fp6g432963vgrh"))))))

(define wayland-next
  (package
    (inherit wayland)
    (name "wayland")
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wayland/wayland")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fwad6w5jm32c04wh4gca7d1ixdj4b9dnsiy1h6qd9nxs0w47wwy"))))))

(define wayland-protocols-next
  (package
    (inherit wayland-protocols)
    (name "wayland-protocols")
    (version "1.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wayland/wayland-protocols")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kpyvnzlwfj9d57v43z5fhk7fliz6224m4hw1xj425c8vrjbw0nx"))))
    (inputs
     (modify-inputs (package-inputs wayland-protocols)
                    (replace "wayland" wayland-next)))))

(define libinput-next
  (package
    (inherit libinput)
    (name "libinput")
    (version "1.22.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/libinput/libinput")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17a5qlym2d6lg2j8fdpxda9k7x5zr35flb4wlj1bz7h0mnkh8326"))))))
(define freshup-wayland-protocols
  (package-input-rewriting/spec
   `(("libdrm" . ,(const libdrm-next))
     ("libinput-minimal" . ,(const libinput-next))
     ("wayland" . ,(const wayland-next))
     ("wayland-protocols" . ,(const wayland-protocols-next)))))

(define wlroots-next
  (freshup-wayland-protocols
   (package
     (inherit wlroots)
     (name "wlroots")
     (version "0.16.0")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://gitlab.freedesktop.org/wlroots/wlroots")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "18rfr3wfm61dv9w8m4xjz4gzq2v3k5vx35ymbi1cggkgbk3lbc4k"))))
     (arguments (substitute-keyword-arguments (package-arguments wlroots)
                  ;; ((#:configure-flags flags ''())
                  ;;  `(cons* "-Dbackends=['drm','libinput']" ,flags))
                  ((#:phases phases)
                   #~(modify-phases #$phases
                       (add-before 'configure 'sub-hwdata
                         (lambda* (#:key native-inputs inputs #:allow-other-keys)
                           (substitute* "backend/drm/meson.build"
                             (("/usr/share/hwdata/pnp.ids")
                              (search-input-file (or native-inputs inputs)
                                                 "share/hwdata/pnp.ids")))
                           (substitute* "xwayland/server.c"
                             (("Xwayland")
                              (search-input-file (or native-inputs inputs)
                                                 "bin/Xwayland")))))))))

     (propagated-inputs
      (modify-inputs (package-propagated-inputs wlroots)
        (prepend (list hwdata "pnp") ;libdrm-next
                                        ;libglvnd
                 ;; xcb-util-renderutil
                 ;; ((options->transformation
                 ;;   '((with-version . "vulkan-headers=1.3.240")))
                 ;;  vulkan-headers)
                 )
        ;; (replace "wayland" wayland-next)
        ;; (replace "libinput-minimal" libinput-next)
        ;; (replace "wayland-protocols" wayland-protocols-next)
        ))
     )))

(define freshup-wlroots
  (package-input-rewriting/spec
   `(("libdrm" . ,(const libdrm-next))
     ("libinput-minimal" . ,(const libinput-next))
     ("wayland" . ,(const wayland-next))
     ("wayland-protocols" . ,(const wayland-protocols-next))
     ("wlroots" . ,(const wlroots-next)))))

(define-public gwwm
  (package
    (name "gwwm")
    (version "0.1")
    (source (local-file "." "gwwm-checkout"
                        #:recursive? #t
                        #:select? (git-predicate %srcdir)))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list "GUILE_AUTO_COMPILE=0")
                     ;;; XXX: is a bug? why can't use gexp for #:modules
           #:modules `(((guix build guile-build-system)
                        #:select (target-guile-effective-version))
                       ,@%gnu-build-system-modules)
           #:imported-modules `((guix build guile-build-system)
                                ,@%gnu-build-system-modules)
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'build 'load-extension
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute*
                       (find-files "." ".*\\.scm")
                     (("\\(load-extension \"libgwwm\" *\"(.*)\"\\)" _ o)
                      (string-append
                       (object->string
                        `(or (false-if-exception (load-extension "libgwwm" ,o))
                             (load-extension
                              ,(string-append
                                (assoc-ref outputs "out")
                                "/lib/libgwwm.so")
                              ,o))))))))
               (add-after 'install 'wrap-executable
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (deps (map (lambda (a)
                                       (assoc-ref inputs a ))
                                     '("guile-wayland"
                                       "guile-wlroots"
                                       "guile-bytestructures"
                                       "util572"
                                       "guile-srfi-189"
                                       "guile-xkbcommon"
                                       "guile-libinput")))
                          (effective (target-guile-effective-version))
                          (mods (map (lambda (o)
                                       (string-append
                                        o "/share/guile/site/" effective))
                                     (cons out deps)))
                          (gos
                           (map (lambda (o)
                                  (string-append
                                   o "/lib/guile/" effective "/site-ccache"))
                                (cons out deps))))
                     (wrap-program (search-input-file outputs "bin/gwwm")
                       #:sh (search-input-file inputs "bin/bash")
                       `("GUILE_AUTO_COMPILE" ":" = ("0"))
                       `("GUILE_LOAD_PATH" ":" prefix ,mods)
                       `("GUILE_LOAD_COMPILED_PATH" ":" prefix ,gos))))))))
    (native-inputs
     (list autoconf automake
           pkg-config
           libtool
           gettext-minimal
           guile-3.0-latest
           bash-minimal
           texinfo))
    (inputs (list guile-3.0-latest wlroots-next xorg-server-xwayland
                  guile-cairo
                  guile-bytestructures
                  guile-srfi-189
                  (primitive-load
                   (string-append (dirname (dirname (current-filename)))
                                  "/guile-wayland/guix.scm"))
                  (primitive-load
                   (string-append (dirname (dirname (current-filename)))
                                  "/util572/guix.scm"))
                  (primitive-load
                   (string-append (dirname (dirname (current-filename)))
                                  "/guile-xkbcommon/guix.scm"))
                  (primitive-load
                   (string-append (dirname (dirname (current-filename)))
                                  "/guile-wlroots/guix.scm"))
                  (primitive-load
                   (string-append (dirname (dirname (current-filename)))
                                  "/guile-libinput/guix.scm"))))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))

(freshup-wlroots gwwm)
