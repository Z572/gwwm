(list (channel
        (name 'guix)
        (url "https://codeberg.org/guix/guix-mirror")
        (branch "master")
        (commit
          "ede407920553f5d1ec58944db949ae13e94c6c56")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'guile-wayland)
        (url "https://github.com/guile-wayland/channel")
        (branch "master")
        (commit
          "777adcc7f61294a320f089a3d45b0d9ca41c1d89")))

;; Local Variables:
;; mode: lisp-data
;; End:
