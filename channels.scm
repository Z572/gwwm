(list (channel
       (inherit %default-guix-channel)
       (url "https://codeberg.org/guix/guix-mirror"))
      (channel
       (name 'guile-wayland)
       (introduction
        (make-channel-introduction
         "9e4433fe570d2b74caee0182a2929e4a35ba59fb"
         (openpgp-fingerprint
          "7EBE A494 60CE 5E2C 0875  7FDB 3B5A A993 E1A2 DFF0")))
       (url "https://github.com/guile-wayland/channel")))
