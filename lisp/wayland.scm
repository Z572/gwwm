(define-module (wayland))

(eval-when (eval load compile)
  (begin
    (define %public-modules
      (map (lambda (a) (cons 'wayland (list a)))
           '(;api
                                        ;argument
             callback
             client
                                        ;compositor
             config
             cursor
             display
             egl
             event-loop
             global
             interface
                                        ;keyboard
             list
             listener
                                        ;output
                                        ;pointer
             proxy
             registry
             resource
             ;;server-core
             shm
             signal
                                        ;touch
             util)))

    (let* ((current-module (current-module))
           (current-module-interface (resolve-interface (module-name current-module))))
      (for-each
       (lambda (submodule)
         (let ((submodule-interface (resolve-interface submodule)))
           (module-use! current-module submodule-interface)
           (module-use! current-module-interface submodule-interface)))
       %public-modules))))
