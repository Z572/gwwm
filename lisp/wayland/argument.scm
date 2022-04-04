
(define %wl-argument (bs:union `((i ,ffi:int32)
                                 (u ,ffi:uint32)
                                 (f ,ffi:int32)
                                 (s ,cstring-pointer)
                                 (o ,(bs:pointer '*)) ;; wl_object *
                                 (n ,ffi:uint32)
                                 (a ,(bs:pointer '*)) ;; wl_array *
                                 (h ,ffi:int32))))
