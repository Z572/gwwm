#include <libguile.h>
#include <wayland-server-core.h>
SCM_DEFINE (scm_wl_signal_get, "wl-signal-get" ,2,0,0
            ,(SCM signal, SCM notify),""){

  return (scm_call_1 (scm_c_public_ref("wayland signal", "wrap-wl-signal"),
                      scm_from_pointer(wl_signal_get(scm_to_pointer
                                                     (scm_call_1
                                                      (scm_c_public_ref("wayland signal", "unwrap-signal"),signal))
                                                     ,scm_to_pointer(scm_call_1(scm_c_public_ref("gwwm init", "pc->pointer"),signal)  ))
                                       ,NULL)));
}
void
init_w (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "w-s.x"
#endif
}
