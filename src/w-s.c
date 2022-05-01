#include <libguile.h>
#include <wayland-server-core.h>
SCM_DEFINE(scm_wl_signal_get, "%wl-signal-get", 2, 0, 0,
           (SCM signal, SCM notify), "") {

  return scm_from_pointer(
      wl_signal_get(scm_to_pointer(signal), scm_to_pointer(notify)), NULL);
}

void init_w(void) {
#ifndef SCM_MAGIC_SNARFER
#include "w-s.x"
#endif
}
