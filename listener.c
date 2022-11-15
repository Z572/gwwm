#include "listener.h"
#include <wayland-server-core.h>
#include "libguile/error.h"
#include "libguile/goops.h"
#include "util.h"
typedef struct Gwwm_listener {
  SCM obj;
  struct wl_listener listener;
} Gwwm_listener;

SCM_DEFINE(scm_register_gwwm_listener, "%register-listener", 1, 0, 0, (SCM o), "") {
  Gwwm_listener *listener=ecalloc(1, sizeof(*listener));
  SCM listeners=scm_slot_ref(o,scm_from_utf8_symbol("listeners"));
  listener->obj=o;
  scm_slot_set_x(o, scm_from_utf8_symbol("listeners"), scm_cons(WRAP_WL_LISTENER(listener)
                                                                 ,listeners));
  return WRAP_WL_LISTENER(&listener->listener);
}

SCM_DEFINE (scm_add_listen ,"%add-listen",3,0,0,(SCM o,SCM signal,SCM p),""){
    struct wl_listener* listener=UNWRAP_WL_LISTENER((scm_register_gwwm_listener(o)));
    listener->notify=TO_P(p);
    wl_signal_add(UNWRAP_WL_SIGNAL(signal), listener);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(remove_listeners, "remove-listeners", 1, 0, 0, (SCM c), "") {
  SCM listeners = (scm_slot_ref(c, scm_from_utf8_symbol("listeners")));
  int length = scm_to_int(REF_CALL_1("guile", "length", listeners));
  for (int i = 0; i < length; i++) {
    struct wl_listener *listener =
      UNWRAP_WL_LISTENER(scm_list_ref(listeners, scm_from_int(i)));
    wl_list_remove(&listener->link);
  }
  scm_slot_set_x(c, scm_from_utf8_symbol("listeners"),
                 scm_make_list(scm_from_int(0), SCM_UNSPECIFIED));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(scm_from_listener, "scm-from-listener", 1, 0, 0, (SCM listener),
           "")
#define FUNC_NAME s_scm_from_listener
{
  Gwwm_listener *gl =
      wl_container_of(UNWRAP_WL_LISTENER(listener), gl, listener);
  if (SCM_IS_A_P(gl->obj, REF("gwwm listener","<listener-manager>")))
      return gl->obj;
  else {
    scm_wrong_type_arg(s_scm_from_listener, 0, gl->obj);
    return SCM_UNSPECIFIED;
  };
}
#undef FUNC_NAME


void
scm_init_gwwm_listener(void)
{
#ifndef SCM_MAGIC_SNARFER
#include "listener.x"
#endif
}
