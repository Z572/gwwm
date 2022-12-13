#include "listener.h"
#include <wayland-server-core.h>
#include "libguile/error.h"
#include "libguile/gc.h"
#include "libguile/goops.h"
#include "libguile/scm.h"
#include "util.h"
typedef struct Gwwm_listener {
  SCM obj;
  struct wl_listener listener;
} Gwwm_listener;

SCM_DEFINE(scm_register_gwwm_listener, "%register-listener", 1, 0, 0, (SCM o), "")
#define FUNC_NAME s_scm_register_gwwm_listener
{
  SCM_ASSERT(scm_from_bool(SCM_IS_A_P(o, REF("gwwm listener","<listener-manager>")))
             ,o, SCM_ARG1, FUNC_NAME);
  Gwwm_listener *listener=ecalloc(sizeof(*listener));
  SCM listeners=scm_slot_ref(o,scm_from_utf8_symbol("listeners"));
  listener->obj=o;
  scm_slot_set_x(o, scm_from_utf8_symbol("listeners"), scm_cons(WRAP_WL_LISTENER(listener)
                                                                 ,listeners));
  return WRAP_WL_LISTENER(&listener->listener);
}
#undef FUNC_NAME

SCM_DEFINE (scm_add_listen ,"%add-listen",3,0,0,(SCM o,SCM signal,SCM p),""){
    struct wl_listener* listener=UNWRAP_WL_LISTENER((scm_register_gwwm_listener(o)));
    scm_gc_protect_object(p);
    listener->notify=TO_P(p);
    wl_signal_add(UNWRAP_WL_SIGNAL(signal), listener);
    return SCM_UNSPECIFIED;
}

SCM remove_listeners(SCM o){
  REF_CALL_1("gwwm listener", "remove-listeners", o);
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
    return SCM_BOOL_F;
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
