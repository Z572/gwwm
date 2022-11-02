#ifndef GWWM_LISTENER_H
#define GWWM_LISTENER_H
#include <libguile.h>
SCM remove_listeners(SCM c);
SCM scm_from_listener(SCM listener);
SCM scm_register_gwwm_listener(SCM o);
#endif
