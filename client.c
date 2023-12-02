#include "libguile/boolean.h"
#include "libguile/eval.h"
#include "libguile/goops.h"
#include "libguile/gsubr.h"
#include "libguile/list.h"
#include "libguile/numbers.h"
#include "libguile/scm.h"
#include "libguile/symbols.h"
#include "libguile/values.h"
#include "string.h"
#include <stdbool.h>
#include <stdint.h>
#include <wlr/types/wlr_scene.h>
#include "util.h"
#include "client.h"
#include "gwwm.h"
#include "wayland-util.h"
#include <wlr/types/wlr_layer_shell_v1.h>

SCM_DEFINE (gwwm_client_is_float_type_p,"client-is-float-type?",1,0,0,
            (SCM c),"")
#define FUNC_NAME s_gwwm_client_is_float_type_p
{
  GWWM_ASSERT_CLIENT_OR_FALSE(c ,1);
  struct wlr_box min = {0}, max = {0};
  SCM values =
    REF_CALL_1("gwwm client", "client-get-size-hints", c);
  max = *(UNWRAP_WLR_BOX(scm_c_value_ref(values, 0)));
  min = *(UNWRAP_WLR_BOX(scm_c_value_ref(values, 1)));

  return scm_from_bool(((min.width > 0 || min.height > 0 || max.width > 0 ||
                         max.height > 0) &&
                        (min.width == max.width || min.height == max.height)) ||
                       (UNWRAP_WLR_XDG_SURFACE(REF_CALL_1("gwwm client", "client-super-surface", c)))
                       ->toplevel->parent);

}
#undef FUNC_NAME

void
scm_init_gwwm_client(void)
{
#ifndef SCM_MAGIC_SNARFER
#include "client.x"
#endif
}
