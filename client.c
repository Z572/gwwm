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
SCM find_client(Client *c) {
  return (c) ? c->scm : SCM_BOOL_F;
}

Client*
unwrap_client_1(SCM o)
{
  if (scm_is_false(o)) {
    return NULL;
  }
  SCM a=scm_slot_ref(o, scm_from_utf8_symbol("data"));
  if (scm_to_bool(scm_zero_p(a))) {
    scm_error(scm_misc_error_key,"unwrap-client","client is delated" ,SCM_EOL,SCM_EOL);
    return NULL;
  }
  return (TO_P(MAKE_P(a)));
}

void register_client(Client *c, enum gwwm_client_type type) {
  PRINT_FUNCTION;
  char *tp = "<gwwm-xdg-client>";
  switch (type) {
  case GWWM_LAYER_CLIENT_TYPE:
    tp = "<gwwm-layer-client>";
    break;
  case GWWM_XDG_CLIENT_TYPE:
    tp = "<gwwm-xdg-client>";
    break;
  case GWWM_X_CLIENT_TYPE:
    tp = "<gwwm-x-client>";
    break;
  }
  SCM sc=(scm_call_3(REF("oop goops", "make"), REF("gwwm client", tp),
                              scm_from_utf8_keyword("data"),
                     scm_pointer_address(FROM_P(c))));
  c->scm=sc;
}

void
logout_client(Client *c){
  PRINT_FUNCTION;
  SCM sc=WRAP_CLIENT(c);
  scm_call_1(REFP("gwwm client","logout-client") ,sc);
  c->scm=NULL;
  /* free(c); */
}


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

  if (scm_to_bool(
          REF_CALL_1("gwwm client", "client-is-x11?", c))) {
    struct wlr_xwayland_surface *surface =
        wlr_xwayland_surface_from_wlr_surface(UNWRAP_WLR_SURFACE(
            REF_CALL_1("gwwm client", "client-surface", c)));
    if (surface->modal)
      return scm_from_bool(1);

    for (size_t i = 0; i < surface->window_type_len; i++)
      if (surface->window_type[i] == get_netatom_n(NetWMWindowTypeDialog) ||
          surface->window_type[i] == get_netatom_n(NetWMWindowTypeSplash) ||
          surface->window_type[i] == get_netatom_n(NetWMWindowTypeToolbar) ||
          surface->window_type[i] == get_netatom_n(NetWMWindowTypeUtility))
        return scm_from_bool(1);

    return scm_from_bool(((min.width > 0 || min.height > 0 || max.width > 0 ||
             max.height > 0) &&
            (min.width == max.width || min.height == max.height)) ||
           wlr_xwayland_surface_from_wlr_surface(
               ((struct wlr_surface *)((scm_to_pointer(
                   (scm_call_1((scm_c_public_ref("wlroots types surface",
                                                 "unwrap-wlr-surface")),
                               (scm_call_1((scm_c_public_ref("gwwm client",
                                                             "client-surface")),
                                           c)))))))))->parent);
  }

  return scm_from_bool(((min.width > 0 || min.height > 0 || max.width > 0 ||
           max.height > 0) &&
          (min.width == max.width || min.height == max.height)) ||
         wlr_xdg_surface_from_wlr_surface(
             (scm_to_pointer(
                 (scm_call_1(scm_c_public_ref("wlroots types surface",
                                               "unwrap-wlr-surface"),
                             (scm_call_1(scm_c_public_ref("gwwm client",
                                                           "client-surface"),
                                         c)))))))
             ->toplevel->parent);

}
#undef FUNC_NAME

struct wlr_surface *client_surface_at(SCM c, double cx, double cy,
                                      double *sx, double *sy) {
  if (scm_to_bool(REF_CALL_1("gwwm client", "client-is-x11?", c)))
    return wlr_surface_surface_at(
        (scm_to_pointer((scm_call_1(
            (scm_c_public_ref("wlroots types surface", "unwrap-wlr-surface")),
            (scm_call_1((scm_c_public_ref("gwwm client", "client-surface")),
                        c)))))),
        cx, cy, sx, sy);
  return wlr_xdg_surface_surface_at(
      wlr_xdg_surface_from_wlr_surface(
          (scm_to_pointer((scm_call_1(
              (scm_c_public_ref("wlroots types surface", "unwrap-wlr-surface")),
              (scm_call_1((scm_c_public_ref("gwwm client", "client-surface")),
                          c))))))),
      cx, cy, sx, sy);
}

SCM_DEFINE_PUBLIC(gwwm_client_from_popup,"client-from-popup",1,0,0,(SCM spopup),"" ){
  struct wlr_xdg_popup *popup=UNWRAP_WLR_XDG_POPUP(spopup);
  struct wlr_xdg_surface *surface = popup->base;

  while (true) {
    switch (surface->role) {
    case WLR_XDG_SURFACE_ROLE_POPUP:
      if (wlr_surface_is_layer_surface(surface->popup->parent))
        return (wlr_layer_surface_v1_from_wlr_surface(surface->popup->parent)->data);
      else if (!wlr_surface_is_xdg_surface(surface->popup->parent))
        return NULL;

      surface = wlr_xdg_surface_from_wlr_surface(surface->popup->parent);
      break;
    case WLR_XDG_SURFACE_ROLE_TOPLEVEL:
      return (surface->data);
    case WLR_XDG_SURFACE_ROLE_NONE:
      return SCM_BOOL_F;
    }
  }
  return SCM_BOOL_F;
}

void
scm_init_gwwm_client(void)
{
#ifndef SCM_MAGIC_SNARFER
#include "client.x"
#endif
}
