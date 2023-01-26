#ifndef GWWM_CLIENT_H
#define GWWM_CLIENT_H
#include "util.h"
#include <libguile.h>
#include <stdbool.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_surface.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/box.h>

#define GWWM_BORDERCOLOR()                                                     \
  (TO_P(REF_CALL_1(                                                            \
      "gwwm color", "color->pointer",                                          \
      REF_CALL_1("gwwm config", "config-bordercolor", get_gwwm_config()))))

#define GWWM_FOCUSCOLOR()                                                      \
  (TO_P(REF_CALL_1(                                                            \
      "gwwm color", "color->pointer",                                          \
      REF_CALL_1("gwwm config", "config-focuscolor", get_gwwm_config()))))

#define GWWM_FULLSCREEN_BG()                                                   \
  (TO_P(REF_CALL_1(                                                            \
      "gwwm color", "color->pointer",                                          \
      REF_CALL_1("gwwm config", "config-fullscreenbg", get_gwwm_config()))))
#define MAKE_CLIENT(o)                                                         \
  (scm_call_3(REF("oop goops", "make"), REF("gwwm client", "<gwwm-client>"),   \
              scm_from_utf8_keyword("data"), FROM_P(o)))

#define CLIENT_IS_FULLSCREEN(c)                                                \
  scm_to_bool(REF_CALL_1("gwwm client", "client-fullscreen?", WRAP_CLIENT(c)))
#define CLIENT_SET_FULLSCREEN(c, f)                                            \
  (REF_CALL_2("gwwm client", "client-set-fullscreen!", (WRAP_CLIENT(c)),       \
              (scm_from_bool(f))))
#define CLIENT_IS_FLOATING(c)                                                  \
  scm_to_bool(REF_CALL_1("gwwm client", "client-floating?", WRAP_CLIENT(c)))
#define CLIENT_SET_FLOATING(c, f)                                              \
  (REF_CALL_2("gwwm client", "client-set-floating!", (WRAP_CLIENT(c)),         \
              (scm_from_bool(f))))
#define CLIENT_IS_URGENT_P(c)                                                  \
  scm_to_bool(REF_CALL_1("gwwm client", "client-urgent?", WRAP_CLIENT(c)))
#define CLIENT_SET_URGENT(c, f)                                                \
  (REF_CALL_2("gwwm client", "client-set-urgent!", (WRAP_CLIENT(c)),           \
              (scm_from_bool(f))))
#define CLIENT_SCENE(c)                                                        \
  ((struct wlr_scene_node *)(UNWRAP_WLR_SCENE_NODE(                            \
      REF_CALL_1("gwwm client", "client-scene", WRAP_CLIENT(c)))))
#define CLIENT_SET_SCENE(c, s)                                                 \
  (REF_CALL_2("gwwm client", "client-set-scene!", (WRAP_CLIENT(c)),            \
              (WRAP_WLR_SCENE_NODE(s))))
#define CLIENT_BW(c)                                                           \
  (scm_to_unsigned_integer(                                                    \
      REF_CALL_1("gwwm client", "client-border-width", WRAP_CLIENT(c)), 0,     \
      100))
#define CLIENT_SET_BW(c, b)                                                    \
  (REF_CALL_2("gwwm client", "client-set-border-width!", (WRAP_CLIENT(c)),     \
              scm_from_unsigned_integer(b)))

#define CLIENT_SURFACE(c)                                                      \
  ((struct wlr_surface *)UNWRAP_WLR_SURFACE(                                   \
      REF_CALL_1("gwwm client", "client-surface", WRAP_CLIENT(c))))

#define CLIENT_IS_LAYER_SHELL(cl)                               \
  (SCM_IS_A_P(cl, REFP("gwwm client", "<gwwm-layer-client>")))
#define CLIENT_IS_XDG_SHELL(cl)                                                \
  (SCM_IS_A_P(cl, REFP("gwwm client", "<gwwm-xdg-client>")))
#define CLIENT_IS_MANAGED(c)                                                   \
  (!scm_to_bool(REF_CALL_1("gwwm client", "client-is-unmanaged?", WRAP_CLIENT(c))))
#define CLIENT_SET_BORDER_COLOR(c, color)                                      \
  scm_call_2(REFP("gwwm client", "client-set-border-color"), WRAP_CLIENT(c),   \
             color)

struct wlr_surface *client_surface_at(SCM c, double cx, double cy,
                                      double *sx, double *sy);

#endif
