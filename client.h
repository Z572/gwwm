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
#define CLIENT_IS_LAYER_SHELL(cl)                               \
  (SCM_IS_A_P(cl, REFP("gwwm client", "<gwwm-layer-client>")))
struct wlr_surface *client_surface_at(SCM c, double cx, double cy,
                                      double *sx, double *sy);

#endif
