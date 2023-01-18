#ifndef GWWM_CLIENT_H
#define GWWM_CLIENT_H
#include "util.h"
#include <libguile.h>
#include <stdbool.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_surface.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/box.h>
/*
 * Attempt to consolidate unavoidable suck into one file, away from dwl.c.  This
 * file is not meant to be pretty.  We use a .h file with static inline
 * functions instead of a separate .c module, or function pointers like sway, so
 * that they will simply compile out if the chosen #defines leave them unused.
 */

/* Leave these functions first; they're used in the others */
enum gwwm_client_type {
  GWWM_XDG_CLIENT_TYPE,
  GWWM_X_CLIENT_TYPE,
  GWWM_LAYER_CLIENT_TYPE
};
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

#define WRAP_CLIENT(o) find_client(o)
#define UNWRAP_CLIENT(o) unwrap_client_1(o)

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
#define CLIENT_SET_SURFACE(c, b)                                               \
  (scm_call_2(                                                                 \
      REF_CALL_1("guile", "setter", (REF("gwwm client", "client-surface"))),   \
      (WRAP_CLIENT(c)), (WRAP_WLR_SURFACE(b))))

#define CLIENT_IS_LAYER_SHELL(cl)                                              \
  (SCM_IS_A_P(cl, REFP("gwwm client", "<gwwm-layer-client>")))
#define CLIENT_IS_XDG_SHELL(cl)                                                \
  (SCM_IS_A_P(cl, REFP("gwwm client", "<gwwm-xdg-client>")))
#define CLIENT_IS_MANAGED(c)                                                   \
  (!scm_to_bool(REF_CALL_1("gwwm client", "client-is-unmanaged?", WRAP_CLIENT(c))))
#define CLIENT_SET_BORDER_COLOR(c, color)                                      \
  scm_call_2(REFP("gwwm client", "client-set-border-color"), WRAP_CLIENT(c),   \
             color)
typedef struct Monitor Monitor;
typedef struct Client {
  /* Must keep these three elements in this order */
  struct wl_list link;
  SCM scm;
} Client;

Client *unwrap_client_1(SCM o);
SCM find_client(Client *c);
void client_set_resize_configure_serial(Client *c, uint32_t i);
uint32_t client_resize_configure_serial(Client *c);
void register_client(Client *c, enum gwwm_client_type type);
void logout_client(Client *c);
struct wlr_scene_rect *client_border_n(Client *c, int n);
struct wlr_scene_rect *client_fullscreen_bg(void *c,
                                            struct wlr_scene_rect *change);
bool client_is_x11(Client *c);
Client *client_from_wlr_surface(struct wlr_surface *s);
void client_activate_surface(struct wlr_surface *s, int activated);
void client_for_each_surface(Client *c, wlr_surface_iterator_func_t fn,
                             void *data);
const char *client_get_appid(Client *c);
struct wlr_box *client_get_geometry(Client *c);
const char *client_get_title(Client *c);
bool client_is_float_type(Client *c);
bool client_is_unmanaged(Client *c);
void client_notify_enter(struct wlr_surface *s, struct wlr_keyboard *kb);
struct wlr_surface *client_surface_at(Client *c, double cx, double cy,
                                      double *sx, double *sy);
typedef void (*client_iterator_func_t)(Client *c);
void client_for_each_alives(client_iterator_func_t fn);
void client_restack_surface(Client *c);
void *client_from_listener(struct wl_listener *listener);
void client_add_listen(void *c ,struct wl_signal *signal, wl_notify_func_t func);
int client_tags(Client *c);
void set_client_tags(Client *c,int tags);
void client_set_resizing(Client *c, bool resizing);
Client* toplevel_from_popup(struct wlr_xdg_popup *popup);
struct wlr_box* client_geom(void *c);
void set_client_geom(Client *c , struct wlr_box* box);
struct wlr_scene_node *client_scene_surface(Client *c, struct wlr_scene_node *surface);

#endif
