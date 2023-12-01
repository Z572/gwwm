#ifndef GWWM_H
#define GWWM_H
#include "client.h"
#include "util.h"
#include <X11/Xlib.h>
#include <libguile.h>
#include <stdbool.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_output_management_v1.h>
#include <wlr/xwayland.h>
#include <xkbcommon/xkbcommon.h>
/* macros */
#define MAX(a,b)               \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })
#define MIN(a,b)               \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })
#define CLEANMASK(mask) (mask & ~WLR_MODIFIER_CAPS)
#define LENGTH(X) (sizeof X / sizeof X[0])
#define END(A) ((A) + LENGTH(A))
#define TAGMASK ((1 << LENGTH(tags)) - 1)

typedef struct Client Client;
enum {
  NetWMWindowTypeDialog,
  NetWMWindowTypeSplash,
  NetWMWindowTypeToolbar,
  NetWMWindowTypeUtility,
  NetLast
}; /* EWMH atoms */

typedef struct {
  uint32_t singular_anchor;
  uint32_t anchor_triplet;
  int *positive_axis;
  int *negative_axis;
  int margin;
} Edge;

typedef struct {
  const char *name;
  float mfact;
  int nmaster;
  float scale;
  enum wl_output_transform rr;
} MonitorRule;

typedef struct {
  const char *id;
  const char *title;
  unsigned int tags;
  int isfloating;
  int monitor;
} Rule;

/* function declarations */
void applyexclusive(struct wlr_box *usable_area, uint32_t anchor,
                    int32_t exclusive, int32_t margin_top, int32_t margin_right,
                    int32_t margin_bottom, int32_t margin_left);
void pointerfocus(SCM c, struct wlr_surface *surface, double sx, double sy,
                  uint32_t time);
void quitsignal(int signo);
void sigchld(int unused);
void logout_monitor(SCM m);
SCM get_gwwm_config(void);
#define GWWM_BORDERPX()                                                        \
  (scm_to_unsigned_integer(                                                    \
      REF_CALL_1("gwwm config", "config-borderpx", gwwm_config), 0, 1000))
#define GWWM_SLOPPYFOCUS_P()                                                   \
  (scm_to_bool(REF_CALL_1("gwwm config", "config-sloppyfocus?", gwwm_config)))

#define GWWM_LOCKFULLSCREEN_P()                                                \
  (scm_to_bool(REF_CALL_1("gwwm config", "config-lockfullscreen?", get_gwwm_config())))
#define GWWM_CURSOR_NORMAL_IMAGE()                                             \
  (scm_to_utf8_string(REF_CALL_1("gwwm config", "config-cursor-normal-image", get_gwwm_config())))

void xwaylandready(struct wl_listener *listener, void *data);
Atom getatom(xcb_connection_t *xc, const char *name);
/* void xwaylandready(struct wl_listener *listener, void *data); */
Atom get_netatom_n(int n);
#endif // GWWM_H
