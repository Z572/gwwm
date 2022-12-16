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
#define LISTEN(E, L, H) wl_signal_add((E), ((L)->notify = (H), (L)))
typedef struct Client Client;
typedef struct Monitor Monitor;
/* enums */
enum { CurNormal, CurMove, CurResize }; /* cursor */
/* enum { */
/*   LyrBg, */
/*   LyrBottom, */
/*   LyrTop, */
/*   LyrOverlay, */
/*   LyrTile, */
/*   LyrFloat, */
/*   LyrNoFocus, */
/*   NUM_LAYERS */
/* }; /\* scene layers *\/ */
#ifdef XWAYLAND
enum {
  NetWMWindowTypeDialog,
  NetWMWindowTypeSplash,
  NetWMWindowTypeToolbar,
  NetWMWindowTypeUtility,
  NetLast
}; /* EWMH atoms */
#endif

typedef union {
  int i;
  unsigned int ui;
  float f;
  const void *v;
} Arg;

typedef struct {
  unsigned int mod;
  unsigned int button;
  void (*func)(const Arg *);
  const Arg arg;
} Button;

typedef struct {
  uint32_t singular_anchor;
  uint32_t anchor_triplet;
  int *positive_axis;
  int *negative_axis;
  int margin;
} Edge;

typedef struct {
  struct wl_list link;
  struct wlr_input_device *device;

  struct wl_listener modifiers;
  struct wl_listener key;
  struct wl_listener destroy;
} Keyboard;

#define WRAP_MONITOR(o) find_monitor(o)
#define UNWRAP_MONITOR(o)                                                      \
  (struct Monitor *)(TO_P(MAKE_P(scm_call_1(REFP("gwwm monitor", ".data"), o))))
#define MONITOR_WLR_OUTPUT(m)                                                  \
  (struct wlr_output *)(UNWRAP_WLR_OUTPUT(scm_call_1(                          \
      REFP("gwwm monitor", "monitor-wlr-output"), (WRAP_MONITOR(m)))))
#define MONITOR_LAYOUTS(m)                                                     \
  (REF_CALL_1("gwwm monitor", "monitor-layouts", (WRAP_MONITOR(m))))
#define MONITOR_SELLT(m)                                                       \
  scm_to_int((                                                                 \
      (scm_call_1(REFP("gwwm monitor", "monitor-sellt"), (WRAP_MONITOR(m))))))
#define SET_MONITOR_WLR_OUTPUT(m, o)                                           \
  scm_call_2(REFP("gwwm monitor", "set-.wlr-output!"), (WRAP_MONITOR(m)),      \
             WRAP_WLR_OUTPUT(o))
#define SET_MONITOR_SELLT(m, o)                                                \
  scm_call_2(REFP("gwwm monitor", "set-.monitor-sellt!"), (WRAP_MONITOR(m)),   \
             scm_from_int(o))
#define MONITOR_WINDOW_AREA(m)                                                 \
  (struct wlr_box *)(UNWRAP_WLR_BOX(                                           \
      REF_CALL_1("gwwm monitor", "monitor-window-area", (WRAP_MONITOR(m)))))
#define SET_MONITOR_WINDOW_AREA(m, o)                                          \
  scm_call_2(REFP("gwwm monitor", "set-.window-area!"), (WRAP_MONITOR(m)),     \
             REF_CALL_1("oop goops","shallow-clone",WRAP_WLR_BOX(o)))
#define MONITOR_AREA(m)                                                        \
  ((struct wlr_box *)(UNWRAP_WLR_BOX(                                          \
      REF_CALL_1("gwwm monitor", "monitor-area", (WRAP_MONITOR(m))))))
#define SET_MONITOR_AREA(m, o)                                                 \
  scm_call_2(REFP("gwwm monitor", "set-.area!"), (WRAP_MONITOR(m)),            \
             WRAP_WLR_BOX(o))

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
Monitor *current_monitor();
Monitor *client_monitor(void *c, Monitor *change);
void applybounds(Client *c, struct wlr_box *bbox);
void applyexclusive(struct wlr_box *usable_area, uint32_t anchor,
                    int32_t exclusive, int32_t margin_top, int32_t margin_right,
                    int32_t margin_bottom, int32_t margin_left);
void applyrules(Client *c);
void arrange(Monitor *m);
void arrangelayer(Monitor *m, struct wl_list *list, struct wlr_box *usable_area,
                  int exclusive);
void checkidleinhibitor(struct wlr_surface *exclude);
void cleanupkeyboard(struct wl_listener *listener, void *data);
void commitlayersurfacenotify(struct wl_listener *listener, void *data);
void commitnotify(struct wl_listener *listener, void *data);
void createidleinhibitor(struct wl_listener *listener, void *data);
SCM find_monitor(Monitor *m);
void createkeyboard(struct wlr_input_device *device);
void createlayersurface(struct wl_listener *listener, void *data);
void createmon(struct wl_listener *listener, void *data);
void createnotify(struct wl_listener *listener, void *data);
void createpointer(struct wlr_input_device *device);
void create_touch(struct wlr_input_device *device);
void create_tablet_tool(struct wlr_input_device *device);
void create_tablet_pad(struct wlr_input_device *device);
void create_switch(struct wlr_input_device *device);
void destroyidleinhibitor(struct wl_listener *listener, void *data);
void destroylayersurfacenotify(struct wl_listener *listener, void *data);
Monitor *dirtomon(enum wlr_direction dir);
void dragicondestroy(struct wl_listener *listener, void *data);
void focusclient(Client *c, int lift);
void focusmon(const Arg *arg);
void focusstack(const Arg *arg);
Client *focustop(Monitor *m);
void fullscreennotify(struct wl_listener *listener, void *data);
void incnmaster(const Arg *arg);
bool keybinding(uint32_t mods, xkb_keycode_t keycode);
void keypress(struct wl_listener *listener, void *data);
void keypressmod(struct wl_listener *listener, void *data);
void maplayersurfacenotify(struct wl_listener *listener, void *data);
void motionnotify(uint32_t time);
void moveresize(const Arg *arg);
void outputmgrapply(struct wl_listener *listener, void *data);
void outputmgrapplyortest(struct wlr_output_configuration_v1 *config, int test);
void outputmgrtest(struct wl_listener *listener, void *data);
void pointerfocus(Client *c, struct wlr_surface *surface, double sx, double sy,
                  uint32_t time);
void quit(const Arg *arg);
void quitsignal(int signo);
void destroy_surface_notify(struct wl_listener *listener, void *data);
void requeststartdrag(struct wl_listener *listener, void *data);
void resize(Client *c, struct wlr_box geo, int interact);
Client *current_client(void);
void setcursor(struct wl_listener *listener, void *data);
void setfullscreen(Client *c, int fullscreen);
void setlayout(const Arg *arg);
Monitor* monitor_from_listener(struct wl_listener *listener);
void setmfact(const Arg *arg);
void setmon(Client *c, Monitor *m, unsigned int newtags);
void setpsel(struct wl_listener *listener, void *data);
void setsel(struct wl_listener *listener, void *data);
void sigchld(int unused);
void spawn(const Arg *arg);
void startdrag(struct wl_listener *listener, void *data);
void tag(const Arg *arg);
void tagmon(const Arg *arg);
void logout_monitor(SCM m);
void togglefloating(const Arg *arg);

void toggleview(const Arg *arg);
void unmaplayersurfacenotify(struct wl_listener *listener, void *data);
void updatetitle(struct wl_listener *listener, void *data);
void urgent(struct wl_listener *listener, void *data);
void set_layersurface_geom(Client *c , struct wlr_box* box);
void view(const Arg *arg);
void virtualkeyboard(struct wl_listener *listener, void *data);
Monitor *xytomon(double x, double y);
struct wlr_scene_node *xytonode(double x, double y,
                                struct wlr_surface **psurface, Client **pc,
                                Client **pl, double *nx, double *ny);
struct wlr_seat *get_gloabl_seat(void);
SCM get_gwwm_config(void);
#define GWWM_BORDERPX()                                                        \
  (scm_to_unsigned_integer(                                                    \
      REF_CALL_1("gwwm config", "config-borderpx", gwwm_config), 0, 1000))
#define GWWM_SLOPPYFOCUS_P()                                                   \
  (scm_to_bool(REF_CALL_1("gwwm config", "config-sloppyfocus?", gwwm_config)))

#define GWWM_LOCKFULLSCREEN_P()                                                \
  (scm_to_bool(                                                                \
      REF_CALL_1("gwwm config", "config-lockfullscreen?", gwwm_config)))
#define GWWM_CURSOR_NORMAL_IMAGE()                                             \
  (scm_to_utf8_string(                                                         \
      REF_CALL_1("gwwm config", "config-cursor-normal-image", gwwm_config)))
#ifdef XWAYLAND
void activatex11(struct wl_listener *listener, void *data);
void configurex11(struct wl_listener *listener, void *data);
void createnotifyx11(struct wl_listener *listener, void *data);
void xwaylandready(struct wl_listener *listener, void *data);
Atom getatom(xcb_connection_t *xc, const char *name);
void sethints(struct wl_listener *listener, void *data);
/* void xwaylandready(struct wl_listener *listener, void *data); */
Atom get_netatom_n(int n);
#endif

#endif // GWWM_H
