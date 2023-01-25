/*
 * See LICENSE file for copyright and license details.
 */
#include "libguile/boolean.h"
#include "libguile/eq.h"
#include "libguile/eval.h"
#include "libguile/foreign.h"
#include "libguile/gc.h"
#include "libguile/goops.h"
#include "libguile/gsubr.h"
#include "libguile/keywords.h"
#include "libguile/list.h"
#include "libguile/modules.h"
#include "libguile/numbers.h"
#include "libguile/scm.h"
#include "libguile/symbols.h"
#include "wlr-layer-shell-unstable-v1-protocol.h"
#include "wlr/util/box.h"
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#define _POSIX_C_SOURCE 200809L
#include <getopt.h>
#include <libinput.h>
#include <limits.h>
#include <libguile.h>
#include <linux/input-event-codes.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/backend/libinput.h>
#include <wlr/backend/wayland.h>
#include <wlr/backend/x11.h>
#include <wlr/render/allocator.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_control_v1.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
#include <wlr/types/wlr_idle.h>
#include <wlr/types/wlr_idle_inhibit_v1.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_input_inhibitor.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_layer_shell_v1.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_output_management_v1.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_presentation_time.h>
#include <wlr/types/wlr_primary_selection.h>
#include <wlr/types/wlr_primary_selection_v1.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_screencopy_v1.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_server_decoration.h>
#include <wlr/types/wlr_viewporter.h>
#include <wlr/types/wlr_virtual_keyboard_v1.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_activation_v1.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_output_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>
#include "util.h"

#ifdef XWAYLAND
#include <X11/Xlib.h>
#include <wlr/xwayland.h>
#endif
#include "gwwm.h"
#include "client.h"
/* configuration, allows nested code to access above variables */
#include "config.h"
typedef struct Monitor {
  unsigned int tagset[2];
  SCM scm;
} Monitor;

const char broken[] = "broken";
struct wlr_virtual_keyboard_manager_v1 *virtual_keyboard_mgr;
Atom netatom[NetLast];
Atom get_netatom_n(int n){
  return netatom[n];
};

SCM gwwm_config;
SCM get_gwwm_config(void) {
  return gwwm_config;
}

struct wl_listener new_virtual_keyboard = {.notify = virtualkeyboard};

SCM_DEFINE_PUBLIC(gwwm_visibleon, "visibleon", 2, 0, 0, (SCM sc, SCM sm), "")
#define FUNC_NAME s_gwwm_visibleon
{
  GWWM_ASSERT_CLIENT_OR_FALSE(sc, 1);
  PRINT_FUNCTION
  bool a =
      (scm_is_true(sm) &&
       SCM_EQ_P(REF_CALL_1("gwwm client", "client-monitor", sc), sm) &&
       (((int)exp2(scm_to_int(scm_slot_ref(sc, scm_from_utf8_symbol("tags")))))

        & scm_to_int(scm_list_ref
                     (scm_call_1(REFP("gwwm", "%monitor-tagset"), sm),
                      scm_slot_ref(REF_CALL_0("gwwm monitor", "current-monitor"),
                                   scm_from_utf8_symbol("seltags"))))));

  return scm_from_bool(a);
}
#undef FUNC_NAME

#define define_wlr_v(module ,v) struct wlr_ ##v * gwwm_##v          \
  (struct wlr_##v* var)                                             \
  {                                                                 \
    SCM b;                                                          \
    const char* m=module;                                           \
    SCM v=scm_c_public_ref("gwwm", "gwwm-" #v);                     \
    if (var) {                                                      \
      b=scm_call_1(v,                              \
                   (REF_CALL_1(m, "wrap-wlr-" #v, FROM_P(var))));   \
      return var;                                                   \
    } else {                                                        \
      b=scm_call_0(v);                             \
      return scm_is_false(b) ? NULL :                               \
        ((struct wlr_ ##v *)                                        \
         (TO_P(REF_CALL_1(m, "unwrap-wlr-" #v, b))));               \
    }                                                               \
}
define_wlr_v("wlroots backend",backend);
define_wlr_v("wlroots render allocator",allocator);
define_wlr_v("wlroots render renderer",renderer);
define_wlr_v("wlroots types idle",idle);
define_wlr_v("wlroots types cursor",cursor);
define_wlr_v("wlroots types seat",seat);
define_wlr_v("wlroots types scene",scene);
define_wlr_v("wlroots types compositor",compositor);
define_wlr_v("wlroots xwayland",xwayland);
#undef define_wlr_v
struct wlr_xdg_activation_v1 *gwwm_activation(struct wlr_xdg_activation_v1 *var) {
  SCM b;
  const char *m = "wlroots types xdg-activation";
  SCM v=scm_c_public_ref("gwwm", "gwwm-activation");
  if (var) {
    b = (scm_call_1(v,
                    ((scm_call_1((scm_c_public_ref(m, "wrap-wlr-activation")),
                                 (scm_from_pointer(var, ((void *)0))))))));
    return var;
  } else {
    b = (scm_call_0(v));
    return scm_is_false(b)
               ? NULL
               : ((struct wlr_xdg_activation_v1 *)((scm_to_pointer(
                     (scm_call_1((scm_c_public_ref(m, "unwrap-wlr-xdg-activation-v1")),
                                 b))))));
  }
};

struct wlr_layer_shell_v1 *gwwm_layer_shell(struct wlr_layer_shell_v1 *var) {
  SCM b;
  const char *m = "wlroots types layer-shell";
  SCM v=scm_c_public_ref("gwwm", "gwwm-layer-shell");
  if (var) {
    b = (scm_call_1(v,
                    ((scm_call_1((scm_c_public_ref(m, "wrap-wlr-layer-shell")),
                                 (scm_from_pointer(var, ((void *)0))))))));
    return var;
  } else {
    b = (scm_call_0(v));
    return scm_is_false(b)
               ? NULL
               : ((struct wlr_layer_shell_v1 *)((scm_to_pointer(
                     (scm_call_1((scm_c_public_ref(m, "unwrap-wlr-layer-shell")),
                                 b))))));
  }
};

struct wlr_surface* exclusive_focus(SCM surface){
  SCM b=REFP("gwwm", "exclusive-focus");
  if (surface) {
    scm_call_1(b, surface);
    return UNWRAP_WLR_SURFACE(surface);
  } else {
    SCM o=scm_call_0(b);
    return (scm_is_false(o)) ? NULL : UNWRAP_WLR_SURFACE(o);
  }
}

static struct wlr_scene_node *return_scene_node(enum zwlr_layer_shell_v1_layer n){
  char* s="";
  switch (n){
  case ZWLR_LAYER_SHELL_V1_LAYER_BACKGROUND:
    s="background-layer";
    break;
  case ZWLR_LAYER_SHELL_V1_LAYER_BOTTOM:
    s="bottom-layer";
    break;
  case ZWLR_LAYER_SHELL_V1_LAYER_TOP:
    s="top-layer";
    break;
  case ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY:
    s="overlay-layer";
    break;
  default:
    send_log(ERROR, "UNKNOW!!! enum zwlr_layer_shell_v1_layer!");
    exit(1);
  }
  return UNWRAP_WLR_SCENE_NODE(REF("gwwm",s));
}

struct wlr_xdg_shell *gwwm_xdg_shell(struct wlr_xdg_shell *var) {
  SCM b;
  const char *m = "wlroots types xdg-shell";
  if (var) {
    b = (scm_call_1((scm_c_public_ref("gwwm", "gwwm-xdg-shell")),
                    ((scm_call_1((scm_c_public_ref(m, "wrap-wlr-xdg-shell")),
                                 (scm_from_pointer(var, NULL)))))));
    return var;
  } else {
    b = (scm_call_0((scm_c_public_ref("gwwm", "gwwm-xdg-shell"))));
    return scm_is_false(b)
      ? NULL
               : ((struct wlr_xdg_shell *)((scm_to_pointer(
                     (scm_call_1((scm_c_public_ref(m, "unwrap-wlr-xdg-shell")),
                                 b))))));
  }
};
struct wlr_seat *get_gloabl_seat(void) {
  return gwwm_seat(NULL);
}

struct wlr_xcursor_manager*
gwwm_xcursor_manager(struct wlr_xcursor_manager *o) {
  SCM d;
  if (o) {
    d = REF_CALL_1("gwwm", "gwwm-xcursor-manager", WRAP_WLR_XCURSOR_MANAGER(o));
    return o;
  } else {
    d = REF_CALL_0("gwwm", "gwwm-xcursor-manager");
    return scm_is_false(d) ? NULL : UNWRAP_WLR_XCURSOR_MANAGER(d);
  }
}

struct wl_display *gwwm_display(struct wl_display *display) {
  SCM d;
  if (display) {
    d = REF_CALL_1("gwwm", "gwwm-display", WRAP_WL_DISPLAY(display));
    return display;
  } else {
    d = REF_CALL_0("gwwm", "gwwm-display");
    return scm_is_false(d) ? NULL : UNWRAP_WL_DISPLAY(d);
  }
}

struct wlr_output_layout*
gwwm_output_layout(struct wlr_output_layout *o) {
  SCM d;
  if (o) {
    d = REF_CALL_1("gwwm", "gwwm-output-layout", WRAP_WLR_OUTPUT_LAYOUT(o));
    return o;
  } else {
    d = REF_CALL_0("gwwm", "gwwm-output-layout");
    return scm_is_false(d) ? NULL : UNWRAP_WLR_OUTPUT_LAYOUT(d);
  }
}

SCM_DEFINE (gwwm_c_config, "gwwm-config",0, 0,0,
            () ,
            "c")
#define FUNC_NAME s_gwwm_c_config
{
  return gwwm_config;
}
#undef FUNC_NAME

/* compile-time check if all tags fit into an unsigned int bit array. */
struct NumTags { char limitexceeded[LENGTH(tags) > 31 ? -1 : 1]; };

/* function implementations */
Monitor *
current_monitor(){
  PRINT_FUNCTION
  SCM o=(REF_CALL_0("gwwm monitor", "current-monitor"));
  return scm_is_false(o) ? NULL: UNWRAP_MONITOR(o);
/* _current_monitor; */
}
void
set_current_monitor(Monitor *m){
  PRINT_FUNCTION
  /* _current_monitor=m; */
  scm_call_1(REFP("gwwm monitor","set-current-monitor"),WRAP_MONITOR(m));
}

void
applyexclusive(struct wlr_box *usable_area,
		uint32_t anchor, int32_t exclusive,
		int32_t margin_top, int32_t margin_right,
		int32_t margin_bottom, int32_t margin_left) {
  PRINT_FUNCTION
	Edge edges[] = {
		{ /* Top */
			.singular_anchor = ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP,
			.anchor_triplet = ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT |
				ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT |
				ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP,
			.positive_axis = &usable_area->y,
			.negative_axis = &usable_area->height,
			.margin = margin_top,
		},
		{ /* Bottom */
			.singular_anchor = ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM,
			.anchor_triplet = ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT |
				ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT |
				ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM,
			.positive_axis = NULL,
			.negative_axis = &usable_area->height,
			.margin = margin_bottom,
		},
		{ /* Left */
			.singular_anchor = ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT,
			.anchor_triplet = ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT |
				ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP |
				ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM,
			.positive_axis = &usable_area->x,
			.negative_axis = &usable_area->width,
			.margin = margin_left,
		},
		{ /* Right */
			.singular_anchor = ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT,
			.anchor_triplet = ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT |
				ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP |
				ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM,
			.positive_axis = NULL,
			.negative_axis = &usable_area->width,
			.margin = margin_right,
		}
	};
	for (size_t i = 0; i < LENGTH(edges); i++) {
		if ((anchor == edges[i].singular_anchor || anchor == edges[i].anchor_triplet)
				&& exclusive + edges[i].margin > 0) {
			if (edges[i].positive_axis)
				*edges[i].positive_axis += exclusive + edges[i].margin;
			if (edges[i].negative_axis)
				*edges[i].negative_axis -= exclusive + edges[i].margin;
			break;
		}
	}
}


SCM_DEFINE (applyrules ,"%applyrules" ,1,0,0,(SCM sc),"")
{
  PRINT_FUNCTION;
  /* rule matching */
  const char *appid, *title;
  unsigned int i=0, newtags = 0;
  const Rule *r;
  SCM sm=(REF_CALL_0("gwwm monitor", "current-monitor"));
  if (!(appid = scm_to_utf8_string(REF_CALL_1("gwwm client", "client-get-appid",sc))))
    appid = broken;
  if (!(title = (scm_to_utf8_string(REF_CALL_1("gwwm client","client-get-title", sc)))))
    title = broken;

  for (r = rules; r < END(rules); r++) {
    if ((!r->title || strstr(title, r->title))
        && (!r->id || strstr(appid, r->id))) {
      (REF_CALL_2("gwwm client", "client-set-floating!", sc,
                  scm_from_bool(r->isfloating)));
      newtags |= r->tags;
      i = 0;
      SCM monitor_list=(REF_CALL_0("gwwm monitor", "monitor-list"));
      int monitor_list_length= scm_to_int(scm_length(monitor_list));
      for (int i=0;r->monitor==i <= monitor_list_length ; i++){
        sm=scm_list_ref(monitor_list, scm_from_int(i));
      }
    }
  }
  wlr_scene_node_reparent((UNWRAP_WLR_SCENE_NODE
                           (REF_CALL_1("gwwm client", "client-scene", sc))),
                          (UNWRAP_WLR_SCENE_NODE
                           (REF("gwwm",
                                scm_to_bool
                                (REF_CALL_1
                                 ("gwwm client", "client-floating?", sc))
                                ? "float-layer"
                                : "tile-layer"))));
  scm_call_3(REFP("gwwm", "setmon"),
             sc,
             sm,
             scm_from_unsigned_integer(exp(newtags)));

  return SCM_UNSPECIFIED;
}

void
arrange(Monitor *m)
{
  REF_CALL_1("gwwm commands","arrange",(WRAP_MONITOR(m)));
}

void arrange_l(Client *layersurface,SCM m, struct wlr_box *usable_area, int exclusive) {

  struct wlr_box *full_area = ((struct wlr_box *)(UNWRAP_WLR_BOX(
      REF_CALL_1("gwwm monitor", "monitor-area", m))));
  struct wlr_layer_surface_v1 *wlr_layer_surface =
      wlr_layer_surface_v1_from_wlr_surface(CLIENT_SURFACE(layersurface));
  struct wlr_layer_surface_v1_state *state = &wlr_layer_surface->current;
  struct wlr_box bounds;
  struct wlr_box box = {.width = state->desired_width,
                        .height = state->desired_height};
  const uint32_t both_horiz =
      ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT | ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT;
  const uint32_t both_vert =
      ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP | ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM;

  if (!(exclusive != (state->exclusive_zone > 0))) {
    bounds = state->exclusive_zone == -1 ? *full_area : *usable_area;

    /* Horizontal axis */
    if ((state->anchor & both_horiz) && box.width == 0) {
      box.x = bounds.x;
      box.width = bounds.width;
    } else if ((state->anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT)) {
      box.x = bounds.x;
    } else if ((state->anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT)) {
      box.x = bounds.x + (bounds.width - box.width);
    } else {
      box.x = bounds.x + ((bounds.width / 2) - (box.width / 2));
    }
    /* Vertical axis */
    if ((state->anchor & both_vert) && box.height == 0) {
      box.y = bounds.y;
      box.height = bounds.height;
    } else if ((state->anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP)) {
      box.y = bounds.y;
    } else if ((state->anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM)) {
      box.y = bounds.y + (bounds.height - box.height);
    } else {
      box.y = bounds.y + ((bounds.height / 2) - (box.height / 2));
    }
    /* Margin */
    if ((state->anchor & both_horiz) == both_horiz) {
      box.x += state->margin.left;
      box.width -= state->margin.left + state->margin.right;
    } else if ((state->anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_LEFT)) {
      box.x += state->margin.left;
    } else if ((state->anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_RIGHT)) {
      box.x -= state->margin.right;
    }
    if ((state->anchor & both_vert) == both_vert) {
      box.y += state->margin.top;
      box.height -= state->margin.top + state->margin.bottom;
    } else if ((state->anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP)) {
      box.y += state->margin.top;
    } else if ((state->anchor & ZWLR_LAYER_SURFACE_V1_ANCHOR_BOTTOM)) {
      box.y -= state->margin.bottom;
    }
    if (box.width < 0 || box.height < 0) {
      wlr_layer_surface_v1_destroy(wlr_layer_surface);
      /* continue; */
    } else {
      set_client_geom(layersurface, &box);

      if (state->exclusive_zone > 0)
        applyexclusive(usable_area, state->anchor, state->exclusive_zone,
                       state->margin.top, state->margin.right,
                       state->margin.bottom, state->margin.left);
      wlr_scene_node_set_position(CLIENT_SCENE(layersurface), box.x, box.y);
      wlr_layer_surface_v1_configure(wlr_layer_surface, box.width, box.height);
    }
  }
}

SCM_DEFINE(arrangelayer,"arrangelayer",4,0,0,(SCM sm,SCM sl,SCM box,SCM exclusive),"")
{
  Client *layersurface;
  struct wl_list *list=(UNWRAP_WL_LIST(sl));
  struct wlr_box *usable_area=(UNWRAP_WLR_BOX(box));
  bool b=scm_to_bool(exclusive);
  wl_list_for_each(layersurface, list, link) {
    arrange_l(layersurface, sm, usable_area,b);
  }

  return SCM_UNSPECIFIED;
}

SCM_DEFINE(arrange_interactive_layer,"arrange-interactive-layer",1,0,0,(SCM sm),"")
{
  Client *layersurface;
  uint32_t layers_above_shell[] = {
    ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY,
    ZWLR_LAYER_SHELL_V1_LAYER_TOP,
  };
  for (size_t i = 0; i < LENGTH(layers_above_shell); i++) {
    wl_list_for_each_reverse(layersurface,
                             (UNWRAP_WL_LIST
                              (scm_list_ref
                               (scm_slot_ref(sm, scm_from_utf8_symbol("layers")),
                                scm_from_size_t(layers_above_shell[i])))),
                             link) {
      struct wlr_surface *surface = CLIENT_SURFACE(layersurface);
      struct wlr_layer_surface_v1 *lsurface =
        TO_P(REF_CALL_1("wlroots types","get-pointer",
                        scm_slot_ref(WRAP_CLIENT(layersurface),
                                     scm_from_utf8_symbol("super-surface"))));
      if (lsurface->current.keyboard_interactive ==
          ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_EXCLUSIVE) {
        /* Deactivate the focused client. */
        REF_CALL_2("gwwm","focusclient",SCM_BOOL_F, SCM_BOOL_F);

        exclusive_focus(WRAP_WLR_SURFACE(surface));
        client_notify_enter(exclusive_focus(NULL), wlr_seat_get_keyboard(gwwm_seat(NULL)));
        return SCM_UNSPECIFIED;
      }
    }
  }
  return SCM_UNSPECIFIED;
}

Monitor *
client_monitor(void *c ,Monitor *change) {
  /* PRINT_FUNCTION; */
  SCM m;
  SCM sc=WRAP_CLIENT(c);
  if (change) {
    m=WRAP_MONITOR(change);
    scm_slot_set_x(sc,scm_from_utf8_symbol("monitor"),m);
    return change;
  } else {
    m=REF_CALL_1("gwwm client", "client-monitor", sc);
    return scm_is_false(m)? NULL : UNWRAP_MONITOR(m);
  }
}

struct wlr_scene_output*
monitor_scene_output(Monitor *m, struct wlr_scene_output *o){
  SCM sm=WRAP_MONITOR(m);
  SCM s=scm_from_utf8_symbol("scene-output");
  if (o) {
    scm_slot_set_x(sm,s,WRAP_WLR_SCENE_OUTPUT(o));
    return o;
  } else {
    return UNWRAP_WLR_SCENE_OUTPUT(scm_slot_ref(sm, s));
  }
}

SCM_DEFINE (gwwm_cleanup, "%gwwm-cleanup",0,0,0, () ,"")
#define D(funcname,args , ...) (funcname(args ,##__VA_ARGS__));; send_log(DEBUG,#funcname " done");
{

  PRINT_FUNCTION;
  scm_c_run_hook(REF("gwwm hooks", "gwwm-cleanup-hook"),
                 scm_make_list(scm_from_int(0), SCM_UNSPECIFIED));
#ifdef XWAYLAND
    D(wlr_xwayland_destroy,gwwm_xwayland(NULL));
#endif
	D(wl_display_destroy_clients,(gwwm_display(NULL)));
	D(wlr_backend_destroy,(gwwm_backend(NULL)));
    D(wlr_renderer_destroy,(gwwm_renderer(NULL)));
	D(wlr_allocator_destroy,(gwwm_allocator(NULL)));
	D(wlr_xcursor_manager_destroy,(gwwm_xcursor_manager(NULL)));
    D(wlr_cursor_destroy,(gwwm_cursor(NULL)));
    D(wlr_output_layout_destroy,(gwwm_output_layout(NULL)));
	D(wlr_seat_destroy,(gwwm_seat(NULL)));
	D(wl_display_destroy,(gwwm_display(NULL)));
    return SCM_UNSPECIFIED;
}
#undef D

SCM_DEFINE (commitlayersurfacenotify,"commit-layer-client-notify",3,0,0,
            (SCM c,SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
  Client *layersurface = UNWRAP_CLIENT(c);
	struct wlr_layer_surface_v1 *wlr_layer_surface = wlr_layer_surface_v1_from_wlr_surface(CLIENT_SURFACE(layersurface));
    Monitor *m=client_monitor(layersurface,NULL);
	if (return_scene_node(wlr_layer_surface->current.layer) != CLIENT_SCENE(layersurface)) {
		wlr_scene_node_reparent(CLIENT_SCENE(layersurface),
                                return_scene_node(wlr_layer_surface->current.layer));
		wl_list_remove(&layersurface->link);
		wl_list_insert(((UNWRAP_WL_LIST(scm_list_ref(scm_slot_ref(WRAP_MONITOR(m), scm_from_utf8_symbol("layers")),scm_from_int(wlr_layer_surface->current.layer))))),
				&layersurface->link);
	}
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (createlayersurface,"create-layer-client",2,0,0,(SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
	struct wlr_layer_surface_v1 *wlr_layer_surface = data;
	Client *layersurface;
	struct wlr_layer_surface_v1_state old_state;
	if (!wlr_layer_surface->output) {
      wlr_layer_surface->output = MONITOR_WLR_OUTPUT(current_monitor());
	}
	layersurface = scm_gc_calloc(sizeof(Client),"layer-client");

    register_client(layersurface,GWWM_LAYER_CLIENT_TYPE);
    scm_slot_set_x(WRAP_CLIENT(layersurface),
                   scm_from_utf8_symbol("super-surface"),
                   WRAP_WLR_LAYER_SURFACE(wlr_layer_surface));
    Monitor *m=wlr_layer_surface->output->data;
    client_monitor(layersurface,m);
	wlr_layer_surface->data = WRAP_CLIENT(layersurface);

	CLIENT_SET_SCENE(layersurface,(wlr_layer_surface->surface->data =
                                   wlr_scene_subsurface_tree_create(return_scene_node(wlr_layer_surface->pending.layer),
			wlr_layer_surface->surface)));
	CLIENT_SCENE(layersurface)->data = WRAP_CLIENT(layersurface);
	wl_list_insert(((UNWRAP_WL_LIST(scm_list_ref(scm_slot_ref(WRAP_MONITOR(m), scm_from_utf8_symbol("layers")),scm_from_int(wlr_layer_surface->pending.layer))))),
			&layersurface->link);

	/* Temporarily set the layer's current state to pending
	 * so that we can easily arrange it
	 */
	old_state = wlr_layer_surface->current;
	wlr_layer_surface->current = wlr_layer_surface->pending;
    scm_call_1(REFP("gwwm", "arrangelayers"),WRAP_MONITOR(m));
	wlr_layer_surface->current = old_state;
    return WRAP_CLIENT(layersurface);
}

void
register_monitor(Monitor *m) {
  PRINT_FUNCTION;
  SCM sm=(scm_call_3(REF("oop goops", "make"), REF("gwwm monitor", "<gwwm-monitor>"),
                     scm_from_utf8_keyword("data"), scm_pointer_address(FROM_P(m))));
  m->scm=sm;
}

SCM
find_monitor(Monitor *m) {
  return (m && m->scm) ? m->scm : SCM_BOOL_F;
}

void
logout_monitor(SCM m){
}

void
init_monitor(struct wlr_output *wlr_output){
  const MonitorRule *r;
  Monitor *m = wlr_output->data;
	m->tagset[0] = m->tagset[1] = 2;
	for (r = monrules; r < END(monrules); r++) {
		if (!r->name || strstr(wlr_output->name, r->name)) {
			wlr_output_set_scale(wlr_output, r->scale);
			wlr_xcursor_manager_load(gwwm_xcursor_manager(NULL), r->scale);
			wlr_output_set_transform(wlr_output, r->rr);
			break;
		}
	}
}

SCM_DEFINE (createmon,"create-monitor",2,0,0,(SCM slistener ,SCM sdata),"")
{
	/* This event is raised by the backend when a new output (aka a display or
	 * monitor) becomes available. */

  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
	struct wlr_output *wlr_output = data;
	const MonitorRule *r;
	Monitor *m = wlr_output->data = scm_gc_calloc(sizeof(*m),"monitor");
    register_monitor(m);
    init_monitor(wlr_output);
    return WRAP_MONITOR(m);
}

SCM_DEFINE(createnotify,"create-notify",2,0,0,(SCM sl ,SCM d),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(sl);
  void *data= TO_P(d);
  struct wlr_xdg_surface *xdg_surface = data;
  Client *c = scm_gc_calloc(sizeof(*c), "xdg-client");
  register_client(c,GWWM_XDG_CLIENT_TYPE);
  return WRAP_CLIENT(c);
}

SCM_DEFINE (destroylayersurfacenotify,"destroy-layer-client-notify",3,0,0,(SCM c,SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
  Client *layersurface = UNWRAP_CLIENT(c);
  wl_list_remove(&layersurface->link);
  logout_client(UNWRAP_CLIENT(c));
  return SCM_UNSPECIFIED;
}

Monitor *
dirtomon(enum wlr_direction dir)
{
  PRINT_FUNCTION
	struct wlr_output *next;
	if ((next = wlr_output_layout_adjacent_output
         (gwwm_output_layout(NULL),
          dir, MONITOR_WLR_OUTPUT(current_monitor()),
          MONITOR_AREA((current_monitor()))->x,
          MONITOR_AREA((current_monitor()))->y)))
		return next->data;
	if ((next = wlr_output_layout_farthest_output
         (gwwm_output_layout(NULL),
          dir ^ (WLR_DIRECTION_LEFT|WLR_DIRECTION_RIGHT),
          MONITOR_WLR_OUTPUT(current_monitor()),
          MONITOR_AREA((current_monitor()))->x,
          MONITOR_AREA((current_monitor()))->y)))
		return next->data;
	return current_monitor();
}

SCM_DEFINE (gwwm_dirtomon ,"dirtomon" ,1,0,0,(SCM dir),"")
  #define FUNC_NAME s_gwwm_dirtomon
{
  return WRAP_MONITOR(dirtomon(scm_to_int(dir)));
}
#undef  FUNC_NAME

SCM_DEFINE (gwwm_focusmon ,"focusmon",1,0,0,(SCM a),"" )
#define FUNC_NAME s_gwwm_focusmon
{
  PRINT_FUNCTION;
  int i = 0, nmons = scm_to_int(scm_length((REF_CALL_0("gwwm monitor", "monitor-list"))));
  if (nmons)
    do /* don't switch to disabled mons */
      set_current_monitor(dirtomon(scm_to_int(a)));
    while (!MONITOR_WLR_OUTPUT(current_monitor())->enabled && i++ < nmons);
  REF_CALL_2("gwwm","focusclient",
             REF_CALL_1("gwwm commands", "focustop",
                        (REF_CALL_0("gwwm monitor", "current-monitor"))),
             SCM_BOOL_T);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (destroy_surface_notify,"destroy-surface-notify",3,0,0,
            (SCM c, SCM listener, SCM data),"")
{
  PRINT_FUNCTION;
  logout_client(UNWRAP_CLIENT(c));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (gwwm_motionnotify, "%motionnotify" , 1,0,0,
            (SCM stime), "")
#define FUNC_NAME s_gwwm_motionnotify
{
  uint32_t time=scm_to_uint32( stime);
  PRINT_FUNCTION;
	double sx = 0, sy = 0;
	Client *c = NULL;
	struct wlr_surface *surface = NULL;
	struct wlr_drag_icon *icon;
    struct wlr_cursor *cursor=gwwm_cursor(NULL);

	/* Find the client under the pointer and send the event along. */
	xytonode(cursor->x, cursor->y, &surface, &c, NULL, &sx, &sy);

	/* If there's no client surface under the cursor, set the cursor image to a
	 * default. This is what makes the cursor image appear when you move it
	 * off of a client or over its border. */
	if (!surface && time)
      wlr_xcursor_manager_set_cursor_image(gwwm_xcursor_manager(NULL), GWWM_CURSOR_NORMAL_IMAGE(), cursor);

	pointerfocus(c, surface, sx, sy, time);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(gwwm_outputmgrapplyortest,"output-manager-apply-or-test",2,0,0,
           (SCM sconfig,SCM test_p),"")
{
  PRINT_FUNCTION;
	/*
	 * Called when a client such as wlr-randr requests a change in output
	 * configuration.  This is only one way that the layout can be changed,
	 * so any Monitor information should be updated by updatemons() after an
	 * gwwm_output_layout(NULL).change event, not here.
	 */
  struct wlr_output_configuration_v1 *config=UNWRAP_WLR_OUTPUT_CONFIGURATION_V1(sconfig);
  bool test=scm_to_bool(test_p);
	struct wlr_output_configuration_head_v1 *config_head;
	int ok = 1;

	/* First disable outputs we need to disable */
	wl_list_for_each(config_head, &config->heads, link) {
		struct wlr_output *wlr_output = config_head->state.output;
		if (!wlr_output->enabled || config_head->state.enabled)
			continue;
		wlr_output_enable(wlr_output, 0);
		if (test) {
			ok &= wlr_output_test(wlr_output);
			wlr_output_rollback(wlr_output);
		} else {
			ok &= wlr_output_commit(wlr_output);
		}
	}

	/* Then enable outputs that need to */
	wl_list_for_each(config_head, &config->heads, link) {
		struct wlr_output *wlr_output = config_head->state.output;
		if (!config_head->state.enabled)
			continue;

		wlr_output_enable(wlr_output, 1);
		if (config_head->state.mode)
			wlr_output_set_mode(wlr_output, config_head->state.mode);
		else
			wlr_output_set_custom_mode(wlr_output,
					config_head->state.custom_mode.width,
					config_head->state.custom_mode.height,
					config_head->state.custom_mode.refresh);

		wlr_output_layout_move(gwwm_output_layout(NULL), wlr_output,
				config_head->state.x, config_head->state.y);
		wlr_output_set_transform(wlr_output, config_head->state.transform);
		wlr_output_set_scale(wlr_output, config_head->state.scale);

		if (test) {
			ok &= wlr_output_test(wlr_output);
			wlr_output_rollback(wlr_output);
		} else {
			int output_ok = 1;
			/* If it's a custom mode to avoid an assertion failed in wlr_output_commit()
			 * we test if that mode does not fail rather than just call wlr_output_commit().
			 * We do not test normal modes because (at least in my hardware (@sevz17))
			 * wlr_output_test() fails even if that mode can actually be set */
			if (!config_head->state.mode)
				ok &= (output_ok = wlr_output_test(wlr_output)
						&& wlr_output_commit(wlr_output));
			else
				ok &= wlr_output_commit(wlr_output);

			/* In custom modes we call wlr_output_test(), it it fails
			 * we need to rollback, and normal modes seems to does not cause
			 * assertions failed in wlr_output_commit() which rollback
			 * the output on failure */
			if (!output_ok)
				wlr_output_rollback(wlr_output);
		}
	}

	if (ok)
		wlr_output_configuration_v1_send_succeeded(config);
	else
		wlr_output_configuration_v1_send_failed(config);
	wlr_output_configuration_v1_destroy(config);
    return SCM_UNSPECIFIED;
}

void
pointerfocus(Client *c, struct wlr_surface *surface, double sx, double sy,
		uint32_t time)
{
  PRINT_FUNCTION
	struct timespec now;
	int internal_call = !time;

	if (GWWM_SLOPPYFOCUS_P() && !internal_call && c && !client_is_unmanaged(c))
      REF_CALL_2("gwwm","focusclient",WRAP_CLIENT(c), SCM_BOOL_F);

	/* If surface is NULL, clear pointer focus */
	if (!surface) {
		wlr_seat_pointer_notify_clear_focus(gwwm_seat(NULL));
		return;
	}

	if (internal_call) {
		clock_gettime(CLOCK_MONOTONIC, &now);
		time = now.tv_sec * 1000 + now.tv_nsec / 1000000;
	}

	/* Let the client know that the mouse cursor has entered one
	 * of its surfaces, and make keyboard focus follow if desired.
	 * wlroots makes this a no-op if surface is already focused */
	wlr_seat_pointer_notify_enter(gwwm_seat(NULL), surface, sx, sy);
	wlr_seat_pointer_notify_motion(gwwm_seat(NULL), time, sx, sy);

}

void
quitsignal(int signo)
{
  REF_CALL_0("gwwm commands","gwwm-quit");
}

SCM_DEFINE (setpsel,"setpsel",2,0,0,(SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
	/* This event is raised by the seat when a client wants to set the selection,
	 * usually when the user copies something. wlroots allows compositors to
	 * ignore such requests if they so choose, but in dwl we always honor
	 */
	struct wlr_seat_request_set_primary_selection_event *event = data;
	wlr_seat_set_primary_selection(gwwm_seat(NULL), event->source, event->serial);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (gwwm_setup_signal,"%gwwm-setup-signal",0,0,0,(),"")
{
  sigchld(0);
  signal(SIGINT, quitsignal);
  signal(SIGTERM, quitsignal);
  return SCM_UNSPECIFIED;
}
SCM_DEFINE (gwwm_setup_othres,"%gwwm-setup-othres",0,0,0,(),"")
{
  	wlr_export_dmabuf_manager_v1_create(gwwm_display(NULL));
	wlr_screencopy_manager_v1_create(gwwm_display(NULL));
	wlr_data_control_manager_v1_create(gwwm_display(NULL));
	wlr_data_device_manager_create(gwwm_display(NULL));
	wlr_gamma_control_manager_v1_create(gwwm_display(NULL));
	wlr_primary_selection_v1_device_manager_create(gwwm_display(NULL));
	wlr_viewporter_create(gwwm_display(NULL));
    return SCM_UNSPECIFIED;
}
SCM_DEFINE (gwwm_setup,"%gwwm-setup" ,0,0,0,(),"")
{
    wlr_xdg_output_manager_v1_create(gwwm_display(NULL), gwwm_output_layout(NULL));
	wlr_server_decoration_manager_set_default_mode(
			wlr_server_decoration_manager_create(gwwm_display(NULL)),
			WLR_SERVER_DECORATION_MANAGER_MODE_SERVER);
	wlr_xdg_decoration_manager_v1_create(gwwm_display(NULL));
	virtual_keyboard_mgr = wlr_virtual_keyboard_manager_v1_create(gwwm_display(NULL));
	wl_signal_add(&virtual_keyboard_mgr->events.new_virtual_keyboard,
			&new_virtual_keyboard);
    return SCM_UNSPECIFIED;
}

void
sigchld(int unused)
{
	/* We should be able to remove this function in favor of a simple
	 *     signal(SIGCHLD, SIG_IGN);
	 * but the Xwayland implementation in wlroots currently prevents us from
	 * setting our own disposition for SIGCHLD.
	 */
	if (signal(SIGCHLD, sigchld) == SIG_ERR)
		die("can't install SIGCHLD handler:");
}

SCM_DEFINE(gwwm_toggleview, "toggleview", 1, 0, 0, (SCM ui), "") {
  PRINT_FUNCTION;
  unsigned int newtagset =
    (current_monitor())
    ->tagset[(scm_to_int(scm_slot_ref(REF_CALL_0("gwwm monitor", "current-monitor"),
                                      scm_from_utf8_symbol("seltags"))))] ^
    ((1 << (scm_to_int(ui))) & TAGMASK);

  if (newtagset) {
    (current_monitor())->tagset[(scm_to_int(scm_slot_ref(REF_CALL_0("gwwm monitor", "current-monitor"),
                                                         scm_from_utf8_symbol("seltags"))))] = newtagset;
    REF_CALL_2("gwwm", "focusclient",
               REF_CALL_1("gwwm commands", "focustop",
                          REF_CALL_0("gwwm monitor", "current-monitor")),
               SCM_BOOL_T);
    REF_CALL_1("gwwm commands", "arrange", REF_CALL_0("gwwm monitor", "current-monitor"));
  }
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (gwwm_view, "view",1,0,0,(SCM ui),""){
  int n=(1 << (scm_to_int(ui)));
  PRINT_FUNCTION;
  if ((n & TAGMASK) ==
      current_monitor()->tagset[(scm_to_int(scm_slot_ref(REF_CALL_0("gwwm monitor", "current-monitor"),
                                      scm_from_utf8_symbol("seltags"))))])
    return SCM_UNSPECIFIED;
  (scm_slot_set_x(REF_CALL_0("gwwm monitor", "current-monitor"),
                  scm_from_utf8_symbol("seltags"),
                  (scm_from_int(scm_to_int(scm_slot_ref(REF_CALL_0("gwwm monitor", "current-monitor"),
                                                        scm_from_utf8_symbol("seltags")))  ^ 1))));
  if (n & TAGMASK)
    (current_monitor())->tagset[(scm_to_int(scm_slot_ref(REF_CALL_0("gwwm monitor", "current-monitor"),
                                      scm_from_utf8_symbol("seltags"))))] = n & TAGMASK;
  REF_CALL_2("gwwm","focusclient",
             REF_CALL_1("gwwm commands", "focustop",
                        (REF_CALL_0("gwwm monitor", "current-monitor"))),
             SCM_BOOL_T);
  REF_CALL_1("gwwm commands","arrange",(WRAP_MONITOR(current_monitor())));
  return SCM_UNSPECIFIED;
}

void
virtualkeyboard(struct wl_listener *listener, void *data)
{
  PRINT_FUNCTION
	struct wlr_virtual_keyboard_v1 *keyboard = data;
	struct wlr_input_device *device = &keyboard->input_device;
    scm_call_1(REFP("gwwm","create-keyboard"), WRAP_WLR_INPUT_DEVICE(device));
}

SCM_DEFINE (gwwm_monitor_tagset, "%monitor-tagset",1, 0,0,
            (SCM m) ,
            "return M's tagset.")
#define FUNC_NAME s_gwwm_monitor_tagset
{
  Monitor *rm=UNWRAP_MONITOR(m);
  SCM a= scm_make_list(scm_from_int(0), SCM_UNSPECIFIED);
  for (size_t i = 0; i < LENGTH((rm)->tagset); i++)
    {
      a=scm_cons(scm_from_unsigned_integer((rm)->tagset[i]),a);
    };
  return a;
}
#undef FUNC_NAME

struct wlr_scene_node *
xytonode(double x, double y, struct wlr_surface **psurface,
		Client **pc, Client **pl, double *nx, double *ny)
{
  PRINT_FUNCTION
	struct wlr_scene_node *node, *pnode;
	struct wlr_surface *surface = NULL;
	SCM c = NULL;
	SCM l = NULL;
	char* focus_order[] = {"overlay-layer",
                          "top-layer",
                          "float-layer",
                          "fullscreen-layer",
                          "tile-layer",
                          "bottom-layer",
                          "background-layer"};

	for (int layer = 0; layer < 6; layer++) {
		if ((node = wlr_scene_node_at(UNWRAP_WLR_SCENE_NODE(REF("gwwm",focus_order[layer])), x, y, nx, ny))) {
			if (node->type == WLR_SCENE_NODE_SURFACE)
				surface = wlr_scene_surface_from_node(node)->surface;
			/* Walk the tree to find a node that knows the client */
			for (pnode = node; pnode && !c; pnode = pnode->parent)
				c = pnode->data;
			if (c && CLIENT_IS_LAYER_SHELL(c)) {
				c = NULL;
				l = pnode->data;
			}
		}
		if (surface)
			break;
	}

	if (psurface) *psurface = surface;
	if (pc && c) *pc = UNWRAP_CLIENT(c);
	if (pl && l) *pl = UNWRAP_CLIENT(l);
	return node;
}

#ifdef XWAYLAND

SCM_DEFINE(createnotifyx11,"create-notify/x11",2,0,0,(SCM sl ,SCM d),"")
{
  PRINT_FUNCTION;
  Client *c = scm_gc_calloc(sizeof(*c),"x-client");
  register_client(c,GWWM_X_CLIENT_TYPE);
  return WRAP_CLIENT(c);
}


Atom
getatom(xcb_connection_t *xc, const char *name)
{
  PRINT_FUNCTION
	Atom atom = 0;
	xcb_intern_atom_reply_t *reply;
	xcb_intern_atom_cookie_t cookie = xcb_intern_atom(xc, 0, strlen(name), name);
	if ((reply = xcb_intern_atom_reply(xc, cookie, NULL)))
		atom = reply->atom;
	free(reply);

	return atom;
}

void
xwaylandready(struct wl_listener *listener,void *data)
{
  PRINT_FUNCTION;
  struct wlr_xcursor *xcursor;
	xcb_connection_t *xc = xcb_connect(gwwm_xwayland(NULL)->display_name, NULL);
	int err = xcb_connection_has_error(xc);
	if (err) {
		fprintf(stderr, "xcb_connect to X server failed with code %d\n. Continuing with degraded functionality.\n", err);
		return;
	}

	/* Collect atoms we are interested in.  If getatom returns 0, we will
	 * not detect that window type. */
	netatom[NetWMWindowTypeDialog] = getatom(xc, "_NET_WM_WINDOW_TYPE_DIALOG");
	netatom[NetWMWindowTypeSplash] = getatom(xc, "_NET_WM_WINDOW_TYPE_SPLASH");
	netatom[NetWMWindowTypeToolbar] = getatom(xc, "_NET_WM_WINDOW_TYPE_TOOLBAR");
	netatom[NetWMWindowTypeUtility] = getatom(xc, "_NET_WM_WINDOW_TYPE_UTILITY");

	/* assign the one and only seat */
	wlr_xwayland_set_seat(gwwm_xwayland(NULL), gwwm_seat(NULL));

	/* Set the default XWayland cursor to match the rest of dwl. */
	if ((xcursor = wlr_xcursor_manager_get_xcursor(gwwm_xcursor_manager(NULL), GWWM_CURSOR_NORMAL_IMAGE(), 1)))
		wlr_xwayland_set_cursor(gwwm_xwayland(NULL),
				xcursor->images[0]->buffer, xcursor->images[0]->width * 4,
				xcursor->images[0]->width, xcursor->images[0]->height,
				xcursor->images[0]->hotspot_x, xcursor->images[0]->hotspot_y);

	xcb_disconnect(xc);
    return;
}
#endif

SCM_DEFINE (config_setup,"%config-setup" ,0,0,0,(),"")
{
  gwwm_config = (REF_CALL_0("gwwm config","load-init-file"));
  return SCM_UNSPECIFIED;
}


void
scm_init_gwwm(void)
{
#define define_listener(name,scm_name,func) struct wl_listener *name=   \
    scm_gc_calloc(sizeof(struct wl_listener),                           \
                  "wl_listener");                                       \
  name->notify = func;                                                  \
  scm_c_define(scm_name, (WRAP_WL_LISTENER(name)));
  define_listener(xwayland_ready,"xwaylandready",xwaylandready);
#undef define_listener
#ifndef SCM_MAGIC_SNARFER
#include "gwwm.x"
#endif
}
