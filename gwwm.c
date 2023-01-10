/*
 * See LICENSE file for copyright and license details.
 */
#include "libguile/boolean.h"
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
#include "listener.h"

#ifdef XWAYLAND
#include <X11/Xlib.h>
#include <wlr/xwayland.h>
#endif
#include "gwwm.h"
#include "client.h"
/* configuration, allows nested code to access above variables */
#include "config.h"
typedef struct Monitor {
  struct wl_list link;
  struct wl_list layers[4]; /* layer Client::link */
  unsigned int seltags;
  unsigned int tagset[2];
  double mfact;
  int nmaster;
  int un_map; /* If a map/unmap happened on this monitor, then this should be
                 true */
  SCM scm;
} Monitor;

Monitor* monitor_from_listener(struct wl_listener *listener) {
  /* PRINT_FUNCTION; */
  SCM scm = scm_from_listener(WRAP_WL_LISTENER(listener));
  return scm_is_false(scm) ? NULL : UNWRAP_MONITOR(scm);
}
 const char broken[] = "broken";
 struct wl_list clients; /* tiling order */
 struct wlr_idle_inhibit_manager_v1 *idle_inhibit_mgr;
 struct wlr_input_inhibit_manager *input_inhibit_mgr;
 struct wlr_virtual_keyboard_manager_v1 *virtual_keyboard_mgr;
 Atom netatom[NetLast];
 Atom get_netatom_n(int n){
   return netatom[n];
 };
 struct wl_list keyboards;
 unsigned int cursor_mode;
 Client *grabc;
 int grabcx, grabcy; /* client-relative */

SCM gwwm_config;
SCM get_gwwm_config(void) {
  return gwwm_config;
}

struct wl_list mons;

struct wl_listener idle_inhibitor_create = {.notify = createidleinhibitor};
struct wl_listener idle_inhibitor_destroy = {.notify = destroyidleinhibitor};
struct wl_listener new_virtual_keyboard = {.notify = virtualkeyboard};

bool visibleon(Client *c, Monitor *m) {
  return ((m) && (client_monitor(c, NULL) == (m)) &&
          (client_tags(c) & (m)->tagset[(m)->seltags]));
}

SCM_DEFINE_PUBLIC(gwwm_visibleon, "visibleon", 2, 0, 0, (SCM c, SCM m), "")
#define FUNC_NAME s_gwwm_visibleon
{
  GWWM_ASSERT_CLIENT_OR_FALSE(c ,1);
  PRINT_FUNCTION
  Client *s =(UNWRAP_CLIENT(c));
  bool a = (visibleon(s,(struct Monitor*)(UNWRAP_MONITOR(m))));
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

void
applyrules(Client *c)
{
  PRINT_FUNCTION
	/* rule matching */
	const char *appid, *title;
	unsigned int i, newtags = 0;
	const Rule *r;
	Monitor *mon = current_monitor(), *m;

    CLIENT_SET_FLOATING(c,client_is_float_type(c));
	if (!(appid = client_get_appid(c)))
		appid = broken;
	if (!(title = client_get_title(c)))
		title = broken;

	for (r = rules; r < END(rules); r++) {
		if ((!r->title || strstr(title, r->title))
				&& (!r->id || strstr(appid, r->id))) {
          CLIENT_SET_FLOATING(c,scm_from_bool(r->isfloating));
			newtags |= r->tags;
			i = 0;
			wl_list_for_each(m, &mons, link)
				if (r->monitor == i++)
					mon = m;
		}
	}
	wlr_scene_node_reparent(CLIENT_SCENE(c),
                            UNWRAP_WLR_SCENE_NODE(REF("gwwm",CLIENT_IS_FLOATING(c)
                                                      ? "float-layer"
                                                      : "tile-layer")));
	setmon(c, mon, newtags);
}

void
arrange(Monitor *m)
{
  REF_CALL_1("gwwm commands","arrange",(WRAP_MONITOR(m)));
}

void arrange_l(Client *layersurface,Monitor *m, struct wlr_box *usable_area, int exclusive) {

  struct wlr_box *full_area = MONITOR_AREA(m);
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

void
arrangelayer(Monitor *m, struct wl_list *list, struct wlr_box *usable_area, int exclusive)
{
  PRINT_FUNCTION
	Client *layersurface;

	wl_list_for_each(layersurface, list, link) {
      arrange_l(layersurface, m, usable_area,exclusive);
    }
}

void arrange_interactive_layer(Monitor *m) {
  Client *layersurface;
  uint32_t layers_above_shell[] = {
    ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY,
    ZWLR_LAYER_SHELL_V1_LAYER_TOP,
  };
  for (size_t i = 0; i < LENGTH(layers_above_shell); i++) {
    wl_list_for_each_reverse(layersurface, &m->layers[layers_above_shell[i]],
                             link) {
      struct wlr_surface *surface = CLIENT_SURFACE(layersurface);
      struct wlr_layer_surface_v1 *lsurface =
        TO_P(REF_CALL_1("wlroots types","get-pointer",
                        scm_slot_ref(WRAP_CLIENT(layersurface),
                                     scm_from_utf8_symbol("super-surface"))));
      if (lsurface->current.keyboard_interactive ==
          ZWLR_LAYER_SURFACE_V1_KEYBOARD_INTERACTIVITY_EXCLUSIVE) {
        /* Deactivate the focused client. */
        focusclient(NULL, 0);

        exclusive_focus(WRAP_WLR_SURFACE(surface));
        client_notify_enter(exclusive_focus(NULL), wlr_seat_get_keyboard(gwwm_seat(NULL)));
        return;
      }
    }
  }
}

SCM_DEFINE(arrangelayers,"arrangelayers",1,0,0,(SCM sm),"")
{
  Monitor *m=UNWRAP_MONITOR(sm);
  PRINT_FUNCTION
	int i;
	struct wlr_box usable_area = *MONITOR_AREA(m);

	/* Arrange exclusive surfaces from top->bottom */
	for (i = 3; i >= 0; i--)
		arrangelayer(m, &m->layers[i], &usable_area, 1);

	if (memcmp(&usable_area, MONITOR_WINDOW_AREA(m), sizeof(struct wlr_box))) {
      (SET_MONITOR_WINDOW_AREA(m, &usable_area));
		arrange(m);
	}

	/* Arrange non-exlusive surfaces from top->bottom */
	for (i = 3; i >= 0; i--)
		arrangelayer(m, &m->layers[i], &usable_area, 0);

	/* Find topmost keyboard interactive layer, if such a layer exists */
    arrange_interactive_layer(m);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (buttonpress,"buttonpress",2,0,0,(SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
  struct wlr_event_pointer_button *event = data;
    struct wlr_cursor *cursor=gwwm_cursor(NULL);
	struct wlr_keyboard *keyboard;
	uint32_t mods;
	Client *c;
	const Button *b;
	switch (event->state) {
	case WLR_BUTTON_PRESSED:
		/* Change focus if the button was _pressed_ over a client */
      xytonode(cursor->x, cursor->y, NULL, &c, NULL, NULL, NULL);
		/* Don't focus unmanaged clients */
		if (c && !client_is_unmanaged(c))
			focusclient(c, 1);

		keyboard = wlr_seat_get_keyboard(gwwm_seat(NULL));
		mods = keyboard ? wlr_keyboard_get_modifiers(keyboard) : 0;
		for (b = buttons; b < END(buttons); b++) {
			if (CLEANMASK(mods) == CLEANMASK(b->mod) &&
					event->button == b->button && b->func) {
				b->func(&b->arg);
				return SCM_UNSPECIFIED;
			}
		}
		break;
	case WLR_BUTTON_RELEASED:
		/* If you released any buttons, we exit interactive move/resize mode. */
		/* TODO should reset to the pointer focus's current setcursor */
		if (cursor_mode != CurNormal) {
          if (cursor_mode == CurResize &&
              xytonode(cursor->x, cursor->y, NULL, &c, NULL, NULL, NULL) &&
              (c && !client_is_unmanaged(c)))
            {
              client_set_resizing(c,0);
            }
          wlr_xcursor_manager_set_cursor_image(gwwm_xcursor_manager(NULL), GWWM_CURSOR_NORMAL_IMAGE(), cursor);
			cursor_mode = CurNormal;
			/* Drop the window off on its new monitor */
		    set_current_monitor(xytomon(cursor->x, cursor->y));
			setmon(grabc, current_monitor(), 0);
			return SCM_UNSPECIFIED;
		}
		break;
	}
	/* If the event wasn't handled by the compositor, notify the client with
	 * pointer focus that a button press has occurred */
	wlr_seat_pointer_notify_button(gwwm_seat(NULL),
			event->time_msec, event->button, event->state);
    return SCM_UNSPECIFIED;
}

void
checkidleinhibitor(struct wlr_surface *exclude)
{
  PRINT_FUNCTION
	Client *c, *w;
	int inhibited = 0;
	struct wlr_idle_inhibitor_v1 *inhibitor;
	wl_list_for_each(inhibitor, &idle_inhibit_mgr->inhibitors, link) {
		c = client_from_wlr_surface(inhibitor->surface);
		if (exclude && (!(w = client_from_wlr_surface(exclude)) || w == c))
			continue;
		if (!c || visibleon(c, client_monitor(c ,NULL))) {
			inhibited = 1;
			break;
		}
	}

	wlr_idle_set_enabled(gwwm_idle(NULL), NULL, !inhibited);
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

SCM_DEFINE (cleanupmon,"cleanup-monitor",3,0,0,(SCM sm, SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
  struct wlr_output *wlr_output = data;
  Monitor *m = UNWRAP_MONITOR(sm);
  int nmons, i = 0;

  wl_list_remove(&m->link);
  wlr_scene_output_destroy(monitor_scene_output(m,NULL));

  if ((nmons = wl_list_length(&mons)))
    do /* don't switch to disabled mons */
      set_current_monitor(
                          wl_container_of(mons.prev, (current_monitor()), link));
    while (!(MONITOR_WLR_OUTPUT(current_monitor()))->enabled && i++ < nmons);
  logout_monitor(sm);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (commitlayersurfacenotify,"commit-layer-client-notify",3,0,0,
            (SCM c,SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
  Client *layersurface = UNWRAP_CLIENT(c);
	struct wlr_layer_surface_v1 *wlr_layer_surface = wlr_layer_surface_v1_from_wlr_surface(CLIENT_SURFACE(layersurface));
    Monitor *m=client_monitor(layersurface,NULL);
	if (!m)
		return SCM_UNSPECIFIED;

	if (return_scene_node(wlr_layer_surface->current.layer) != CLIENT_SCENE(layersurface)) {
		wlr_scene_node_reparent(CLIENT_SCENE(layersurface),
                                return_scene_node(wlr_layer_surface->current.layer));
		wl_list_remove(&layersurface->link);
		wl_list_insert(&m->layers[wlr_layer_surface->current.layer],
				&layersurface->link);
	}

    if (wlr_layer_surface->current.committed == 0) return SCM_UNSPECIFIED;
	arrangelayers(WRAP_MONITOR(m));
    return SCM_UNSPECIFIED;
}

void
createidleinhibitor(struct wl_listener *listener, void *data)
{
  PRINT_FUNCTION;
	struct wlr_idle_inhibitor_v1 *idle_inhibitor = data;
	wl_signal_add(&idle_inhibitor->events.destroy, &idle_inhibitor_destroy);

	checkidleinhibitor(NULL);
}

SCM_DEFINE(createkeyboard,"%create-keyboard",1,0,0,(SCM sdevice),"")
{
  struct wlr_input_device *device=UNWRAP_WLR_INPUT_DEVICE(sdevice);
  PRINT_FUNCTION;
	struct xkb_context *context;
	struct xkb_keymap *keymap;
    SCM kb=scm_make(scm_list_3(REFP("gwwm keyboard", "<gwwm-keyboard>"),
                               scm_from_utf8_keyword("device"),
                               WRAP_WLR_INPUT_DEVICE(device)));
    scm_c_run_hook(REF("gwwm hooks", "create-keyboard-hook"),
                   scm_list_1(kb));
    SCM s_xkb = REF_CALL_1("gwwm config","config-xkb-rules", gwwm_config);
   #define rf(name) (scm_to_utf8_string(scm_slot_ref(s_xkb, scm_from_utf8_symbol(name))))
    const struct xkb_rule_names xr = {
    .rules = rf("rules"),
      .model = rf("model"),
      .layout = rf("layout"),
      .variant = rf("variant"),
      .options = rf("options")
      };
#undef rf
	/* Prepare an XKB keymap and assign it to the keyboard. */
	context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
	keymap = xkb_keymap_new_from_names(context, &xr,
		XKB_KEYMAP_COMPILE_NO_FLAGS);

	wlr_keyboard_set_keymap(device->keyboard, keymap);
	xkb_keymap_unref(keymap);
	xkb_context_unref(context);
	wlr_keyboard_set_repeat_info(device->keyboard, (scm_to_int32(REF_CALL_1("gwwm config","config-repeat-rate", gwwm_config))), repeat_delay);
    return kb;
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
    CLIENT_SET_SURFACE(layersurface,wlr_layer_surface->surface);
    scm_slot_set_x(WRAP_CLIENT(layersurface),
                   scm_from_utf8_symbol("super-surface"),
                   WRAP_WLR_LAYER_SURFACE(wlr_layer_surface));
    Monitor *m=wlr_layer_surface->output->data;
    client_monitor(layersurface,m);
	wlr_layer_surface->data = WRAP_CLIENT(layersurface);

	CLIENT_SET_SCENE(layersurface,(wlr_layer_surface->surface->data =
                                   wlr_scene_subsurface_tree_create(return_scene_node(wlr_layer_surface->pending.layer),
			wlr_layer_surface->surface)));
	CLIENT_SCENE(layersurface)->data = layersurface;
	wl_list_insert(&m->layers[wlr_layer_surface->pending.layer],
			&layersurface->link);

	/* Temporarily set the layer's current state to pending
	 * so that we can easily arrange it
	 */
	old_state = wlr_layer_surface->current;
	wlr_layer_surface->current = wlr_layer_surface->pending;
	arrangelayers(WRAP_MONITOR(m));
	wlr_layer_surface->current = old_state;

    scm_c_run_hook(REF("gwwm hooks", "create-client-hook"),
                 scm_list_1(WRAP_CLIENT(layersurface)));
    return SCM_UNSPECIFIED;
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
  for (size_t i = 0; i < LENGTH(m->layers); i++)
		wl_list_init(&m->layers[i]);
	m->tagset[0] = m->tagset[1] = 2;
	for (r = monrules; r < END(monrules); r++) {
		if (!r->name || strstr(wlr_output->name, r->name)) {
			m->mfact = r->mfact;
			m->nmaster = r->nmaster;
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
	SET_MONITOR_WLR_OUTPUT(m,wlr_output);
	wlr_output_init_render(wlr_output, gwwm_allocator(NULL), gwwm_renderer(NULL));
	/* Initialize monitor state using configured rules */
	init_monitor(wlr_output);
	/* The mode is a tuple of (width, height, refresh rate), and each
	 * monitor supports only a specific set of modes. We just pick the
	 * monitor's preferred mode; a more sophisticated compositor would let
	 * the user configure it. */
    scm_c_run_hook(REF("gwwm hooks", "create-monitor-hook"), scm_list_1(WRAP_MONITOR(m)));
    if (wlr_output_is_wl(wlr_output)) {
      wlr_wl_output_set_title(wlr_output, "gwwm");
    } else if (wlr_output_is_x11(wlr_output)) {
      wlr_x11_output_set_title(wlr_output, "gwwm");
    }
    if (!wlr_output_commit(wlr_output))
      return SCM_UNSPECIFIED;

    REF_CALL_2("ice-9 q", "q-push!", REF_CALL_0("gwwm monitor", "%monitors"),WRAP_MONITOR(m));
	wl_list_insert(&mons, &m->link);

	/* Adds this to the output layout in the order it was configured in.
	 *
	 * The output layout utility automatically adds a wl_output global to the
	 * display, which Wayland clients can see to find out information about the
	 * output (such as DPI, scale factor, manufacturer, etc).
	 */
    monitor_scene_output(m , wlr_scene_output_create(gwwm_scene(NULL), wlr_output));
	wlr_output_layout_add_auto(gwwm_output_layout(NULL), wlr_output);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(gwwm_new_popup_notify,"new-popup-notify",2,0,0,(SCM sl ,SCM d),"")
{
  struct wl_listener *listener=UNWRAP_WL_LISTENER(sl);
  void *data= TO_P(d);
  PRINT_FUNCTION;
  struct wlr_xdg_popup *popup = data;
  struct wlr_box box;
  Client *l = toplevel_from_popup(popup);
  struct wlr_scene_node *node=wlr_scene_xdg_surface_create(popup->parent->data,
                                                          popup->base);
  popup->base->surface->data=node;
  if (!l || !client_monitor(l,NULL))
    return SCM_UNSPECIFIED;
  if (!CLIENT_IS_FLOATING(l)) (wlr_scene_node_raise_to_top(node->parent));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(createnotify,"create-notify",2,0,0,(SCM sl ,SCM d),"")
{
  /* This event is raised when wlr_xdg_shell receives a new xdg surface from a
   * client, either a toplevel (application window) or popup,
   * or when wlr_layer_shell receives a new popup from a layer.
   * If you want to do something tricky with popups you should check if
   * its parent is wlr_xdg_shell or wlr_layer_shell */
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(sl);
  void *data= TO_P(d);

	struct wlr_xdg_surface *xdg_surface = data;
	Client *c;

	if (xdg_surface->role == WLR_XDG_SURFACE_ROLE_POPUP ||
        xdg_surface->role == WLR_XDG_SURFACE_ROLE_NONE) {
      return SCM_UNSPECIFIED;
	}

	/* Allocate a Client for this surface */
	c = scm_gc_calloc(sizeof(*c), "xdg-client");
    register_client(c,GWWM_XDG_CLIENT_TYPE);
    xdg_surface->data = WRAP_CLIENT(c);
    CLIENT_SET_SURFACE(c ,xdg_surface->surface);
    scm_slot_set_x(WRAP_CLIENT(c),
                   scm_from_utf8_symbol("super-surface"),
                   WRAP_WLR_XDG_SURFACE(xdg_surface));
    CLIENT_SET_BW(c,GWWM_BORDERPX());

    scm_c_run_hook(REF("gwwm hooks", "create-client-hook"),
                 scm_list_1(WRAP_CLIENT(c)));
    return SCM_UNSPECIFIED;
}


SCM_DEFINE(createpointer,"create-pointer",1,0,0,(SCM sdevice),"")
{
  PRINT_FUNCTION;
  struct wlr_input_device *device=UNWRAP_WLR_INPUT_DEVICE(sdevice);

  scm_c_run_hook(REF("gwwm hooks", "create-pointer-hook"),
                 scm_list_1(WRAP_WLR_INPUT_DEVICE(device)));
	if (wlr_input_device_is_libinput(device)) {
		struct libinput_device *libinput_device =  (struct libinput_device*)
			wlr_libinput_get_device_handle(device);

		if (libinput_device_config_tap_get_finger_count(libinput_device)) {
			libinput_device_config_tap_set_enabled(libinput_device, tap_to_click);
			libinput_device_config_tap_set_drag_enabled(libinput_device, tap_and_drag);
			libinput_device_config_tap_set_drag_lock_enabled(libinput_device, drag_lock);
		}

		if (libinput_device_config_scroll_has_natural_scroll(libinput_device))
			libinput_device_config_scroll_set_natural_scroll_enabled(libinput_device, natural_scrolling);

		if (libinput_device_config_dwt_is_available(libinput_device))
			libinput_device_config_dwt_set_enabled(libinput_device, disable_while_typing);

		if (libinput_device_config_left_handed_is_available(libinput_device))
			libinput_device_config_left_handed_set(libinput_device, left_handed);

		if (libinput_device_config_middle_emulation_is_available(libinput_device))
			libinput_device_config_middle_emulation_set_enabled(libinput_device, middle_button_emulation);

		if (libinput_device_config_scroll_get_methods(libinput_device)
            != LIBINPUT_CONFIG_SCROLL_NO_SCROLL)
          libinput_device_config_scroll_set_method (libinput_device, scroll_method);
		
		 if (libinput_device_config_click_get_methods(libinput_device)
             != LIBINPUT_CONFIG_CLICK_METHOD_NONE)
                        libinput_device_config_click_set_method (libinput_device, click_method);

		if (libinput_device_config_send_events_get_modes(libinput_device))
			libinput_device_config_send_events_set_mode(libinput_device, send_events_mode);

		if (libinput_device_config_accel_is_available(libinput_device)) {
			libinput_device_config_accel_set_profile(libinput_device, accel_profile);
			libinput_device_config_accel_set_speed(libinput_device, accel_speed);
		}
	}

	wlr_cursor_attach_input_device(gwwm_cursor(NULL), device);
    return SCM_UNSPECIFIED;
}

void
destroyidleinhibitor(struct wl_listener *listener, void *data)
{
  PRINT_FUNCTION;
	/* `data` is the wlr_surface of the idle inhibitor being destroyed,
	 * at this point the idle inhibitor is still in the list of the manager */
	checkidleinhibitor(data);
}

SCM_DEFINE (destroylayersurfacenotify,"destroy-layer-client-notify",3,0,0,(SCM c,SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
  Client *layersurface = UNWRAP_CLIENT(c);
  wl_list_remove(&layersurface->link);
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

void
focusclient(Client *c, int lift)
{
  PRINT_FUNCTION;
  struct wlr_surface *old = gwwm_seat(NULL)->keyboard_state.focused_surface;
  SCM sc=WRAP_CLIENT(c);
	/* Do not focus clients if a layer surface is focused */
  if (exclusive_focus(NULL))
		return;

	/* Raise client in stacking order if requested */
	if (c && lift)
		wlr_scene_node_raise_to_top(CLIENT_SCENE(c));

    if (c && CLIENT_SURFACE(c) == old)
		return;
	/* Put the new client atop the focus stack and select its monitor */
    if (c && !(CLIENT_IS_LAYER_SHELL(sc))) {
      REF_CALL_2("ice-9 q", "q-remove!", REF_CALL_0("gwwm client", "%fstack"), sc);
      REF_CALL_2("ice-9 q", "q-push!", REF_CALL_0("gwwm client", "%fstack"), sc);
      set_current_monitor(client_monitor(c,NULL));
        CLIENT_SET_URGENT(c ,0);
		client_restack_surface(c);

        CLIENT_SET_BORDER_COLOR(c ,REF_CALL_1("gwwm config", "config-focuscolor", gwwm_config));
	}

	/* Deactivate old client if focus is changing */
	if (old && (!c || CLIENT_SURFACE(c) != old)) {
		/* If an overlay is focused, don't focus or activate the client,
		 * but only update its position in fstack to render its border with focuscolor
		 * and focus it after the overlay is closed.
		 * It's probably pointless to check if old is a layer surface
		 * since it can't be anything else at this point. */
		if (wlr_surface_is_layer_surface(old)) {
			struct wlr_layer_surface_v1 *wlr_layer_surface =
				wlr_layer_surface_v1_from_wlr_surface(old);

			if (wlr_layer_surface->mapped && (
						wlr_layer_surface->current.layer == ZWLR_LAYER_SHELL_V1_LAYER_TOP ||
						wlr_layer_surface->current.layer == ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY
						))
				return;
		} else {
			Client *w;
			if ((w = client_from_wlr_surface(old)))
              {CLIENT_SET_BORDER_COLOR(w,REF_CALL_1("gwwm config", "config-bordercolor", gwwm_config));};

			client_activate_surface(old, 0);
		}
	}

	checkidleinhibitor(NULL);
	if (!c) {
		/* With no client, all we have left is to clear focus */
		wlr_seat_keyboard_notify_clear_focus(gwwm_seat(NULL));
		return;
	}

	/* Have a client, so focus its top-level wlr_surface */
	client_notify_enter(CLIENT_SURFACE(c), wlr_seat_get_keyboard(gwwm_seat(NULL)));

	/* Activate the new client */
	client_activate_surface(CLIENT_SURFACE(c), 1);
}

SCM_DEFINE (gwwm_focusclient, "focusclient" ,2,0,0,(SCM client,SCM lift),"")
#define FUNC_NAME s_gwwm_focusclient
{
  GWWM_ASSERT_CLIENT_OR_FALSE(client ,1);
  Client *c= UNWRAP_CLIENT(client);
  focusclient(c, scm_to_bool(lift));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_focusmon ,"focusmon",1,0,0,(SCM a),"" )
#define FUNC_NAME s_gwwm_focusmon
{
  PRINT_FUNCTION;
  int i = 0, nmons = wl_list_length(&mons);
  if (nmons)
    do /* don't switch to disabled mons */
      set_current_monitor(dirtomon(scm_to_int(a)));
    while (!MONITOR_WLR_OUTPUT(current_monitor())->enabled && i++ < nmons);
  focusclient(focustop(current_monitor()), 1);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

Client *
focustop(Monitor *m)
{
  PRINT_FUNCTION;
  SCM c=REF_CALL_1("gwwm commands", "focustop", WRAP_MONITOR(m));
  return UNWRAP_CLIENT(c);
}

void
fullscreennotify_x11(struct wl_listener *listener, void *data)
{
  PRINT_FUNCTION;
  struct wlr_xwayland_surface *surface=data;
  if (surface->surface) {
    Client *c = client_from_wlr_surface(surface->surface);
    if (c)
      {
        scm_c_run_hook(REF("gwwm hooks", "fullscreen-event-hook"),
                       scm_list_2(WRAP_CLIENT(c),WRAP_WLR_XWAYLAND_SURFACE(data)));
      }
  }

}

void
incnmaster(const Arg *arg)
{
  PRINT_FUNCTION
  (current_monitor())->nmaster = MAX((current_monitor())->nmaster + arg->i, 0);
  arrange(current_monitor());
}

SCM_DEFINE(inputdevice,"inputdevice",2,0,0,(SCM sl ,SCM d),"")
{
  PRINT_FUNCTION
  struct wl_listener *listener=UNWRAP_WL_LISTENER(sl);
  void *data= TO_P(d);
	/* This event is raised by the backend when a new input device becomes
	 * available. */
	struct wlr_input_device *device = data;
	uint32_t caps;
	/* We need to let the wlr_seat know what our capabilities are, which is
	 * communiciated to the client. In dwl we always have a cursor, even if
	 * there are no pointer devices, so we always include that capability. */
	/* TODO do we actually require a cursor? */
	caps = WL_SEAT_CAPABILITY_POINTER;
	if (!scm_to_bool(scm_zero_p(scm_length(REF_CALL_0("gwwm keyboard", "keyboard-list")))))
		caps |= WL_SEAT_CAPABILITY_KEYBOARD;
	wlr_seat_set_capabilities(gwwm_seat(NULL), caps);
    return SCM_UNSPECIFIED;
}

bool
keybinding(uint32_t mods, xkb_keycode_t keycode)
{
  PRINT_FUNCTION
	/*
	 * Here we handle compositor keybindings. This is when the compositor is
	 * processing keys, rather than passing them on to the client for its own
	 * processing.
	 */
  return scm_to_bool(scm_call_2(scm_c_private_ref("gwwm keybind", "keybinding"),
                                scm_from_uint32(mods),
                                scm_from_uint32(keycode)));
}

SCM_DEFINE (keypress,"keypress",3,0,0,(SCM kb, SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
	/* This event is raised when a key is pressed or released. */
	struct wlr_event_keyboard_key *event = TO_P(sdata);
  /* Translate libinput keycode -> xkbcommon */
	uint32_t keycode = event->keycode + 8;
	int handled = 0;
	uint32_t mods = wlr_keyboard_get_modifiers
      ((UNWRAP_WLR_INPUT_DEVICE
        (scm_slot_ref(kb, scm_from_utf8_symbol("device"))))->keyboard);
	/* On _press_ if there is no active screen locker,
	 * attempt to process a compositor keybinding. */
	if (!input_inhibit_mgr->active_inhibitor
			&& event->state == WL_KEYBOARD_KEY_STATE_PRESSED)
      handled=keybinding(mods, keycode);

	if (!handled) {
		/* Pass unhandled keycodes along to the client. */
		wlr_seat_set_keyboard(gwwm_seat(NULL),
                              (UNWRAP_WLR_INPUT_DEVICE
                               (scm_slot_ref(kb, scm_from_utf8_symbol("device")))));
		wlr_seat_keyboard_notify_key(gwwm_seat(NULL), event->time_msec,
			event->keycode, event->state);
	}
    return SCM_UNSPECIFIED;
}

SCM_DEFINE (destroy_surface_notify,"destroy-surface-notify",3,0,0,
            (SCM c, SCM listener, SCM data),"")
{
  PRINT_FUNCTION;
  logout_client(UNWRAP_CLIENT(c));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(mapnotify,"map-notify",3,0,0,(SCM sc,SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);

  /* Called when the surface is mapped, or ready to display on-screen. */
  Client *p, *c = UNWRAP_CLIENT(sc);
  /* Create scene tree for this client and its border */
  struct wlr_surface *surface = (CLIENT_SURFACE(c));
  struct wlr_scene_node *scene_node = CLIENT_SCENE(c);

  (surface)->data = scene_node;

  scene_node->data = client_scene_surface(c, NULL)->data = c;
  if (client_is_unmanaged(c)) {
    /* Floating */
    wlr_scene_node_reparent(scene_node, UNWRAP_WLR_SCENE_NODE(REF("gwwm","float-layer")));
    wlr_scene_node_set_position(scene_node, client_geom(c)->x + GWWM_BORDERPX(),
                                client_geom(c)->y + GWWM_BORDERPX());
    return SCM_UNSPECIFIED;
  }
  (scm_call_1(REFP("gwwm client","client-init-border"), WRAP_CLIENT(c)));
  /* Insert this client into client lists. */
  wl_list_insert(&clients, &c->link);
  REF_CALL_2("ice-9 q", "q-push!", REF_CALL_0("gwwm client", "%clients"), sc);
  REF_CALL_2("ice-9 q", "q-push!", REF_CALL_0("gwwm client", "%fstack"), sc);

  /* Set initial monitor, tags, floating status, and focus */
  if ((p = client_get_parent(c))) {
    /* Set the same monitor and tags than its parent */
    CLIENT_SET_FLOATING(c, 1);
    wlr_scene_node_reparent(scene_node, UNWRAP_WLR_SCENE_NODE(REF("gwwm","float-layer")));
    /* TODO recheck if !p->mon is possible with wlroots 0.16.0 */
    setmon(c,
           (client_monitor(p, NULL)) ? client_monitor(p, NULL)
                                     : current_monitor(),
           client_tags(p));
  } else {
    applyrules(c);
  }

  if (CLIENT_IS_FULLSCREEN(c))
    setfullscreen(c, 1);

  client_monitor(c, NULL)->un_map = 1;
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (gwwm_motionnotify, "%motionnotify" , 1,0,0,
            (SCM stime), "")
#define FUNC_NAME s_gwwm_motionnotify
{
  uint32_t time=scm_to_uint32( stime);
  PRINT_FUNCTION
	double sx = 0, sy = 0;
	Client *c = NULL;
	struct wlr_surface *surface = NULL;
	struct wlr_drag_icon *icon;
    struct wlr_cursor *cursor=gwwm_cursor(NULL);

	/* time is 0 in internal calls meant to restore pointer focus. */
	if (time) {
      wlr_idle_notify_activity(gwwm_idle(NULL), gwwm_seat(NULL));

		/* Update current_monitor (even while dragging a window) */
		if (GWWM_SLOPPYFOCUS_P())
			set_current_monitor(xytomon(cursor->x, cursor->y));
	}

    scm_c_run_hook(REF("gwwm hooks", "motion-notify-hook"),
                   scm_list_1(scm_from_uint32(time)));

	if (cursor_mode == CurMove) {

		/* Move the grabbed client to the new position. */
		resize(grabc, (struct wlr_box){
            .x = cursor->x - grabcx,
            .y = cursor->y - grabcy,
			.width = (client_geom(grabc))->width,
            .height = (client_geom(grabc))->height
          },
          1);
		return SCM_UNSPECIFIED;
	} else if (cursor_mode == CurResize) {
      resize(grabc, (struct wlr_box){
          .x = (client_geom(grabc))->x,
          .y = (client_geom(grabc))->y,
          .width = cursor->x - client_geom(grabc)->x,
          .height = cursor->y - client_geom(grabc)->y
        }, 1);
		return SCM_UNSPECIFIED;
	}

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

void
moveresize(const Arg *arg)
{
  PRINT_FUNCTION;
  struct wlr_cursor *cursor=gwwm_cursor(NULL);
  if (cursor_mode != CurNormal)
		return;
	xytonode(cursor->x, cursor->y, NULL, &grabc, NULL, NULL, NULL);
	if (!grabc || client_is_unmanaged(grabc) || CLIENT_IS_FULLSCREEN(grabc))
		return;
    SCM sgrabc= WRAP_CLIENT(grabc);
	/* Float the window and tell motionnotify to grab it */
	CLIENT_SET_FLOATING(grabc,1);
	switch (cursor_mode = arg->ui) {
	case CurMove:
		grabcx = cursor->x - client_geom(grabc)->x;
		grabcy = cursor->y - client_geom(grabc)->y;
		wlr_xcursor_manager_set_cursor_image(gwwm_xcursor_manager(NULL), "fleur", cursor);
        arrange(current_monitor());
		break;
	case CurResize:
      client_set_resizing(grabc,1);
		/* Doesn't work for X11 output - the next absolute motion event
		 * returns the cursor to where it started */
		wlr_cursor_warp_closest(cursor, NULL,
                                client_geom(grabc)->x +
                                client_geom(grabc)->width,
                                client_geom(grabc)->y
                                + client_geom(grabc)->height);
		wlr_xcursor_manager_set_cursor_image(gwwm_xcursor_manager(NULL),
				"bottom_right_corner", cursor);
		break;
	}
}


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
		focusclient(c, 0);

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
quit(const Arg *arg)
{
  REF_CALL_0("gwwm commands","gwwm-quit");
}

void
quitsignal(int signo)
{
	quit(NULL);
}


SCM_DEFINE(rendermon,"render-monitor-notify",3,0,0,(SCM sm,SCM slistener ,SCM sdata),"")
{
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
  /* PRINT_FUNCTION */
  struct wlr_output *wlr_output=data;
  /* This function is called every time an output is ready to display a frame,
   * generally at the output's refresh rate (e.g. 60Hz). */
  Monitor *m = UNWRAP_MONITOR(sm);
	Client *c;
	int skip = 0;
	struct timespec now;

	clock_gettime(CLOCK_MONOTONIC, &now);

	/* Render if no XDG clients have an outstanding resize and are visible on
	 * this monitor. */
	/* Checking m->un_map for every client is not optimal but works */
	wl_list_for_each(c, &clients, link) {
      if ((client_resize_configure_serial(c) && m->un_map) || ((wlr_surface_is_xdg_surface( CLIENT_SURFACE(c)))
				&& (wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->pending.geometry.width !=
				wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->current.geometry.width
				|| wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->pending.geometry.height !=
				wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->current.geometry.height))) {
			/* Lie */
			wlr_surface_send_frame_done(CLIENT_SURFACE(c), &now);
			skip = 1;
		}
	}
	if (!skip && !wlr_scene_output_commit(monitor_scene_output(m,NULL)))
		return SCM_UNSPECIFIED;
	/* Let clients know a frame has been rendered */
	wlr_scene_output_send_frame_done(monitor_scene_output(m,NULL), &now);
	m->un_map = 0;
    return SCM_UNSPECIFIED;
}

void resize(Client *c, struct wlr_box geo, int interact) {
  PRINT_FUNCTION
  REF_CALL_3("gwwm client", "client-resize", WRAP_CLIENT(c), SHALLOW_CLONE(WRAP_WLR_BOX(&geo)),
             scm_from_bool(interact));
}

Client *
current_client(void)
{
  PRINT_FUNCTION;
  SCM c=REF_CALL_0("gwwm client", "current-client");
  return (UNWRAP_CLIENT(c)) ;
}

SCM_DEFINE (setcursor,"setcursor",2,0,0,(SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
  PRINT_FUNCTION;
  /* This event is raised by the seat when a client provides a cursor image */
  struct wlr_seat_pointer_request_set_cursor_event *event = data;
  struct wlr_cursor *cursor=gwwm_cursor(NULL);
	/* If we're "grabbing" the cursor, don't use the client's image */
	/* TODO still need to save the provided surface to restore later */
	if (cursor_mode != CurNormal)
		return SCM_UNSPECIFIED;
	/* This can be sent by any client, so we check to make sure this one is
	 * actually has pointer focus first. If so, we can tell the cursor to
	 * use the provided surface as the cursor image. It will set the
	 * hardware cursor on the output that it's currently on and continue to
	 * do so as the cursor moves between outputs. */
	if (event->seat_client == gwwm_seat(NULL)->pointer_state.focused_client)
		wlr_cursor_set_surface(cursor, event->surface,
				event->hotspot_x, event->hotspot_y);
    return SCM_UNSPECIFIED;
}

void
setfullscreen(Client *c, int fullscreen)
{
  PRINT_FUNCTION;
  REF_CALL_2("gwwm client", "client-do-set-fullscreen", WRAP_CLIENT(c), scm_from_bool(fullscreen));
}

void
setmon(Client *c, Monitor *m, unsigned int newtags)
{
  PRINT_FUNCTION;
  scm_call_3(REFP("gwwm", "setmon"),
             WRAP_CLIENT(c),
             WRAP_MONITOR(m),
             scm_from_unsigned_integer(exp(newtags)));
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

	/* Configure a listener to be notified when new outputs are available on the
	 * backend. */
	wl_list_init(&mons);
	/* Set up our client lists and the xdg-shell. The xdg-shell is a
	 * Wayland protocol which is used for application windows. For more
	 * detail on shells, refer to the article:
	 *
	 * https://drewdevault.com/2018/07/29/Wayland-shells.html
	 */
	wl_list_init(&clients);

	idle_inhibit_mgr = wlr_idle_inhibit_v1_create(gwwm_display(NULL));
	wl_signal_add(&idle_inhibit_mgr->events.new_inhibitor, &idle_inhibitor_create);

	input_inhibit_mgr = wlr_input_inhibit_manager_create(gwwm_display(NULL));

	/* Use decoration protocols to negotiate server-side decorations */
	wlr_server_decoration_manager_set_default_mode(
			wlr_server_decoration_manager_create(gwwm_display(NULL)),
			WLR_SERVER_DECORATION_MANAGER_MODE_SERVER);
	wlr_xdg_decoration_manager_v1_create(gwwm_display(NULL));

    /*
	 * wlr_cursor *only* displays an image on screen. It does not move around
	 * when the pointer moves. However, we can attach input devices to it, and
	 * it will generate aggregate events for all of them. In these events, we
	 * can choose how we want to process them, forwarding them to clients and
	 * moving the cursor around. More detail on this process is described in my
	 * input handling blog post:
	 *
	 * https://drewdevault.com/2018/07/17/Input-handling-in-wlroots.html
	 *
	 * And more comments are sprinkled throughout the notify functions above.
	 */
	/*
	 * Configures a seat, which is a single "seat" at which a user sits and
	 * operates the computer. This conceptually includes up to one keyboard,
	 * pointer, touch, and drawing tablet device. We also rig up a listener to
	 * let us know when new input devices are available on the backend.
	 */
	wl_list_init(&keyboards);
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

void
tagmon(const Arg *arg)
{
  PRINT_FUNCTION
	Client *sel = current_client();
	if (!sel)
		return;
	setmon(sel, dirtomon(arg->i), 0);
}

SCM_DEFINE(gwwm_tile, "%tile", 1, 0, 0, (SCM monitor), "c")
#define FUNC_NAME s_gwwm_tile
{
  struct Monitor *m = UNWRAP_MONITOR(monitor);
  unsigned int i, n = 0, mw, my, ty;
  Client *c;

  wl_list_for_each(c, &clients, link) if (visibleon(c, m) &&
                                          !(CLIENT_IS_FLOATING(c)) &&
                                          !CLIENT_IS_FULLSCREEN(c)) n++;
  if (n == 0)
    return SCM_UNSPECIFIED;

  if (n > m->nmaster)
    mw = m->nmaster ? (MONITOR_WINDOW_AREA(m))->width * m->mfact : 0;
  else
    mw = (MONITOR_WINDOW_AREA(m))->width;
  i = my = ty = 0;
  wl_list_for_each(c, &clients, link) {
    /* SCM sc=WRAP_CLIENT(c); */
    if (!visibleon(c, m) || CLIENT_IS_FLOATING(c) || CLIENT_IS_FULLSCREEN(c))
      continue;
    if (i < m->nmaster) {
      resize(
             c,
             (struct wlr_box){.x = (MONITOR_WINDOW_AREA(m))->x,
                              .y = (MONITOR_WINDOW_AREA(m))->y + my,
                              .width = mw,
                              .height = ((MONITOR_WINDOW_AREA(m))->height - my) /
                              (MIN(n, m->nmaster) - i)},
             0);
      my += client_geom(c)->height;
    } else {
      resize(c,
             (struct wlr_box){
               .x = (MONITOR_WINDOW_AREA(m))->x + mw,
               .y = (MONITOR_WINDOW_AREA(m))->y + ty,
               .width = (MONITOR_WINDOW_AREA(m))->width - mw,
               .height = ((MONITOR_WINDOW_AREA(m))->height - ty) / (n - i)},
             0);
      ty += client_geom(c)->height;
    }
    i++;
  }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
togglefloating(const Arg *arg)
{
  PRINT_FUNCTION;
  REF_CALL_0("gwwm commands" ,"togglefloating");
}

void
toggleview(const Arg *arg)
{
  PRINT_FUNCTION
  unsigned int newtagset = (current_monitor())->tagset[(current_monitor())->seltags] ^ (arg->ui & TAGMASK);

	if (newtagset) {
      (current_monitor())->tagset[(current_monitor())->seltags] = newtagset;
      focusclient(focustop(current_monitor()), 1);
      arrange(current_monitor());
	}
}

SCM_DEFINE (gwwm_toggleview, "toggleview",1,0,0,(SCM ui),""){
  toggleview(&((Arg){.ui=1 << (scm_to_int(ui))}));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(unmapnotify,"unmap-notify",3,0,0,(SCM sc,SCM slistener ,SCM sdata),"")
{
  PRINT_FUNCTION;
  struct wl_listener *listener=UNWRAP_WL_LISTENER(slistener);
  void *data=TO_P(sdata);
  PRINT_FUNCTION
	/* Called when the surface is unmapped, and should no longer be shown. */
	Client *c = UNWRAP_CLIENT(sc);
	if (c == grabc) {
		cursor_mode = CurNormal;
		grabc = NULL;
	}

	if (client_monitor(c,NULL))
		client_monitor(c,NULL)->un_map = 1;

	if (client_is_unmanaged(c)) {
		wlr_scene_node_destroy(CLIENT_SCENE(c));
		return SCM_UNSPECIFIED;
	}

	wl_list_remove(&c->link);
    return SCM_UNSPECIFIED;
}

SCM_DEFINE(gwwm_updatemon,"update-monitor",2,0,0,(SCM sm,SCM sconfig),""){

    Monitor *m=UNWRAP_MONITOR(sm);
    struct wlr_output_configuration_v1 *config=UNWRAP_WLR_OUTPUT_CONFIGURATION_V1(sconfig);
  struct wlr_output_configuration_head_v1 *config_head =
    wlr_output_configuration_head_v1_create(config, MONITOR_WLR_OUTPUT(m));

  /* TODO: move clients off disabled monitors */
  /* TODO: move focus if current_monitor is disabled */

  /* Get the effective monitor geometry to use for surfaces */
  SET_MONITOR_AREA(m, wlr_output_layout_get_box(gwwm_output_layout(NULL),
                                                MONITOR_WLR_OUTPUT(m)));
  (SET_MONITOR_WINDOW_AREA(m, MONITOR_AREA(m)));
  wlr_scene_output_set_position(monitor_scene_output(m,NULL), (MONITOR_AREA(m))->x,
                                (MONITOR_AREA(m))->y);
  /* Calculate the effective monitor geometry to use for clients */
  arrangelayers(sm);
  /* Don't move clients to the left output when plugging monitors */
  arrange(m);

  config_head->state.enabled = MONITOR_WLR_OUTPUT(m)->enabled;
  config_head->state.mode = ((MONITOR_WLR_OUTPUT(m))->current_mode);
  config_head->state.x = MONITOR_AREA(m)->x;
  config_head->state.y = MONITOR_AREA(m)->y;
  return SCM_UNSPECIFIED;
}

void updatetitle_x11(struct wl_listener *listener, void *data)
{
  Client *c;
  struct wlr_xwayland_surface *xsurface=data;
  if (xsurface->mapped && xsurface->surface && (c=client_from_wlr_surface(xsurface->surface)))
    {
      scm_c_run_hook(REF("gwwm hooks", "update-title-hook"),
                 scm_list_1(WRAP_CLIENT(c)));
    }
}

void
view(const Arg *arg)
{
  PRINT_FUNCTION
  if ((arg->ui & TAGMASK) ==
      current_monitor()->tagset[(current_monitor())->seltags])
		return;
  (current_monitor())->seltags ^= 1; /* toggle sel tagset */
	if (arg->ui & TAGMASK)
      (current_monitor())->tagset[(current_monitor())->seltags] = arg->ui & TAGMASK;
	focusclient(focustop(current_monitor()), 1);
	arrange(current_monitor());
}

SCM_DEFINE (gwwm_view, "view",1,0,0,(SCM ui),""){
  view(&((Arg){.ui=1 << (scm_to_int(ui))}));
  return SCM_UNSPECIFIED;
}

void
virtualkeyboard(struct wl_listener *listener, void *data)
{
  PRINT_FUNCTION
	struct wlr_virtual_keyboard_v1 *keyboard = data;
	struct wlr_input_device *device = &keyboard->input_device;
	createkeyboard(WRAP_WLR_INPUT_DEVICE(device));
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

SCM_DEFINE (gwwm_monitor_seltags, "%monitor-seltags",1, 0,0,
            (SCM m) ,
            "return M's seltags.")
#define FUNC_NAME s_gwwm_monitor_seltags
{
  return (scm_from_unsigned_integer((UNWRAP_MONITOR(m))->seltags));
}
#undef FUNC_NAME

Monitor *
xytomon(double x, double y)
{
  PRINT_FUNCTION
  SCM o=(REF_CALL_2("gwwm monitor","monitor-at",scm_from_double(x),scm_from_double(y)));
  return scm_is_false(o) ? NULL : (UNWRAP_MONITOR(o));
}

struct wlr_scene_node *
xytonode(double x, double y, struct wlr_surface **psurface,
		Client **pc, Client **pl, double *nx, double *ny)
{
  PRINT_FUNCTION
	struct wlr_scene_node *node, *pnode;
	struct wlr_surface *surface = NULL;
	Client *c = NULL;
	Client *l = NULL;
	char* focus_order[] = {"overlay-layer",
                          "top-layer",
                          "float-layer",
                          "tile-layer",
                          "bottom-layer",
                          "background-layer"};

	for (int layer = 0; layer < 5; layer++) {
		if ((node = wlr_scene_node_at(UNWRAP_WLR_SCENE_NODE(REF("gwwm",focus_order[layer])), x, y, nx, ny))) {
			if (node->type == WLR_SCENE_NODE_SURFACE)
				surface = wlr_scene_surface_from_node(node)->surface;
			/* Walk the tree to find a node that knows the client */
			for (pnode = node; pnode && !c; pnode = pnode->parent)
				c = pnode->data;
			if (c && CLIENT_IS_LAYER_SHELL(WRAP_CLIENT(c))) {
				c = NULL;
				l = pnode->data;
			}
		}
		if (surface)
			break;
	}

	if (psurface) *psurface = surface;
	if (pc) *pc = c;
	if (pl) *pl = l;
	return node;
}

SCM_DEFINE (gwwm_zoom, "zoom",0, 0,0,
            () ,
            "c")
#define FUNC_NAME s_gwwm_zoom
{
  PRINT_FUNCTION
	Client *c, *sel = current_client();
	if (!sel
        || scm_is_false
        (LAYOUT_PROCEDURE
         (scm_list_ref
          (MONITOR_LAYOUTS(current_monitor()),
           (((scm_call_1(REFP("gwwm monitor","monitor-sellt"),  \
                               (WRAP_MONITOR(current_monitor())))))))))
        || (CLIENT_IS_FLOATING(sel)))
		return SCM_UNSPECIFIED;

	/* Search for the first tiled window that is not sel, marking sel as
	 * NULL if we pass it along the way */
	wl_list_for_each(c, &clients, link)
      if (visibleon(c, current_monitor()) && !CLIENT_IS_FLOATING(c)) {
			if (c != sel)
				break;
			sel = NULL;
		}

	/* Return if no other tiled window was found */
	if (&c->link == &clients)
		return  SCM_UNSPECIFIED;

	/* If we passed sel, move c to the front; otherwise, move sel to the
	 * front */
	if (!sel)
		sel = c;
	wl_list_remove(&sel->link);
	wl_list_insert(&clients, &sel->link);
    REF_CALL_2("ice-9 q", "q-remove!", REF_CALL_0("gwwm client", "%client"), sel->scm);
    REF_CALL_2("ice-9 q", "q-push!", REF_CALL_0("gwwm client", "%client"), sel->scm);
	focusclient(sel, 1);
	arrange(current_monitor());
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#ifdef XWAYLAND
void
activatex11(struct wl_listener *listener, void *data)
{
  PRINT_FUNCTION
  struct wlr_xwayland_surface *xsurface=data;
  Client *c;
  if (xsurface->mapped && xsurface->surface &&
      (c = client_from_wlr_surface(xsurface->surface))
      && (CLIENT_IS_MANAGED(c)))
    {
      /* Only "managed" windows can be activated */
      wlr_xwayland_surface_activate(wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c)), 1);
    }
}

void
configurex11(struct wl_listener *listener, void *data)
{
  PRINT_FUNCTION;
  struct wlr_xwayland_surface_configure_event *event = data;
  wlr_xwayland_surface_configure(event->surface,
                                 event->x, event->y, event->width, event->height);
  /* CLIENT_SET_SURFACE(c,event->surface->surface); */
}


void gwwm_i_unfullscreen_all(Client *c){
  PRINT_FUNCTION;
  if (CLIENT_IS_FULLSCREEN(c) && visibleon(c, client_monitor(c,NULL)))
    setfullscreen(c, 0);
}

void
createnotifyx11(struct wl_listener *listener, void *data)
{
  PRINT_FUNCTION;
  struct wlr_xwayland_surface *xwayland_surface = data;
	Client *c;
    client_for_each_alives(&gwwm_i_unfullscreen_all);

	/* Allocate a Client for this surface */
	c = scm_gc_calloc(sizeof(*c),"x-client");
    register_client(c,GWWM_X_CLIENT_TYPE);
    xwayland_surface->data = WRAP_CLIENT(c);
    /* CLIENT_SET_SURFACE(c,xwayland_surface->surface); */
    CLIENT_SET_BW(c,GWWM_BORDERPX());
    scm_slot_set_x(WRAP_CLIENT(c),
                   scm_from_utf8_symbol("super-surface"),
                   WRAP_WLR_XWAYLAND_SURFACE(xwayland_surface));
    scm_c_run_hook(REF("gwwm hooks", "create-client-hook"),
                 scm_list_1(WRAP_CLIENT(c)));
	/* Listen to the various events it can emit */
    client_add_listen(c, &xwayland_surface->events.request_activate, activatex11);
    client_add_listen(c, &xwayland_surface->events.request_configure, configurex11);

    client_add_listen(c,&xwayland_surface->events.set_hints, sethints);
    client_add_listen(c,&xwayland_surface->events.set_title,updatetitle_x11);
    client_add_listen(c,&xwayland_surface->events.request_fullscreen,fullscreennotify_x11);
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
sethints(struct wl_listener *listener, void *data)
{
  PRINT_FUNCTION;
  struct wlr_xwayland_surface *xsurface=data;
  Client *c;
  if (xsurface->mapped && xsurface->surface &&
      (c = client_from_wlr_surface(xsurface->surface))) {

    if (c != current_client() && CLIENT_SURFACE(c)) {
      CLIENT_SET_URGENT(c, (wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c)))->hints_urgency);
    }
  }
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
  define_listener(new_xwayland_surface,"new-xwayland-surface",createnotifyx11);
  define_listener(xwayland_ready,"xwaylandready",xwaylandready);
#undef define_listener
  scm_c_define("%c-clients",WRAP_WL_LIST(&clients));
#ifndef SCM_MAGIC_SNARFER
#include "gwwm.x"
#endif
}
