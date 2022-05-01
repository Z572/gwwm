#define _POSIX_C_SOURCE 200112L
#include <assert.h>
#include <getopt.h>
#include <libguile.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <wayland-server-core.h>
#include <wlr/backend.h>
#include <wlr/render/allocator.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_gamma_control_v1.h>
#include <wlr/types/wlr_idle.h>
#include <wlr/types/wlr_input_device.h>
#include <wlr/types/wlr_keyboard.h>
#include <wlr/types/wlr_layer_shell_v1.h>
#include <wlr/types/wlr_output.h>
#include <wlr/types/wlr_output_layout.h>
#include <wlr/types/wlr_pointer.h>
#include <wlr/types/wlr_scene.h>
#include <wlr/types/wlr_seat.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_shell.h>

#include <wlr/util/log.h>
#include <wlr/xwayland.h>
#include <xkbcommon/xkbcommon.h>
//#include "gws.c"

#define REF(A, B) (scm_c_public_ref(A, B))
#define FROM_P(P) (scm_from_pointer(P, NULL))
#define GI_REF(p) (scm_c_public_ref("gwwm init", p))
/* For brevity's sake, struct members are annotated where they are used. */
enum tinywl_cursor_mode {
  TINYWL_CURSOR_PASSTHROUGH,
  TINYWL_CURSOR_MOVE,
  TINYWL_CURSOR_RESIZE,
};

struct tinywl_server {
  /* struct wl_display *wl_display; */
  /* struct wlr_backend *backend; */
  /* struct wlr_compositor *compositor; */
  /* struct wlr_renderer *renderer; */
  /* struct wlr_allocator *allocator; */
  struct wlr_scene *scene;

  struct wlr_xdg_shell *xdg_shell;
  struct wl_listener new_xdg_surface;
  struct wl_list views;

  struct wlr_cursor *cursor;
  struct wlr_xcursor_manager *cursor_mgr;
  struct wl_listener cursor_motion;
  struct wl_listener cursor_motion_absolute;
  struct wl_listener cursor_button;
  struct wl_listener cursor_axis;
  struct wl_listener cursor_frame;

  /* struct wlr_layer_shell_v1 *layer_shell; */
  /* struct wl_listener request_new_surface; */

  struct wlr_seat *seat;
  struct wl_listener new_input;
  struct wl_listener request_cursor;
  struct wl_listener request_set_selection;
  struct wl_list keyboards;
  //  enum tinywl_cursor_mode cursor_mode;
  struct tinywl_view *grabbed_view;
  double grab_x, grab_y;
  struct wlr_box grab_geobox;
  uint32_t resize_edges;

  struct wlr_output_layout *output_layout;
  struct wl_list outputs;
  struct wl_listener new_output;
  /* enum tinywl_cursor_mode cursor_mode; */
};

struct tinywl_output {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_output *wlr_output;
  struct wl_listener frame;
};

struct tinywl_view {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_xdg_surface *xdg_surface;
  struct wlr_scene_node *scene_node;
  struct wl_listener map;
  struct wl_listener unmap;
  struct wl_listener destroy;
  struct wl_listener request_move;
  struct wl_listener request_resize;
  struct wl_listener request_fullscreen;
  int x, y;
};

struct tinywl_keyboard {
  struct wl_list link;
  struct tinywl_server *server;
  struct wlr_input_device *device;

  struct wl_listener modifiers;
  struct wl_listener key;
};

static void gwwm_wl_list_remove(struct wl_list *l) {
  scm_call_1(
      scm_c_public_ref("wayland list", "wl-list-remove"),
      scm_call_1(scm_c_public_ref("wayland list", "wrap-wl-list"), FROM_P(l)));
}

static void gwwm_wl_list_insert(struct wl_list *l, struct wl_list *l2) {
  scm_call_2(
      REF("wayland list", "wl-list-insert"),
      scm_call_1(scm_c_public_ref("wayland list", "wrap-wl-list"), FROM_P(l)),
      scm_call_1(scm_c_public_ref("wayland list", "wrap-wl-list"), FROM_P(l2)));
}

static void focus_view(struct tinywl_view *view, struct wlr_surface *surface) {
  /* Note: this function only deals with keyboard focus. */
  if (view == NULL) {
    return;
  }
  struct tinywl_server *server = view->server;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *prev_surface = seat->keyboard_state.focused_surface;
  if (prev_surface == surface) {
    /* Don't re-focus an already focused surface. */
    return;
  }
  if (prev_surface) {
    /*
     * Deactivate the previously focused surface. This lets the client know
     * it no longer has focus and the client will repaint accordingly, e.g.
     * stop displaying a caret.
     */

    /* struct wlr_xdg_surface *previous = */
    /*   scm_to_pointer(scm_call_1(scm_c_public_ref("wlroots types xdg-shell",
     * "unwrap-wlr-xdg-surface"), */
    /*                             )) ; */
    /* wlr_xdg_toplevel_set_activated(previous, false); */
    scm_call_1(
        GI_REF("disable-toplevel-activated"),
        scm_call_1(
            scm_c_public_ref("wlroots types xdg-shell", "wrap-wlr-xdg-surface"),
            scm_from_pointer(seat->keyboard_state.focused_surface, NULL)));
  }
  struct wlr_keyboard *keyboard = wlr_seat_get_keyboard(seat);
  /* Move the view to the front */
  wlr_scene_node_raise_to_top(view->scene_node);
  gwwm_wl_list_remove(&view->link);
  gwwm_wl_list_insert(&server->views, &view->link);
  /* Activate the new surface */
  /* wlr_xdg_toplevel_set_activated(view->xdg_surface, true); */
  scm_call_2(scm_c_public_ref("wlroots types xdg-shell",
                              "wlr-xdg-toplevel-set-activated"),
             scm_call_1(scm_c_public_ref("wlroots types xdg-shell",
                                         "wrap-wlr-xdg-surface"),
                        scm_from_pointer(view->xdg_surface, NULL)),
             scm_from_bool(true));
  /*
   * Tell the seat to have the keyboard enter this surface. wlroots will keep
   * track of this and automatically send key events to the appropriate
   * clients without additional work on your part.
   */
  wlr_seat_keyboard_notify_enter(seat, view->xdg_surface->surface,
                                 keyboard->keycodes, keyboard->num_keycodes,
                                 &keyboard->modifiers);
}

SCM_DEFINE(scm_focus_view, "focus-view", 2, 0, 0, (SCM view, SCM surface), "") {
  focus_view(scm_to_pointer(view),
             scm_to_pointer(scm_call_1(scm_c_public_ref("wlroots types surface",
                                                        "unwrap-wlr-surface"),
                                       surface)));
  return SCM_UNSPECIFIED;
}

static void gwwm_signal_add(struct wl_signal *signal,
                            struct wl_listener *listener) {
  scm_call_2(
      scm_c_public_ref("wayland signal", "wl-signal-add"),
      scm_call_1(scm_c_public_ref("wayland signal", "wrap-wl-signal"),
                 scm_from_pointer(signal, NULL)),
      scm_call_1(scm_c_public_ref("wayland listener", "wrap-wl-listener"),
                 scm_from_pointer(listener, NULL)));
}

static struct wl_display *server_wl_display(void) {
  return (struct wl_display *)(scm_to_pointer(
      scm_call_1(scm_c_public_ref("wayland display", "unwrap-wl-display"),
                 GI_REF("gwwm-wl-display"))));
}

static enum tinywl_cursor_mode server_cursor_mode(void) {
  return (scm_to_int(GI_REF("server-cursor-mode")));
}

static void set_server_cursor_mode(enum tinywl_cursor_mode n) {
  (scm_call_1(GI_REF("set-server-cursor-mode"), scm_from_int(n)));
}

static struct wlr_backend *server_backend(void) {
  return (struct wlr_backend *)(scm_to_pointer(
      scm_call_1(scm_c_public_ref("wlroots backend", "unwrap-wlr-backend"),
                 GI_REF("gwwm-server-backend"))));
}
static struct wlr_renderer *server_renderer(void) {
  return (struct wlr_renderer *)(scm_to_pointer(scm_call_1(
      scm_c_public_ref("wlroots render renderer", "unwrap-wlr-renderer"),
      GI_REF("gwwm-server-renderer"))));
}
static struct wlr_allocator *server_allocator(void) {
  return (struct wlr_allocator *)(scm_to_pointer(scm_call_1(
      scm_c_public_ref("wlroots render allocator", "unwrap-wlr-allocator"),
      GI_REF("gwwm-server-allocator"))));
}
static struct wlr_output_layout *server_output_layout(void) {
  return (struct wlr_output_layout *)(scm_to_pointer(
      scm_call_1(REF("wlroots types output-layout", "unwrap-wlr-output-layout"),
                 GI_REF("gwwm-server-output-layout"))));
}

static void keyboard_handle_modifiers(struct wl_listener *listener,
                                      void *data) {
  /* This event is raised when a modifier key, such as shift or alt, is
   * pressed. We simply communicate this to the client. */
  struct tinywl_keyboard *keyboard =
      wl_container_of(listener, keyboard, modifiers);
  /*
   * A seat can only have one keyboard, but this is a limitation of the
   * Wayland protocol - not wlroots. We assign all connected keyboards to the
   * same seat. You can swap out the underlying wlr_keyboard like this and
   * wlr_seat handles this transparently.
   */
  wlr_seat_set_keyboard(keyboard->server->seat, keyboard->device);
  /* Send modifiers to the client. */
  wlr_seat_keyboard_notify_modifiers(keyboard->server->seat,
                                     &keyboard->device->keyboard->modifiers);
}

static void keyboard_handle_key(struct wl_listener *listener, void *data) {
  /* This event is raised when a key is pressed or released. */
  struct tinywl_keyboard *keyboard = wl_container_of(listener, keyboard, key);
  struct tinywl_server *server = keyboard->server;
  struct wlr_event_keyboard_key *event = data;
  struct wlr_seat *seat = server->seat;
  /* Translate libinput keycode -> xkbcommon */
  uint32_t keycode = event->keycode + 8;
  /* Get a list of keysyms based on the keymap for this keyboard */
  const xkb_keysym_t *syms;
  int nsyms = xkb_state_key_get_syms(keyboard->device->keyboard->xkb_state,
                                     keycode, &syms);

  bool handled = false;
  uint32_t modifiers = wlr_keyboard_get_modifiers(keyboard->device->keyboard);
  if (/* (modifiers & WLR_MODIFIER_LOGO) && */
      event->state == WL_KEYBOARD_KEY_STATE_PRESSED) {
    /* If alt is held down and this button was _pressed_, we attempt to
     * process it as a compositor keybinding. */

    for (int i = 0; i < nsyms; i++) {
      handled = scm_to_bool(scm_call_3(
          GI_REF("handle-keybinding"), scm_from_pointer(server, NULL),
          scm_from_uint32(modifiers), scm_from_int(syms[i])));
    }
  }

  if (!handled) {
    /* Otherwise, we pass it along to the client. */
    wlr_seat_set_keyboard(seat, keyboard->device);
    wlr_seat_keyboard_notify_key(seat, event->time_msec, event->keycode,
                                 event->state);
  }
}

static void server_new_keyboard(struct tinywl_server *server,
                                struct wlr_input_device *device) {
  struct tinywl_keyboard *keyboard = calloc(1, sizeof(struct tinywl_keyboard));
  keyboard->server = server;
  keyboard->device = device;

  /* We need to prepare an XKB keymap and assign it to the keyboard. This
   * assumes the defaults (e.g. layout = "us"). */
  struct xkb_context *context = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  struct xkb_keymap *keymap =
      xkb_keymap_new_from_names(context, NULL, XKB_KEYMAP_COMPILE_NO_FLAGS);

  wlr_keyboard_set_keymap(device->keyboard, keymap);
  xkb_keymap_unref(keymap);
  xkb_context_unref(context);
  wlr_keyboard_set_repeat_info(device->keyboard, 25, 600);

  /* Here we set up listeners for keyboard events. */
  keyboard->modifiers.notify = keyboard_handle_modifiers;
  gwwm_signal_add(&device->keyboard->events.modifiers, &keyboard->modifiers);
  keyboard->key.notify = keyboard_handle_key;
  gwwm_signal_add(&device->keyboard->events.key, &keyboard->key);

  wlr_seat_set_keyboard(server->seat, device);

  /* And add the keyboard to our list of keyboards */
  gwwm_wl_list_insert(&server->keyboards, &keyboard->link);
}

static void server_new_input(struct wl_listener *listener, void *data) {
  /* This event is raised by the backend when a new input device becomes
   * available. */
  struct tinywl_server *server = wl_container_of(listener, server, new_input);
  struct wlr_input_device *device = data;
  switch (device->type) {
  case WLR_INPUT_DEVICE_KEYBOARD:
    server_new_keyboard(server, device);
    break;
  case WLR_INPUT_DEVICE_POINTER:
    scm_call_2(GI_REF("server-new-pointer"), FROM_P(server), FROM_P(device));
    break;
  default:
    break;
  }
  /* We need to let the wlr_seat know what our capabilities are, which is
   * communiciated to the client. In TinyWL we always have a cursor, even if
   * there are no pointer devices, so we always include that capability. */
  uint32_t caps = WL_SEAT_CAPABILITY_POINTER;
  if (!wl_list_empty(&server->keyboards)) {
    caps |= WL_SEAT_CAPABILITY_KEYBOARD;
  }
  wlr_seat_set_capabilities(server->seat, caps);
}

static struct tinywl_view *desktop_view_at(struct tinywl_server *server,
                                           double lx, double ly,
                                           struct wlr_surface **surface,
                                           double *sx, double *sy) {
  /* This returns the topmost node in the scene at the given layout coords.
   * we only care about surface nodes as we are specifically looking for a
   * surface in the surface tree of a tinywl_view. */
  struct wlr_scene_node *node =
      wlr_scene_node_at(&server->scene->node, lx, ly, sx, sy);
  if (node == NULL || node->type != WLR_SCENE_NODE_SURFACE) {
    return NULL;
  }
  *surface = wlr_scene_surface_from_node(node)->surface;
  /* Find the node corresponding to the tinywl_view at the root of this
   * surface tree, it is the only one for which we set the data field. */
  while (node != NULL && node->data == NULL) {
    node = node->parent;
  }
  return node->data;
}

SCM_DEFINE(scm_desktop_view_at, "desktop-view-at", 6, 0, 0,
           (SCM server, SCM lx, SCM ly, SCM surface, SCM sx, SCM sy), "hh") {
  double sxx = scm_to_double(sx);
  double syy = scm_to_double(sy);

  return scm_from_pointer(desktop_view_at(scm_to_pointer(server),
                                          scm_to_double(lx), scm_to_double(ly),
                                          scm_to_pointer(surface), &sxx, &syy),
                          NULL);
}

/* SCM scm_desktop_view_at (SCM server ,SCM lx ,SCM ly ,SCM surface ,SCM sx,SCM
 * sy ){ */

/*   double sxx= scm_to_double(sx); */
/*   double syy = scm_to_double(sy); */
/*   return scm_from_pointer(desktop_view_at(scm_to_pointer(server), */
/*                                            scm_to_double(lx), */
/*                                            scm_to_double(ly), */
/*                                            scm_to_pointer(surface), */
/*                                            &sxx, */
/*                                            &syy) ,NULL); */
/* } */

static void process_cursor_move(struct tinywl_server *server, uint32_t time) {
  /* Move the grabbed view to the new position. */
  struct tinywl_view *view = server->grabbed_view;
  view->x = server->cursor->x - server->grab_x;
  view->y = server->cursor->y - server->grab_y;
  scm_call_3(
      scm_c_public_ref("wlroots types scene", "wlr-scene-node-set-position"),
      scm_call_1(scm_c_public_ref("wlroots types scene", "wrap-wlr-scene"),
                 scm_from_pointer(view->scene_node, NULL)),
      scm_from_int(view->x), scm_from_int(view->y));
}

static void process_cursor_resize(struct tinywl_server *server, uint32_t time) {
  /*
   * Resizing the grabbed view can be a little bit complicated, because we
   * could be resizing from any corner or edge. This not only resizes the view
   * on one or two axes, but can also move the view if you resize from the top
   * or left edgzes (or top-left corner).
   *
   * Note that I took some shortcuts here. In a more fleshed-out compositor,
   * you'd wait for the client to prepare a buffer at the new size, then
   * commit any movement that was prepared.
   */
  struct tinywl_view *view = server->grabbed_view;
  double border_x = server->cursor->x - server->grab_x;
  double border_y = server->cursor->y - server->grab_y;
  int new_left = server->grab_geobox.x;
  int new_right = server->grab_geobox.x + server->grab_geobox.width;
  int new_top = server->grab_geobox.y;
  int new_bottom = server->grab_geobox.y + server->grab_geobox.height;

  if (server->resize_edges & WLR_EDGE_TOP) {
    new_top = border_y;
    if (new_top >= new_bottom) {
      new_top = new_bottom - 1;
    }
  } else if (server->resize_edges & WLR_EDGE_BOTTOM) {
    new_bottom = border_y;
    if (new_bottom <= new_top) {
      new_bottom = new_top + 1;
    }
  }
  if (server->resize_edges & WLR_EDGE_LEFT) {
    new_left = border_x;
    if (new_left >= new_right) {
      new_left = new_right - 1;
    }
  } else if (server->resize_edges & WLR_EDGE_RIGHT) {
    new_right = border_x;
    if (new_right <= new_left) {
      new_right = new_left + 1;
    }
  }

  struct wlr_box geo_box;
  wlr_xdg_surface_get_geometry(view->xdg_surface, &geo_box);
  view->x = new_left - geo_box.x;
  view->y = new_top - geo_box.y;
  wlr_scene_node_set_position(view->scene_node, view->x, view->y);

  int new_width = new_right - new_left;
  int new_height = new_bottom - new_top;
  wlr_xdg_toplevel_set_size(view->xdg_surface, new_width, new_height);
}

static void process_cursor_motion(struct tinywl_server *server, uint32_t time) {
  /* If the mode is non-passthrough, delegate to those functions. */
  if (server_cursor_mode() /* server->cursor_mode */ == TINYWL_CURSOR_MOVE) {
    process_cursor_move(server, time);
    return;
  } else if (server_cursor_mode() /* server->cursor_mode */ ==
             TINYWL_CURSOR_RESIZE) {
    process_cursor_resize(server, time);
    return;
  }

  /* Otherwise, find the view under the pointer and send the event along. */
  double sx, sy;
  struct wlr_seat *seat = server->seat;
  struct wlr_surface *surface = NULL;
  struct tinywl_view *view = desktop_view_at(
      server, server->cursor->x, server->cursor->y, &surface, &sx, &sy);
  if (!view) {
    /* If there's no view under the cursor, set the cursor image to a
     * default. This is what makes the cursor image appear when you move it
     * around the screen, not over any views. */
    /* wlr_xcursor_manager_set_cursor_image(server->cursor_mgr, "left_ptr", */
    /*                                      server->cursor); */
    scm_call_1(GI_REF("wlr_xcursor_manager_set_cursor_image"),
               scm_from_utf8_string("left_ptr"));
  }
  if (surface) {
    /*
     * Send pointer enter and motion events.
     *
     * The enter event gives the surface "pointer focus", which is distinct
     * from keyboard focus. You get pointer focus by moving the pointer over
     * a window.
     *
     * Note that wlroots will avoid sending duplicate enter/motion events if
     * the surface has already has pointer focus or if the client is already
     * aware of the coordinates passed.
     */
    wlr_seat_pointer_notify_enter(seat, surface, sx, sy);
    wlr_seat_pointer_notify_motion(seat, time, sx, sy);
  } else {
    /* Clear pointer focus so future button events and such are not sent to
     * the last client to have the cursor over it. */
    wlr_seat_pointer_clear_focus(seat);
  }
}
SCM_DEFINE(scm_process_cursor_motion, "process-cursor-motion", 2, 0, 0,
           (SCM p, SCM time), "") {
  process_cursor_motion(scm_to_pointer(p), scm_to_uint32(time));
  return SCM_UNSPECIFIED;
}

static void server_cursor_button(struct wl_listener *listener, void *data) {
  /* This event is forwarded by the cursor when a pointer emits a button
   * event. */
  struct tinywl_server *server =
      wl_container_of(listener, server, cursor_button);
  struct wlr_event_pointer_button *event = data;
  /* Notify the client with pointer focus that a button press has occurred */
  wlr_seat_pointer_notify_button(server->seat, event->time_msec, event->button,
                                 event->state);
  double sx, sy;
  struct wlr_surface *surface = NULL;
  struct tinywl_view *view = desktop_view_at(
      server, server->cursor->x, server->cursor->y, &surface, &sx, &sy);
  if (event->state == WLR_BUTTON_RELEASED) {
    /* If you released any buttons, we exit interactive move/resize mode. */
    set_server_cursor_mode(TINYWL_CURSOR_PASSTHROUGH);
  } else {
    /* Focus that client if the button was _pressed_ */
    focus_view(view, surface);
  }
}

static void output_frame(struct wl_listener *listener, void *data) {
  /* This function is called every time an output is ready to display a frame,
   * generally at the output's refresh rate (e.g. 60Hz). */
  struct tinywl_output *output = wl_container_of(listener, output, frame);
  struct wlr_scene *scene = output->server->scene;
  struct wlr_scene_output *scene_output =
      wlr_scene_get_scene_output(scene, output->wlr_output);
  struct wlr_output *wlr_output = scene_output->output;

  /* wlr_output_attach_render(wlr_output, NULL); */
  /* wlr_renderer_begin(server_renderer(), wlr_output->width,
   * wlr_output->height); */
  /* wlr_renderer_clear(server_renderer(), (float[]){0.25f, 0.25f, 0.25f, 1});
   */
  /* wlr_renderer_end(server_renderer()); */

  /* wlr_output_commit(wlr_output); */

  /* Render the scene if needed and commit the output */
  wlr_scene_output_commit(scene_output);

  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  wlr_scene_output_send_frame_done(scene_output, &now);
}

static void server_new_output(struct wl_listener *listener, void *data) {
  /* This event is raised by the backend when a new output (aka a display or
   * monitor) becomes available. */
  struct tinywl_server *server = wl_container_of(listener, server, new_output);
  struct wlr_output *wlr_output = data;

  /* } */
  if (!scm_to_bool(scm_call_2(GI_REF("server-new-output"),
                              scm_from_pointer(listener, NULL),
                              scm_from_pointer(data, NULL)))) {
    return;
  }

  /* Allocates and configures our state for this output */
  struct tinywl_output *output = calloc(1, sizeof(struct tinywl_output));
  output->wlr_output = wlr_output;
  output->server = server;
  /* Sets up a listener for the frame notify event. */
  output->frame.notify = output_frame;
  gwwm_signal_add(&wlr_output->events.frame, &output->frame);
  gwwm_wl_list_insert(&server->outputs, &output->link);

  /* Adds this to the output layout. The add_auto function arranges outputs
   * from left-to-right in the order they appear. A more sophisticated
   * compositor would let the user configure the arrangement of outputs in the
   * layout.
   *
   * The output layout utility automatically adds a wl_output global to the
   * display, which Wayland clients can see to find out information about the
   * output (such as DPI, scale factor, manufacturer, etc).
   */
  wlr_output_layout_add_auto(server_output_layout(), wlr_output);
}

static void xdg_toplevel_map(struct wl_listener *listener, void *data) {
  /* Called when the surface is mapped, or ready to display on-screen. */
  struct tinywl_view *view = wl_container_of(listener, view, map);

  /* wl_list_insert(&view->server->views, &view->link); */
  scm_call_2(scm_c_public_ref("wayland list", "wl-list-insert"),
             scm_call_1(scm_c_public_ref("wayland list", "wrap-wl-list"),
                        scm_from_pointer(&view->server->views, NULL)),
             scm_call_1(scm_c_public_ref("wayland list", "wrap-wl-list"),
                        scm_from_pointer(&view->link, NULL)));
  focus_view(view, view->xdg_surface->surface);
}
static void xdg_toplevel_fullscreen(struct wl_listener *listener, void *data) {
  struct tinywl_view *view =
      wl_container_of(listener, view, request_fullscreen);
  struct wlr_box geo_box;
  wlr_xdg_surface_get_geometry(view->xdg_surface, &geo_box);
  wlr_scene_node_set_position(view->scene_node, 0, 0);
  wlr_xdg_toplevel_set_size(view->xdg_surface, geo_box.width, geo_box.height);
  wlr_xdg_toplevel_set_fullscreen(
      view->xdg_surface, !(view->xdg_surface->toplevel->current.fullscreen));
  view->server->grab_x = geo_box.x;
  view->server->grab_y = geo_box.y;
  view->server->grab_geobox.x = 0;
  view->server->grab_geobox.y = 0;
}

static void xdg_toplevel_destroy(struct wl_listener *listener, void *data) {
  /* Called when the surface is destroyed and should never be shown again. */
  struct tinywl_view *view = wl_container_of(listener, view, destroy);

  gwwm_wl_list_remove(&view->map.link);
  gwwm_wl_list_remove(&view->unmap.link);
  gwwm_wl_list_remove(&view->destroy.link);
  gwwm_wl_list_remove(&view->request_move.link);
  gwwm_wl_list_remove(&view->request_resize.link);
  free(view);
}

static void begin_interactive(struct tinywl_view *view,
                              enum tinywl_cursor_mode mode, uint32_t edges) {
  /* This function sets up an interactive move or resize operation, where the
   * compositor stops propegating pointer events to clients and instead
   * consumes them itself, to move or resize windows. */
  struct tinywl_server *server = view->server;
  struct wlr_surface *focused_surface =
      server->seat->pointer_state.focused_surface;
  if (view->xdg_surface->surface !=
      wlr_surface_get_root_surface(focused_surface)) {
    /* Deny move/resize requests from unfocused clients. */
    return;
  }
  server->grabbed_view = view;
  set_server_cursor_mode(mode);

  if (mode == TINYWL_CURSOR_MOVE) {
    server->grab_x = server->cursor->x - view->x;
    server->grab_y = server->cursor->y - view->y;
  } else {
    struct wlr_box geo_box;
    wlr_xdg_surface_get_geometry(view->xdg_surface, &geo_box);

    double border_x =
        (view->x + geo_box.x) + ((edges & WLR_EDGE_RIGHT) ? geo_box.width : 0);
    double border_y = (view->y + geo_box.y) +
                      ((edges & WLR_EDGE_BOTTOM) ? geo_box.height : 0);
    server->grab_x = server->cursor->x - border_x;
    server->grab_y = server->cursor->y - border_y;

    server->grab_geobox = geo_box;
    server->grab_geobox.x += view->x;
    server->grab_geobox.y += view->y;

    server->resize_edges = edges;
  }
}

SCM_DEFINE(scm_begin_interactive, "begin-interactive", 3, 0, 0,
           (SCM view, SCM mode, SCM edges), "") {
  begin_interactive(scm_to_pointer(view), scm_to_int(mode),
                    scm_to_uint32(edges));
  return SCM_UNSPECIFIED;
}

static void server_new_xdg_surface(struct wl_listener *listener, void *data) {
  /* This event is raised when wlr_xdg_shell receives a new xdg surface from a
   * client, either a toplevel (application window) or popup. */
  struct tinywl_server *server =
      wl_container_of(listener, server, new_xdg_surface);
  struct wlr_xdg_surface *xdg_surface = data;

  /* We must add xdg popups to the scene graph so they get rendered. The
   * wlroots scene graph provides a helper for this, but to use it we must
   * provide the proper parent scene node of the xdg popup. To enable this,
   * we always set the user data field of xdg_surfaces to the corresponding
   * scene node. */
  if (xdg_surface->role == WLR_XDG_SURFACE_ROLE_POPUP) {
    struct wlr_xdg_surface *parent =
        wlr_xdg_surface_from_wlr_surface(xdg_surface->popup->parent);
    struct wlr_scene_node *parent_node = parent->data;
    xdg_surface->data = wlr_scene_xdg_surface_create(parent_node, xdg_surface);
    return;
  }
  assert(xdg_surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL);
  scm_call_2(GI_REF("server-new-xdg-surface"), scm_from_pointer(server, NULL),
             scm_from_pointer(xdg_surface, NULL));
  /* Allocate a tinywl_view for this surface */
  struct tinywl_view *view = calloc(1, sizeof(struct tinywl_view));
  view->server = server;
  view->xdg_surface = xdg_surface;
  view->scene_node = wlr_scene_xdg_surface_create(&view->server->scene->node,
                                                  view->xdg_surface);
  view->scene_node->data = view;
  xdg_surface->data = view->scene_node;

  /* Listen to the various events it can emit */
  view->map.notify = xdg_toplevel_map;
  gwwm_signal_add(&xdg_surface->events.map, &view->map);

  view->unmap.notify = scm_to_pointer(GI_REF("xdg-toplevel-unmap-pointer"));
  gwwm_signal_add(&xdg_surface->events.unmap, &view->unmap);
  view->destroy.notify = scm_to_pointer(GI_REF("xdg-toplevel-destroy-pointer"));
  // xdg_toplevel_destroy;
  gwwm_signal_add(&xdg_surface->events.destroy, &view->destroy);

  /* cotd */
  struct wlr_xdg_toplevel *toplevel = xdg_surface->toplevel;
  view->request_fullscreen.notify = xdg_toplevel_fullscreen;
  gwwm_signal_add(&toplevel->events.request_fullscreen,
                  &view->request_fullscreen);
  view->request_move.notify =
      scm_to_pointer(GI_REF("xdg-toplevel-request-move-pointer"));
  gwwm_signal_add(&toplevel->events.request_move, &view->request_move);
  view->request_resize.notify =
      scm_to_pointer(GI_REF("xdg-toplevel-request-resize-pointer"));
  gwwm_signal_add(&toplevel->events.request_resize, &view->request_resize);
}

static void init_snarfer_define(void) {
#ifndef SCM_MAGIC_SNARFER
#include "gwwm.x"
#endif
}
static void inner_main(void *closure, int argc, char *argv[]) {

  scm_c_primitive_load("lisp/gwwm/init.scm");
  init_snarfer_define();
  struct tinywl_server server;
  /* The Wayland display is managed by libwayland. It handles accepting
   * clients from the Unix socket, manging Wayland globals, and so on. */

  /* The backend is a wlroots feature which abstracts the underlying input and
   * output hardware. The autocreate option will choose the most suitable
   * backend based on the current environment, such as opening an X11 window
   * if an X11 server is running. */

  /* Autocreates a renderer, either Pixman, GLES2 or Vulkan for us. The user
   * can also specify a renderer using the WLR_RENDERER env var.
   * The renderer is responsible for defining the various pixel formats it
   * supports for shared memory, this configures that for clients. */

  /* Autocreates an allocator for us.
   * The allocator is the bridge between the renderer and the backend. It
   * handles the buffer creation, allowing wlroots to render onto the
   * screen */
  /* This creates some hands-off wlroots interfaces. The compositor is
   * necessary for clients to allocate surfaces and the data device manager
   * handles the clipboard. Each of these wlroots interfaces has room for you
   * to dig your fingers in and play with their behavior if you want. Note that
   * the clients cannot set the selection directly without compositor approval,
   * see the handling of the request_set_selection event below.*/
  //  wlr_compositor_create(server_wl_display(), server_renderer());
  // wlr_data_device_manager_create(server_wl_display());

  /* Creates an output layout, which a wlroots utility for working with an
   * arrangement of screens in a physical layout. */

  /* Configure a listener to be notified when new outputs are available on the
   * backend. */
  scm_call_1(scm_c_public_ref("wayland list", "wl-list-init"),
             scm_call_1(scm_c_public_ref("wayland list", "wrap-wl-list"),
                        scm_from_pointer(&server.outputs, NULL)));
  server.new_output.notify = server_new_output;
  gwwm_signal_add(&server_backend()->events.new_output, &server.new_output);

  /* Create a scene graph. This is a wlroots abstraction that handles all
   * rendering and damage tracking. All the compositor author needs to do
   * is add things that should be rendered to the scene graph at the proper
   * positions and then call wlr_scene_output_commit() to render a frame if
   * necessary.
   */
  server.scene = scm_to_pointer(
      scm_call_1(scm_c_public_ref("wlroots types scene", "unwrap-wlr-scene"),
                 GI_REF("gwwm-server-scene"))); // wlr_scene_create();
  /* wlr_scene_attach_output_layout(server.scene, server_output_layout()); */

  /* Set up the xdg-shell. The xdg-shell is a Wayland protocol which is used
   * for application windows. For more detail on shells, refer to my article:
   *
   * https://drewdevault.com/2018/07/29/Wayland-shells.html
   */
  wl_list_init(&server.views);
  server.xdg_shell = scm_to_pointer(scm_call_1(
      scm_c_public_ref("wlroots types xdg-shell", "unwrap-wlr-xdg-shell"),
      GI_REF("gwwm-server-xdg-shell")));

  server.new_xdg_surface.notify = server_new_xdg_surface;
  gwwm_signal_add(&server.xdg_shell->events.new_surface,
                  &server.new_xdg_surface);

  /*
   * Creates a cursor, which is a wlroots utility for tracking the cursor
   * image shown on screen.
   */
  server.cursor = scm_to_pointer(
      scm_call_1(scm_c_public_ref("wlroots types cursor", "unwrap-wlr-cursor"),
                 GI_REF("gwwm-server-cursor")));

  /* Creates an xcursor manager, another wlroots utility which loads up
   * Xcursor themes to source cursor images from and makes sure that cursor
   * images are available at all scale factors on the screen (necessary for
   * HiDPI support). We add a cursor theme at scale factor 1 to begin with. */
  server.cursor_mgr = scm_to_pointer(scm_call_1(
      scm_c_public_ref("wlroots types xcursor", "unwrap-wlr-xcursor-manager"),
      GI_REF("gwwm-server-cursor-mgr")));

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
  server.cursor_motion.notify =
      scm_to_pointer(GI_REF("server-cursor-motion-pointer"));
  gwwm_signal_add(&server.cursor->events.motion, &server.cursor_motion);
  server.cursor_motion_absolute.notify =
      scm_to_pointer(GI_REF("server-cursor-motion-absolute-pointer"));
  gwwm_signal_add(&server.cursor->events.motion_absolute,
                  &server.cursor_motion_absolute);
  server.cursor_button.notify = server_cursor_button;
  gwwm_signal_add(&server.cursor->events.button, &server.cursor_button);
  server.cursor_axis.notify =
      scm_to_pointer(GI_REF("server-cursor-axis-pointer"));
  gwwm_signal_add(&server.cursor->events.axis, &server.cursor_axis);
  server.cursor_frame.notify =
      scm_to_pointer(GI_REF("server-cursor-frame-pointer"));
  gwwm_signal_add(&server.cursor->events.frame, &server.cursor_frame);

  /*
   * Configures a seat, which is a single "seat" at which a user sits and
   * operates the computer. This conceptually includes up to one keyboard,
   * pointer, touch, and drawing tablet device. We also rig up a listener to
   * let us know when new input devices are available on the backend.
   */
  wl_list_init(&server.keyboards);
  server.new_input.notify = server_new_input;
  gwwm_signal_add(&server_backend()->events.new_input, &server.new_input);
  server.seat = scm_to_pointer(
      scm_call_1(scm_c_public_ref("wlroots types seat", "unwrap-wlr-seat"),
                 GI_REF("gwwm-server-seat")));
  server.request_cursor.notify =
      scm_to_pointer(GI_REF("gwwm-seat-request-cursor-pointer"));
  gwwm_signal_add(&server.seat->events.request_set_cursor,
                  &server.request_cursor);
  server.request_set_selection.notify =
      scm_to_pointer(GI_REF("gwwm-seat-request-set-selection-pointer"));
  gwwm_signal_add(&server.seat->events.request_set_selection,
                  &server.request_set_selection);
  scm_call_0(GI_REF("gwwm-init-socket"));
  /* Set the WAYLAND_DISPLAY environment variable to our socket and run the
   * startup command if requested. */
  wlr_gamma_control_manager_v1_create(server_wl_display());
  /* Run the Wayland event loop. This does not return until you exit the
   * compositor. Starting the backend rigged up all of the necessary event
   * loop configuration to listen to libinput events, DRM events, generate
   * frame events at the refresh rate, and so on. */
  scm_call_0(GI_REF("gwwm-run!"));
}

int main(int argc, char **argv) {
  scm_boot_guile(argc, argv, inner_main, 0);
  return 0;
}
