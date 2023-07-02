/* See LICENSE.dwm file for copyright and license details. */
#ifndef GWWM_UTIL_H
#define GWWM_UTIL_H

#include <libguile.h>
#include <stdarg.h>
#include <string.h>
void die(const char *fmt, ...);

#define REF(A, B) (scm_c_public_ref(A, B))
#define REFP(A, B) (scm_c_private_ref(A, B))
#define FROM_P(P) (scm_from_pointer(P, NULL))
#define TO_P(P) (scm_to_pointer(P))
#define REF_CALL_0(M, N) (scm_call_0(REF(M, N)))
#define REF_CALL_1(M, N, ARG1) (scm_call_1(REF(M, N), ARG1))
#define REF_CALL_2(M, N, ARG1, ARG2) (scm_call_2(REF(M, N), ARG1, ARG2))
#define REF_CALL_3(M, N, ARG1, ARG2, ARG3)                                     \
  (scm_call_3(REF(M, N), ARG1, ARG2, ARG3))
#define SCM_LOOKUP_REF(name) (scm_variable_ref(scm_c_lookup(name)))
#define MAKE_P(i) (REF_CALL_1("system foreign", "make-pointer", i))
#define WRAP_WLR_BACKEND(p)                                                    \
  (REF_CALL_1("wlroots backend", "wrap-wlr-backend", FROM_P(p)))
#define UNWRAP_WLR_BACKEND(p)                                                  \
  (TO_P(REF_CALL_1("wlroots backend", "unwrap-wlr-backend", p)))
#define WRAP_WLR_INPUT_DEVICE(p)                                               \
  (REF_CALL_1("wlroots types input-device", "wrap-wlr-input-device", FROM_P(p)))
#define UNWRAP_WLR_INPUT_DEVICE(p)                                             \
  ((struct wlr_input_device *)(TO_P(REF_CALL_1("wlroots types input-device", "unwrap-wlr-input-device", p))))
#define WRAP_WL_DISPLAY(p)                                                     \
  (REF_CALL_1("wayland display", "wrap-wl-display", FROM_P(p)))
#define UNWRAP_WL_DISPLAY(p)                                                   \
  (TO_P(REF_CALL_1("wayland display", "unwrap-wl-display", p)))
#define UNWRAP_WL_LISTENER(p)                                       \
  (TO_P(REF_CALL_1("wayland listener", "unwrap-wl-listener", p)))
#define WRAP_WL_LISTENER(p)                                       \
  (REF_CALL_1("wayland listener", "wrap-wl-listener", FROM_P(p)))
#define UNWRAP_WL_LIST(p)                                       \
  ((struct wl_list*)(TO_P(REF_CALL_1("wayland list", "unwrap-wl-list", p))))
#define WRAP_WL_LIST(p)                                       \
  (REF_CALL_1("wayland list", "wrap-wl-list", FROM_P(p)))
#define UNWRAP_WL_SIGNAL(p)                                       \
  (TO_P(REF_CALL_1("wayland signal", "unwrap-wl-signal", p)))
#define WRAP_WL_SIGNAL(p)                                       \
  (REF_CALL_1("wayland signal", "wrap-wl-signal", FROM_P(p)))
#define WRAP_WLR_OUTPUT_LAYOUT(p)                                              \
  (REF_CALL_1("wlroots types output-layout", "wrap-wlr-output-layout",         \
              FROM_P(p)))
#define UNWRAP_WLR_OUTPUT_LAYOUT(p)                                            \
  (TO_P(REF_CALL_1("wlroots types output-layout ", "unwrap-wlr-output-layout", \
                   p)))
#define WRAP_WLR_OUTPUT_CONFIGURATION_V1(p)                                              \
  (REF_CALL_1("wlroots types output-management", "wrap-wlr-output-configuration-v1",         \
              FROM_P(p)))
#define UNWRAP_WLR_OUTPUT_CONFIGURATION_V1(p)                                            \
  (TO_P(REF_CALL_1("wlroots types output-management ", "unwrap-wlr-output-configuration-v1", \
                   p)))
#define WRAP_WLR_OUTPUT(p)                                                     \
  (REF_CALL_1("wlroots types output", "wrap-wlr-output", FROM_P(p)))
#define UNWRAP_WLR_OUTPUT(p)                                                   \
  (struct wlr_output *)(TO_P(                                                  \
      REF_CALL_1("wlroots types output ", "unwrap-wlr-output", p)))
#define WRAP_WLR_CURSOR(p)                                                     \
  (REF_CALL_1("wlroots types cursor", "wrap-wlr-cursor", FROM_P(p)))
#define UNWRAP_WLR_CURSOR(p)                                                   \
  (struct wlr_cursor *)(TO_P(                                                  \
      REF_CALL_1("wlroots types cursor ", "unwrap-wlr-cursor", p)))
#define WRAP_WLR_XCURSOR_MANAGER(p)                                                     \
  (REF_CALL_1("wlroots types xcursor-manager", "wrap-wlr-xcursor-manager", FROM_P(p)))
#define UNWRAP_WLR_XCURSOR_MANAGER(p)                                                   \
  (struct wlr_xcursor_manager *)(TO_P(                                                  \
      REF_CALL_1("wlroots types xcursor-manager ", "unwrap-wlr-xcursor-manager", p)))

#define WRAP_WLR_XDG_SURFACE(p)                                                \
  (REF_CALL_1("wlroots types xdg-shell", "wrap-wlr-xdg-surface", FROM_P(p)))
#define UNWRAP_WLR_XDG_SURFACE(p)                                              \
  ((struct wlr_xdg_surface *)TO_P(REF_CALL_1("wlroots types xdg-shell", "unwrap-wlr-xdg-surface", p)))
#define WRAP_WLR_XDG_POPUP(p)                                                \
  (REF_CALL_1("wlroots types xdg-shell", "wrap-wlr-xdg-popup", FROM_P(p)))
#define UNWRAP_WLR_XDG_POPUP(p)                                              \
  (TO_P(REF_CALL_1("wlroots types xdg-shell", "unwrap-wlr-xdg-popup", p)))

#define WRAP_WLR_SEAT(p)                                                       \
  (REF_CALL_1("wlroots types seat", "wrap-wlr-seat", FROM_P(p)))
#define UNWRAP_WLR_SEAT(p)                                                     \
  (TO_P(REF_CALL_1("wlroots types seat", "unwrap-wlr-seat", p)))
#define WRAP_WLR_SCENE_NODE(p)                                                 \
  (REF_CALL_1("wlroots types scene", "wrap-wlr-scene-node", FROM_P(p)))
#define UNWRAP_WLR_SCENE_NODE(p)                                               \
  (TO_P(REF_CALL_1("wlroots types scene", "unwrap-wlr-scene-node", p)))
#define WRAP_WLR_SCENE_OUTPUT(p)                                                 \
  (REF_CALL_1("wlroots types scene", "wrap-wlr-scene-output", FROM_P(p)))
#define UNWRAP_WLR_SCENE_OUTPUT(p)                                               \
  (TO_P(REF_CALL_1("wlroots types scene", "unwrap-wlr-scene-output", p)))
#define WRAP_WLR_SCENE_RECT(p)                                                 \
  (REF_CALL_1("wlroots types scene", "wrap-wlr-scene-rect", FROM_P(p)))
#define UNWRAP_WLR_SCENE_RECT(p)                                               \
  (TO_P(REF_CALL_1("wlroots types scene", "unwrap-wlr-scene-rect", p)))
#define WRAP_XDG_TOPLEVEL_SET_FULLSCREEN_EVENT(p)                              \
  (REF_CALL_1("wlroots types xdg-shell",                                \
              "wrap-wlr-xdg-toplevel-set-fullscreen-event",FROM_P(p)))
#define WRAP_WLR_SCENE(p)                                                      \
  (REF_CALL_1("wlroots types scene", "wrap-wlr-scene", FROM_P(p)))
#define UNWRAP_WLR_SCENE(p)                                                    \
  (TO_P(REF_CALL_1("wlroots types scene", "unwrap-wlr-scene", p)))

#define WRAP_WLR_EVENT_POINTER_AXIS(p)                                         \
  (REF_CALL_1("wlroots types pointer", "wrap-wlr-event-pointer-axis",          \
              FROM_P(p)))
#define UNWRAP_WLR_EVENT_POINTER_AXIS(p)                                       \
  (TO_P(REF_CALL_1("wlroots types pointer", "unwrap-wlr-event-pointer-axis",   \
                   p)))
#define WRAP_WLR_EVENT_POINTER_BUTTON(p)                                       \
  (REF_CALL_1("wlroots types pointer", "wrap-wlr-event-pointer-button",        \
              FROM_P(p)))
#define UNWRAP_WLR_EVENT_POINTER_BUTTON(p)                                     \
  (TO_P(REF_CALL_1("wlroots types pointer", "unwrap-wlr-event-pointer-button", \
                   p)))
#define WRAP_WLR_SEAT_REWUEST_SET_SELECTION_EVENT(p)                           \
  (REF_CALL_1("wlroots types seat",                                            \
              "wrap-wlr-seat-request-set-selection-event", FROM_P(p)))
#define UNWRAP_WLR_SEAT_REWUEST_SET_SELECTION_EVENT(p)                         \
  (TO_P(REF_CALL_1("wlroots types seat",                                       \
                   "unwrap-wlr-seat-request-set-selection-event", p)))

#define WRAP_WLR_XWAYLAND_SURFACE(p)                                           \
  (REF_CALL_1("wlroots xwayland", "wrap-wlr-xwayland-surface", FROM_P(p)))
#define UNWRAP_WLR_XWAYLAND_SURFACE(p)                                         \
  ((struct wlr_xwayland_surface *)(TO_P(REF_CALL_1("wlroots xwayland", "unwrap-wlr-xwayland-surface", p))))
#define WRAP_WLR_BOX(p)                                                        \
  (REF_CALL_1("wlroots util box", "wrap-wlr-box", FROM_P(p)))
#define UNWRAP_WLR_BOX(p)                                                      \
  ((struct wlr_box*)(TO_P(REF_CALL_1("wlroots util box", "unwrap-wlr-box", p))))
#define WRAP_WLR_SURFACE(p)                                                    \
  (REF_CALL_1("wlroots types compositor", "wrap-wlr-surface", FROM_P(p)))
#define UNWRAP_WLR_SURFACE(p)                                                  \
  (TO_P(REF_CALL_1("wlroots types compositor", "unwrap-wlr-surface", p)))

#define WRAP_WLR_LAYER_SURFACE(p)                                                    \
  (REF_CALL_1("wlroots types layer-shell", "wrap-wlr-layer-surface-v1", FROM_P(p)))
#define UNWRAP_WLR_LAYER_SURFACE(p)                                                  \
  (TO_P(REF_CALL_1("wlroots types layer-shell", "unwrap-wlr-layer-surface-v1", p)))

#define WRAP_WLR_RENDERER(p)                                             \
  (REF_CALL_1("wlroots render renderer", "wrap-wlr-renderer", FROM_P(p)))
#define UNWRAP_WLR_RENDERER(p)                                                  \
  (TO_P(REF_CALL_1("wlroots render renderer", "unwrap-wlr-renderer", p)))
#define WRAP_WLR_ALLOCATOR(p)                                             \
  (REF_CALL_1("wlroots render allocator", "wrap-wlr-allocator", FROM_P(p)))
#define UNWRAP_WLR_ALLOCATOR(p)                                                  \
  (TO_P(REF_CALL_1("wlroots render allocator", "unwrap-wlr-allocator", p)))
#define WRAP_WLR_IDLE(p)                                                       \
  (REF_CALL_1("wlroots types idle", "wrap-wlr-idle", FROM_P(p)))
#define UNWRAP_WLR_IDLE(p)                                                     \
  (TO_P(REF_CALL_1("wlroots types idle", "unwrap-wlr-idle", p)))
#define WRAP_WLR_BUFFER(p)                                                       \
  (REF_CALL_1("wlroots types buffer", "wrap-wlr-buffer", FROM_P(p)))
#define UNWRAP_WLR_BUFFER(p)                                                     \
  (TO_P(REF_CALL_1("wlroots types buffer", "unwrap-wlr-buffer", p)))
#define WRAP_WLR_EVENT_KEYBOARD_KEY(p)                                         \
  (REF_CALL_1("wlroots types keyboard", "wrap-wlr-event-keyboard-key",         \
              FROM_P(p)))
#define UNWRAP_WLR_EVENT_KEYBOARD_KEY(p)                                       \
  (TO_P(REF_CALL_1("wlroots types keyboard", "unwrap-wlr-event-keyboard-key",  \
                   p)))
#define LAYOUT_PROCEDURE(l) (REF_CALL_1("gwwm layout", "layout-procedure", l))
#define INNER_MONITOR_HASH_TABLE REFP("gwwm monitor", "%monitors")
#define send_log(v, b, ...) _send_log(#v, b, ##__VA_ARGS__, "/0")
#define PRINT_FUNCTION send_log(DEBUG, __FUNCTION__);
#define GWWM_ASSERT_CLIENT_OR_FALSE(client, position)                          \
  SCM_ASSERT((SCM_IS_A_P(client, REFP("gwwm client", "<gwwm-base-client>")) || \
              scm_is_false(client)),                                           \
             client, position, FUNC_NAME)
#define SHALLOW_CLONE(o) (REF_CALL_1("oop goops","shallow-clone",o))
void _send_log(const char *arg, ...);
#endif
