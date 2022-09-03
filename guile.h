#include <libguile.h>
#include <stdarg.h>
#include <string.h>
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
#define WRAP_WLR_BACKEND(p)                                                    \
  (REF_CALL_1("wlroots backend", "wrap-wlr-backend", FROM_P(p)))
#define UNWRAP_WLR_BACKEND(p)                                                  \
  (TO_P(REF_CALL_1("wlroots backend", "unwrap-wlr-backend", p)))
#define WRAP_WLR_INPUT_DEVICE(p)                                                    \
  (REF_CALL_1("wlroots types input-device", "wrap-wlr-input-device", FROM_P(p)))
#define UNWRAP_WLR_INPUT_DEVICE(p)                                                  \
  (TO_P(REF_CALL_1("wlroots types input-device", "unwrap-wlr-input-device", p)))
#define WRAP_WL_DISPLAY(p)                                                     \
  (REF_CALL_1("wayland display", "wrap-wl-display", FROM_P(p)))
#define UNWRAP_WL_DISPLAY(p)                                                   \
  (TO_P(REF_CALL_1("wayland display", "unwrap-wl-display", p)))
#define WRAP_MONITOR(o)                                                        \
  (scm_call_3(REF("oop goops", "make"), REF("gwwm monitor", "<gwwm-monitor>"), \
              scm_from_utf8_keyword("data"), FROM_P(o)))
#define UNWRAP_MONITOR(o)                                                      \
  (struct Monitor *)(TO_P(scm_call_1(REFP("gwwm monitor", ".data"), o)))
#define WRAP_WLR_XDG_SURFACE(p)                                                    \
  (REF_CALL_1("wlroots types xdg-shell", "wrap-wlr-xdg-surface", FROM_P(p)))
#define UNWRAP_WLR_XDG_SURFACE(p)                                                  \
  (TO_P(REF_CALL_1("wlroots types xdg-shell", "unwrap-wlr-xdg-surface", p)))
#define WRAP_WLR_SEAT(p)                                                    \
  (REF_CALL_1("wlroots types seat", "wrap-wlr-seat", FROM_P(p)))
#define UNWRAP_WLR_SEAT(p)                                                  \
  (TO_P(REF_CALL_1("wlroots types seat", "unwrap-wlr-seat", p)))
#define WRAP_WLR_SCENE_NODE(p)                                                    \
  (REF_CALL_1("wlroots types scene", "wrap-wlr-scene-node", FROM_P(p)))
#define UNWRAP_WLR_SCENE_NODE(p)                                                    \
  (TO_P(REF_CALL_1("wlroots types scene", "wrap-wlr-scene-node", p)))

#define WRAP_WLR_EVENT_POINTER_AXIS(p)                                                    \
  (REF_CALL_1("wlroots types pointer", "wrap-wlr-event-pointer-axis", FROM_P(p)))
#define UNWRAP_WLR_EVENT_POINTER_AXIS(p)                                                  \
  (TO_P(REF_CALL_1("wlroots types pointer", "unwrap-wlr-event-pointer-axis", p)))
#define WRAP_WLR_SEAT_REWUEST_SET_SELECTION_EVENT(p)                                                    \
  (REF_CALL_1("wlroots types seat", "wrap-wlr-seat-request-set-selection-event", FROM_P(p)))
#define UNWRAP_WLR_SEAT_REWUEST_SET_SELECTION_EVENT(p)                                                  \
  (TO_P(REF_CALL_1("wlroots types seat", "unwrap-wlr-seat-request-set-selection-event", p)))

#define WRAP_WLR_XWAYLAND_SURFACE(p)                                                    \
  (REF_CALL_1("wlroots xwayland", "wrap-wlr-xwayland-surface", FROM_P(p)))
#define UNWRAP_WLR_XWAYLAND_SURFACE(p)                                                  \
  (TO_P(REF_CALL_1("wlroots xwayland", "wrap-wlr-xwayland-surface", p)))
#define WRAP_WLR_BOX(p)                                                    \
  (REF_CALL_1("wlroots util box", "wrap-wlr-box", FROM_P(p)))
#define UNWRAP_WLR_BOX(p)                                                  \
  (TO_P(REF_CALL_1("wlroots util box", "unwrap-wlr-box", p)))
#define WRAP_WLR_IDLE(p)                                                    \
  (REF_CALL_1("wlroots types idle", "wrap-wlr-idle", FROM_P(p)))
#define UNWRAP_WLR_IDLE(p)                                                  \
  (TO_P(REF_CALL_1("wlroots types idle", "unwrap-wlr-idle", p)))
#define WRAP_WLR_EVENT_KEYBOARD_KEY(p) \
  (REF_CALL_1("wlroots types keyboard", "wrap-wlr-event-keyboard-key", FROM_P(p)))
#define UNWRAP_WLR_EVENT_KEYBOARD_KEY(p)                                                  \
  (TO_P(REF_CALL_1("wlroots types keyboard", "unwrap-wlr-event-keyboard-key", p)))

#define WRAP_KEYBOARD(o)                                                        \
  (scm_call_3(REF("oop goops", "make"), REF("gwwm keyboard", "<gwwm-keyboard>"), \
              scm_from_utf8_keyword("data"), FROM_P(o)))
#define UNWRAP_KEYBOARD(o)                                                      \
  (TO_P(scm_call_1(REFP("gwwm keyboard", ".data"), o)))
/* #define SEND_LOG(o ...) */
#define send_log(v,b,...) _send_log(#v,b, ##__VA_ARGS__, "/0")
void
_send_log(const char *arg, ...) {
	va_list ap;
    SCM scm =scm_make_list(scm_from_int(0), SCM_UNSPECIFIED);
    char *para;
    char *para2;

	va_start(ap, arg);
    scm=scm_cons(REF("gwwm utils srfi-215",arg) ,scm);
    scm=scm_cons(scm_from_utf8_string(va_arg(ap, char *)),scm);
    while(1) {
      para=va_arg(ap, char *);
      if ( strcmp( para, "/0") == 0 )
        break;
      para2=va_arg(ap, char *);
      scm=scm_cons2(scm_from_utf8_string(para2),
                    scm_from_utf8_symbol(para),
                    scm);
    }
	va_end(ap);
    scm_apply_0(REF("gwwm utils srfi-215","send-log"), scm_reverse(scm)) ;
}
