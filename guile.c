#include <libguile.h>
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
  (TO_P(REF_CALL_1("wlroots types xdg-shell", "wrap-wlr-xdg-surface", p)))
void init_scm() {
  scm_primitive_load(
      scm_sys_search_load_path(scm_from_utf8_string("gwwm.scm")));
  scm_primitive_load(
      scm_sys_search_load_path(scm_from_utf8_string("gwwm/startup.scm")));
  scm_call_0(SCM_LOOKUP_REF("parse-command-line"));
}
