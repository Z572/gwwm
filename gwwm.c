/*
 * See LICENSE file for copyright and license details.
 */
#define _POSIX_C_SOURCE 200809L
#include "wlr-layer-shell-unstable-v1-protocol.h"
#include "wlr/util/box.h"
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
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
#include <wlr/render/allocator.h>
#include <wlr/render/wlr_renderer.h>
#include <wlr/types/wlr_compositor.h>
#include <wlr/types/wlr_cursor.h>
#include <wlr/types/wlr_data_control_v1.h>
#include <wlr/types/wlr_data_device.h>
#include <wlr/types/wlr_export_dmabuf_v1.h>
#include <wlr/types/wlr_gamma_control_v1.h>
#include <wlr/types/wlr_idle_notify_v1.h>
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
#include <wlr/types/wlr_virtual_keyboard_v1.h>
#include <wlr/types/wlr_xcursor_manager.h>
#include <wlr/types/wlr_xdg_activation_v1.h>
#include <wlr/types/wlr_xdg_decoration_v1.h>
#include <wlr/types/wlr_xdg_output_v1.h>
#include <wlr/types/wlr_xdg_shell.h>
#include <wlr/util/log.h>
#include <xkbcommon/xkbcommon.h>
#include "util.h"

#include "gwwm.h"
/* configuration, allows nested code to access above variables */

SCM_DEFINE (gwwm_focusmon ,"focusmon",1,0,0,(SCM a),"" )
#define FUNC_NAME s_gwwm_focusmon
{
  PRINT_FUNCTION;
  int i = 0, nmons = scm_to_int(scm_length((REF_CALL_0("gwwm monitor", "monitor-list"))));
  if (nmons)
    do /* don't switch to disabled mons */
      scm_call_1(REFP("gwwm monitor","set-current-monitor"),
                 REF_CALL_1("gwwm monitor" ,"dirtomon",a));
    while (!((struct wlr_output *)
                                   (UNWRAP_WLR_OUTPUT(scm_call_1
                                                      (REFP("gwwm monitor", "monitor-output"),
                                                       (REF_CALL_0("gwwm monitor", "current-monitor"))))))->enabled
           && i++ < nmons);
  REF_CALL_2("gwwm","focusclient",
             REF_CALL_1("gwwm commands", "focustop",
                        (REF_CALL_0("gwwm monitor", "current-monitor"))),
             SCM_BOOL_T);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_init_gwwm(void)
{
#ifndef SCM_MAGIC_SNARFER
#include "gwwm.x"
#endif
}
