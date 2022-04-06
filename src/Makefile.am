AUTOMAKE_OPTIONS = gnu
BUILT_SOURCES = %D%/gwwm.c
bin_PROGRAMS= gwwm

gwwm_SOURCES=%D%/gwwm.c
# SOURCES+=%D%/gwwm.c
gwwm_CFLAGS= $(WAYLAND_SERVER_CFLAGS) $(WLROOTS_CFLAGS) $(GUILE_CFLAGS) $(XKBCOMMON_CFLAGS) -DWLR_USE_UNSTABLE
gwwm_LDADD= $(WLROOTS_LIBS) $(WAYLAND_SERVER_LIBS) $(XKBCOMMON_LIBS) $(GUILE_LIBS)
# .SURRIXES = .x
# .c.x: guile-snarf -o $@ $< $(snarfcppopts)

xdg-shell-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

wlr-layer-shell-unstable-v1-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
    $(top_builddir)/protocol/wlr-layer-shell-unstable-v1.xml  $@
CLEANFILES+= wlr-layer-shell-unstable-v1-protocol.h xdg-shell-protocol.h
