AUTOMAKE_OPTIONS = gnu
xdg-shell-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

wlr-layer-shell-unstable-v1-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
    $(top_builddir)/protocol/wlr-layer-shell-unstable-v1.xml $@

CLEANFILES += *.h
bin_PROGRAMS= gwwm

gwwm_SOURCES=%D%/gwwm.c


# SOURCES+=%D%/gwwm.c
gwwm_CFLAGS= $(WAYLAND_SERVER_CFLAGS) $(WLROOTS_CFLAGS) $(GUILE_CFLAGS) $(XKBCOMMON_CFLAGS) -I. -DWLR_USE_UNSTABLE
gwwm_LDADD= $(WLROOTS_LIBS) $(WAYLAND_SERVER_LIBS) $(XKBCOMMON_LIBS) $(GUILE_LIBS)


lib_LTLIBRARIES = libguile-wayland-server.la
libguile_wayland_server_la_SOURCES = %D%/w-s.c
libguile_wayland_server_la_LIBADD = $(WAYLAND_SERVER_LIBS) $(GUILE_LIBS)
libguile_wayland_server_la_LDFLAGS = -module $(GUILE_LDFLAGS)
libguile_wayland_server_la_DEPENDENCIES = $(LTLIBOBJS)
libguile_wayland_server_la_CPPFLAGS = $(CFLAGS) $(GUILE_CFLAGS)



snarfcppopts =  $(INCLUDES) $(CPPFLAGS) $(gwwm_CFLAGS)
SUFFIXES += .c .x
.c.x:
	guile-snarf -o $@ $< $(snarfcppopts)
CLEANFILES += *.x %D%/*.x
c_sources = %D%/gwwm.c %D%/w-s.c
BUILT_SOURCES = wlr-layer-shell-unstable-v1-protocol.h xdg-shell-protocol.h %D%/w-s.c $(c_sources)

BUILT_SOURCES += $(c_sources:%.c=%.x)

