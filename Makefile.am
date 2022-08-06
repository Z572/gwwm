AUTOMAKE_OPTIONS = gnu
xdg-shell-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

wlr-layer-shell-unstable-v1-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
    $(top_builddir)/protocol/wlr-layer-shell-unstable-v1.xml $@

idle-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		protocols/idle.xml $@

config.h:
	cp config.def.h $@

bin_PROGRAMS= gwwm
gwwm_SOURCES=%D%/gwwm.c %D%/client.h %D%/config.h %D%/util.h %D%/util.c
gwwm_CFLAGS= $(WAYLAND_SERVER_CFLAGS) $(WLROOTS_CFLAGS) $(GUILE_CFLAGS) \
			 $(XCB_CFLAGS) $(XKBCOMMON_CFLAGS) $(LIBINPUT_CFLAGS)       \
			-I. -DWLR_USE_UNSTABLE -DXWAYLAND
gwwm_LDADD= $(WLROOTS_LIBS) $(WAYLAND_SERVER_LIBS) $(XKBCOMMON_LIBS) \
			$(GUILE_LIBS) $(LIBINPUT_LIBS) $(XCB_LIBS)
snarfcppopts =  $(INCLUDES) $(CPPFLAGS) $(gwwm_CFLAGS)
SUFFIXES= .c .scm .go .x
.c.x:
	guile-snarf -o $@ $< $(snarfcppopts)
CLEANFILES = %D%/*.x
BUILT_SOURCES = wlr-layer-shell-unstable-v1-protocol.h %D%/gwwm.x xdg-shell-protocol.h idle-protocol.h \
 $(c_sources)

do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
