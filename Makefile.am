AUTOMAKE_OPTIONS = gnu

protocols_h = xdg-shell-protocol.h wlr-layer-shell-unstable-v1-protocol.h idle-protocol.h
protocols=  $(top_builddir)/protocols/wlr-layer-shell-unstable-v1.xml \
			$(top_builddir)/protocols/idle.xml

bin_PROGRAMS= gwwm
gwwm_SOURCES=%D%/gwwm.c %D%/client.h %D%/config.h %D%/util.h %D%/util.c %D%/config.h %D%/guile.h
nodist_gwwm_SOURCES = $(protocols_h)
gwwm_CFLAGS= $(WAYLAND_SERVER_CFLAGS) $(WLROOTS_CFLAGS) $(GUILE_CFLAGS) \
			 $(XCB_CFLAGS) $(XKBCOMMON_CFLAGS) $(LIBINPUT_CFLAGS)       \
			-I. -DWLR_USE_UNSTABLE -DXWAYLAND
gwwm_LDADD= $(WLROOTS_LIBS) $(WAYLAND_SERVER_LIBS) $(XKBCOMMON_LIBS) \
			$(GUILE_LIBS) $(LIBINPUT_LIBS) $(XCB_LIBS)
snarfcppopts =  $(INCLUDES) $(CPPFLAGS) $(gwwm_CFLAGS)
SUFFIXES= .c .scm .go .x .xml -protocol.h

$(gwwm_SOURCES): $(protocols_h)
.c.x:
	guile-snarf -o $@ $< $(snarfcppopts)
CLEANFILES = %D%/*.x $(protocols_h)

BUILT_SOURCES = %D%/gwwm.x $(protocols) \
 $(c_sources)
xdg-shell-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

wlr-layer-shell-unstable-v1-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
     $(top_builddir)/protocols/wlr-layer-shell-unstable-v1.xml $@

idle-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
	$(top_builddir)/protocols/idle.xml $@


do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

EXTRA_DIST = $(SOURCES)  $(protocols)


#
GOBJECTS = $(SOURCES:%.scm=%.go) $(NODIST_SOURCES:%.scm=%.go)

moddir=$(guilemoduledir)/gwwm
godir=$(guileobjectdir)/gwwm
nodist_mod_DATA = lisp/gwwm/color.scm   \
		lisp/gwwm/config.scm  \
		lisp/gwwm/monitor.scm \
		lisp/gwwm/client.scm  \
		lisp/gwwm/keymap.scm  \
		lisp/gwwm/keybind.scm \
		lisp/gwwm/keyboard.scm \
		lisp/gwwm/hooks.scm \
		lisp/gwwm/utils.scm    \
		lisp/gwwm/utils/srfi-215.scm    \
		lisp/gwwm/keys.scm    \
		lisp/gwwm/commands.scm \
		lisp/gwwm/startup.scm

nodist_go_DATA = $(GOBJECTS)
nodist_guilemodule_DATA=lisp/gwwm.scm
nodist_guileobject_DATA=$(nodist_guilemodule_DATA:%.scm=%.go)
SOURCES=$(nodist_mod_DATA) $(nodist_guilemodule_DATA)

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nodist_modDATA install-nobase_guilemoduleDATA

CLEANFILES += $(GOBJECTS)
GUILE_WARNINGS =                                          \
  -Wunbound-variable -Warity-mismatch -Wshadowed-toplevel \
  -Wmacro-use-before-definition                           \
  -Wunused-variable                                       \
  -Wduplicate-case-datum -Wbad-case-datum                 \
  -Wformat

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

run: all
	exec $(top_builddir)/pre-inst-env $(top_builddir)/gwwm
