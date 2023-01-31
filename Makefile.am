AUTOMAKE_OPTIONS = gnu

protocols_h = xdg-shell-protocol.h wlr-layer-shell-unstable-v1-protocol.h idle-protocol.h
protocols=  $(top_builddir)/protocols/wlr-layer-shell-unstable-v1.xml \
			$(top_builddir)/protocols/idle.xml

lib_LTLIBRARIES= libgwwm.la
# bin_PROGRAMS= gwwm
libgwwm_la_SOURCES= %D%/gwwm.c \
					%D%/gwwm.h \
					%D%/client.h \
					%D%/client.c \
					%D%/config.h \
					%D%/util.h \
					%D%/util.c \
					%D%/config.h
nodist_libgwwm_la_SOURCES = $(protocols_h)
libgwwm_la_CFLAGS= $(WAYLAND_SERVER_CFLAGS) $(WLROOTS_CFLAGS) $(GUILE_CFLAGS) \
			 $(XCB_CFLAGS) $(XKBCOMMON_CFLAGS) $(LIBINPUT_CFLAGS)       \
			-I. -DWLR_USE_UNSTABLE -DXWAYLAND -Og -Wall -Wextra
libgwwm_la_LIBADD= $(WLROOTS_LIBS) $(WAYLAND_SERVER_LIBS) $(XKBCOMMON_LIBS) \
			$(GUILE_LIBS) $(LIBINPUT_LIBS) $(XCB_LIBS)
snarfcppopts =  $(INCLUDES) $(CPPFLAGS) $(libgwwm_la_CFLAGS)
SUFFIXES= .c .scm .go .x .xml -protocol.h

$(protocols_h): $(protocols)
$(libgwwm_la_SOURCES): $(protocols_h)
$(SOURCES): $(lib_LTLIBRARIES)
.c.x:
	guile-snarf -o $@ $< $(snarfcppopts)
CLEANFILES = %D%/*.x $(protocols_h)

BUILT_SOURCES = %D%/gwwm.x %D%/client.x $(protocols_h) \
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

SUBDIRS=po
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
nobase_guilemodule_DATA = gwwm.scm \
		gwwm/color.scm   \
		gwwm/monitor.scm \
		gwwm/layout.scm  \
		gwwm/layout/tile.scm  \
		gwwm/layout/monocle.scm  \
		gwwm/client.scm  \
		gwwm/keymap.scm  \
		gwwm/keybind.scm \
		gwwm/keyboard.scm \
		gwwm/i18n.scm \
		gwwm/hooks.scm \
		gwwm/utils.scm    \
		gwwm/utils/srfi-215.scm    \
		gwwm/utils/ref.scm \
		gwwm/keys.scm    \
		gwwm/listener.scm    \
		gwwm/commands.scm \
		gwwm/packages/fullscreen-bg.scm \
		gwwm/configuration.scm \
		gwwm/config.scm

dist_bin_SCRIPTS=bin/gwwm
bin/gwwm: bin/gwwm.in Makefile
	$(AM_V_at)rm -f $@ $@-t
	$(AM_V_at)$(MKDIR_P) "$(@D)"
	$(AM_V_GEN)$(do_subst) < "$(srcdir)/$@.in" > "$@-t"
	$(AM_V_at)chmod a+x,a-w "$@-t" && mv -f "$@-t" "$@"

# nobase_go_DATA = $(GOBJECTS)
nobase_guileobject_DATA=$(nobase_guilemodule_DATA:%.scm=%.go)
SOURCES=$(nobase_guilemodule_DATA)

guile_install_go_files = install-nobase_guileobject_DATA
$(guile_install_go_files): install-nobase_guilemodule_DATA

CLEANFILES += $(GOBJECTS)
GUILE_WARNINGS =                                          \
  -Wunbound-variable -Warity-mismatch -Wshadowed-toplevel \
  -Wmacro-use-before-definition                           \
  -Wunused-variable                                       \
  -Wduplicate-case-datum -Wbad-case-datum                 \
  -Wformat

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"
.PHONY: run
run: all
	exec $(top_builddir)/pre-inst-env $(top_srcdir)/bin/gwwm

TESTS = tests/keymap.scm tests/utils.scm tests/utils/ref.scm
TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
  $(top_srcdir)/build-aux/test-driver.scm
AM_SCM_LOG_DRIVER_FLAGS = --brief=no
AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"
AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"
CLEANFILES += $(TESTS:tests/%.scm=%.log)

gdb: all
	exec $(top_builddir)/pre-inst-env gdb guile -ex "run $(top_builddir)/bin/gwwm"
