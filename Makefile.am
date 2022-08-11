AUTOMAKE_OPTIONS = gnu

procedures_h = wlr-layer-shell-unstable-v1-protocol.h xdg-shell-protocol.h idle-protocol.h
bin_PROGRAMS= gwwm
gwwm_SOURCES=%D%/gwwm.c %D%/client.h %D%/config.h %D%/util.h %D%/util.c %D%/config.h
nodist_gwwm_SOURCES = $(procedures_h)
gwwm_CFLAGS= $(WAYLAND_SERVER_CFLAGS) $(WLROOTS_CFLAGS) $(GUILE_CFLAGS) \
			 $(XCB_CFLAGS) $(XKBCOMMON_CFLAGS) $(LIBINPUT_CFLAGS)       \
			-I. -DWLR_USE_UNSTABLE -DXWAYLAND
gwwm_LDADD= $(WLROOTS_LIBS) $(WAYLAND_SERVER_LIBS) $(XKBCOMMON_LIBS) \
			$(GUILE_LIBS) $(LIBINPUT_LIBS) $(XCB_LIBS)
snarfcppopts =  $(INCLUDES) $(CPPFLAGS) $(gwwm_CFLAGS)
SUFFIXES= .c .scm .go .x

$(gwwm_SOURCES): $(procedures_h)
.c.x:
	guile-snarf -o $@ $< $(snarfcppopts)
CLEANFILES = %D%/*.x $(procedures_h)

BUILT_SOURCES = %D%/gwwm.x $(procedures_h) \
 $(c_sources)

xdg-shell-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

wlr-layer-shell-unstable-v1-protocol.h:
	$(AM_V_GEN) $(WAYLAND_SCANNER) server-header \
    $(top_builddir)/protocols/wlr-layer-shell-unstable-v1.xml $@

idle-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		protocols/idle.xml $@


do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)


#
GOBJECTS = $(SOURCES:%.scm=%.go) $(NODIST_SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(NODIST_SOURCES)
nobase_go_DATA = $(GOBJECTS)

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES += $(GOBJECTS)
GUILE_WARNINGS =                                          \
  -Wunbound-variable -Warity-mismatch -Wshadowed-toplevel \
  -Wmacro-use-before-definition                           \
  -Wunused-variable                                       \
  -Wduplicate-case-datum -Wbad-case-datum                 \
  -Wformat

.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"


moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

run:
	exec $(top_builddir)/pre-inst-env $(top_builddir)/gwwm

include lisp/gwwm/Makefile
