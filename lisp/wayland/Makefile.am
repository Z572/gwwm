GOBJECTS = $(SOURCES:%.scm=%.go) $(NODIST_SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(NODIST_SOURCES)
nobase_go_DATA = $(GOBJECTS)

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

CLEANFILES = $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS =                                          \
  -Wunbound-variable -Warity-mismatch -Wshadowed-toplevel \
  -Wmacro-use-before-definition                           \
  -Wunused-variable                                       \
  -Wduplicate-case-datum -Wbad-case-datum                 \
  -Wformat

SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"


moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/wayland
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/wayland

SOURCES = ../wayland.scm \
 argument.scm        \
 callback.scm        \
 client.scm          \
 client-core.scm     \
 compositor.scm      \
 config.scm          \
 cursor.scm          \
 display.scm         \
 egl.scm             \
 event-loop.scm      \
 global.scm          \
 interface.scm       \
 keyboard.scm        \
 list.scm            \
 listener.scm        \
 output.scm          \
 pointer.scm         \
 proxy.scm           \
 registry.scm        \
 resource.scm        \
 server-core.scm     \
 shm.scm             \
 signal.scm          \
 touch.scm           \
 util.scm
