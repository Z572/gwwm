do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

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


moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

SOURCES= lisp/gwwm/util.scm \
 lisp/wlroots/backend.scm             \
 lisp/wlroots/config.scm              \
 lisp/wlroots/render/allocator.scm    \
 lisp/wlroots/render/renderer.scm     \
 lisp/wlroots/types/data-device.scm   \
 lisp/wlroots/types/output-layout.scm \
 lisp/wlroots/types/scene.scm         \
 lisp/wlroots/types/xdg-shell.scm     \
 lisp/wlroots/util/box.scm            \
 lisp/wlroots/utils.scm               \
 lisp/wayland.scm                     \
 lisp/wayland/argument.scm        \
 lisp/wayland/callback.scm        \
 lisp/wayland/client.scm          \
 lisp/wayland/client-core.scm     \
 lisp/wayland/compositor.scm      \
 lisp/wayland/config.scm          \
 lisp/wayland/cursor.scm          \
 lisp/wayland/display.scm         \
 lisp/wayland/egl.scm             \
 lisp/wayland/event-loop.scm      \
 lisp/wayland/global.scm          \
 lisp/wayland/interface.scm       \
 lisp/wayland/keyboard.scm        \
 lisp/wayland/list.scm            \
 lisp/wayland/listener.scm        \
 lisp/wayland/output.scm          \
 lisp/wayland/pointer.scm         \
 lisp/wayland/proxy.scm           \
 lisp/wayland/registry.scm        \
 lisp/wayland/resource.scm        \
 lisp/wayland/server-core.scm     \
 lisp/wayland/shm.scm             \
 lisp/wayland/signal.scm          \
 lisp/wayland/touch.scm           \
 lisp/wayland/util.scm
include src/Makefile.am
