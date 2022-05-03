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


moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/wlroots
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/wlroots

SOURCES = config.scm \
 backend.scm  \
 render/allocator.scm    \
 render/renderer.scm     \
 types.scm               \
 types/output.scm        \
 types/output-layout.scm \
 types/layer-shell.scm   \
 types/input-device.scm  \
 types/seat.scm          \
 types/data-device.scm   \
 types/compositor.scm    \
 types/xdg-shell.scm     \
 types/pointer.scm       \
 types/cursor.scm        \
 types/xcursor.scm       \
 types/scene.scm         \
 types/surface.scm       \
 util/box.scm            \
 util/log.scm            \
 util/addon.scm          \
 utils.scm
