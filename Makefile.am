do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

SUFFIXES = .scm .go

include src/Makefile.am
SUBDIRS =lisp/wayland/ lisp/wlroots/ lisp/gwwm/
