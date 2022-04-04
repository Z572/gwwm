CC=gcc
WAYLAND_PROTOCOLS=$(shell pkg-config --variable=pkgdatadir wayland-protocols)
WAYLAND_SCANNER=$(shell pkg-config --variable=wayland_scanner wayland-scanner)
LIBS=\
	 $(shell pkg-config --cflags --libs wlroots) \
	 $(shell pkg-config --cflags --libs guile-3.0) \
	 $(shell pkg-config --cflags --libs pixman-1) \
	 $(shell pkg-config --cflags --libs wayland-server) \
	 $(shell pkg-config --cflags --libs xkbcommon)

# wayland-scanner is a tool which generates C headers and rigging for Wayland
# protocols, which are specified in XML. wlroots requires you to rig these up
# to your build system yourself and provide them in the include path.
xdg-shell-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

gwwm: gwwm.c xdg-shell-protocol.h
	$(CC) $(CFLAGS) $(LIBS)\
		-g -Werror -I. \
		-DWLR_USE_UNSTABLE \
		-o $@ $< \


clean:
	rm -f gwwm xdg-shell-protocol.h xdg-shell-protocol.c

.DEFAULT_GOAL=gwwm
.PHONY: clean
