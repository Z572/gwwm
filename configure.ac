AC_INIT([gwwm], [0.0.1], [873216071@qq.com])
AC_CONFIG_SRCDIR([gwwm])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile po/Makefile.in])
AC_CONFIG_FILES([gwwm/configuration.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

AC_PROG_CC
GUILE_PKG([3.0 2.2])
GUILE_PROGS
GUILE_SITE_DIR

dnl AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"

AC_USE_SYSTEM_EXTENSIONS
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.21])
LT_INIT([dlopen])
dnl m4_pattern_allow([AC_LIB_LINKFLAGS_FROM_LIBS])
PKG_CHECK_MODULES([GUILE], [guile-3.0])
PKG_CHECK_MODULES([WAYLAND_SERVER], [wayland-server])
PKG_CHECK_MODULES([WLROOTS], [wlroots >= 0.17.0])
PKG_CHECK_MODULES([LIBINPUT],[libinput])
dnl PKG_CHECK_MODULES([WAYLAND_SCANNER], [wayland-scanner])
PKG_CHECK_MODULES([PIXMAN_1], [pixman-1])
PKG_CHECK_MODULES([XKBCOMMON],[xkbcommon])
PKG_CHECK_MODULES([XCB],[xcb])
#AC_CHECK_LIB()
PKG_CHECK_VAR([WLROOTS_LIBDIR],[wlroots],[libdir])
PKG_CHECK_VAR([WAYLAND_LIBDIR],[wayland-server],[libdir])
PKG_CHECK_VAR([WAYLAND_SCANNER],[wayland-scanner],[wayland_scanner])
PKG_CHECK_VAR([WAYLAND_PROTOCOLS],[wayland-protocols],[pkgdatadir])
PKG_CHECK_VAR([LIBINPUT_LIBDIR],[libinput] ,[libdir])
PKG_CHECK_VAR([XCB_LIBDIR],[xcb] ,[libdir])
AC_SUBST([WLROOTS_LIBDIR])
AC_SUBST([WAYLAND_LIBDIR])
AC_SUBST([XCB_LIBDIR])
GUILE_MODULE_REQUIRED([bytestructures guile])
GUILE_MODULE_REQUIRED([srfi srfi-189])
GUILE_MODULE_REQUIRED([libinput])
GUILE_MODULE_REQUIRED([wayland server display])
GUILE_MODULE_REQUIRED([wlroots])
GUILE_MODULE_REQUIRED([util572 color])

AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])
AC_OUTPUT
