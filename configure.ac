#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([gwwm], [0.0.1], [])
AC_CONFIG_SRCDIR([lisp])
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.14 gnu tar-ustar silent-rules subdir-objects \
 color-tests parallel-tests -Woverride -Wno-portability])

AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile
                lisp/wayland/config.scm
                lisp/wlroots/config.scm])

AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
# AC_CONFIG_HEADERS([config.h])


# Checks for programs.
AC_PROG_CC

# Checks for libraries.
dnl m4_pattern_forbid([PKG_CHECK_MODULES])
dnl m4_pattern_forbid([PKG_CHECK_VAR])


GUILE_PKG([3.0 2.2])
GUILE_PROGS
GUILE_SITE_DIR
m4_include([config-c.ac])

# Checks for header files.
dnl AC_CHECK_HEADERS([wayland-server.h])
dnl AC_CHECK_HEADERS([libguile.h])
# Checks for typedefs, structures, and compiler characteristics.
dnl LT_INIT([dlopen])
dnl LT_CONFIG_LTDL_DIR([libltdl])
dnl LTDL_INIT
# Checks for library functions.
GUILE_MODULE_REQUIRED([bytestructures guile])
#AC_CHECK_LIB([guile],[scm_to_pointer])
if test "$cross_compiling" != no; then
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"

AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])
AC_OUTPUT
