#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([ibus_senn], [0.0.9], [], [ibus-senn])
AM_INIT_AUTOMAKE([1.10 subdir-objects])
AC_CONFIG_SRCDIR([main.cpp])
AC_CONFIG_HEADERS([config.h])

## libtoolize: Consider adding 'AC_CONFIG_MACRO_DIRS([m4])' to configure.ac,
AC_CONFIG_MACRO_DIRS([m4])

# Checks for programs.
AC_PROG_CXX

# Checks for libraries.
m4_define([ibus_required_version], [1.5.0])
m4_define([glib_required_version], [2.26.0])

## Check ibus
PKG_CHECK_MODULES(IBUS, [
    ibus-1.0 >= ibus_required_version
])
## Check glib2.
AM_PATH_GLIB_2_0
PKG_CHECK_MODULES(GLIB2, [
    glib-2.0 >= glib_required_version
])
PKG_CHECK_MODULES(GOBJECT2, [
    gobject-2.0 >= glib_required_version
])
PKG_CHECK_MODULES(GIO2, [
    gio-2.0 gio-unix-2.0 >= glib_required_version
])
PKG_CHECK_MODULES(GTHREAD2, [
    gthread-2.0 >= glib_required_version
])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_TYPE_SIZE_T

# Checks for library functions.
AC_CHECK_FUNCS([setenv])

AC_SUBST([SENN_IME], 'ECL')
AM_CONDITIONAL([SENN_IME_ECL], [true])
AM_CONDITIONAL([SENN_IME_SOCKET], [false])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
