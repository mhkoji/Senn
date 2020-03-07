# vim:set et ts=4:
#
m4_define(ibus_maybe_datestamp,
    m4_esyscmd([if test x]ibus_senn_released[ != x1; then date +.%Y%m%d | tr -d '\n\r'; fi]))
m4_define([ibus_abi_current], [5])
m4_define([ibus_abi_revision],
          [m4_eval(100 * ibus_senn_minor_version + ibus_senn_micro_version)])
m4_define([ibus_abi_age], [0])

dnl - if not 1, append datestamp to the version number.
m4_define([ibus_senn_released], [0])
m4_define([ibus_senn_major_version], [0])
m4_define([ibus_senn_minor_version], [0])
m4_define([ibus_senn_micro_version], [1])
m4_define([ibus_senn_version],
    ibus_senn_major_version.ibus_senn_minor_version.ibus_senn_micro_version[]ibus_maybe_datestamp)

m4_define([ibus_required_version], [1.5.0])
m4_define([glib_required_version], [2.26.0])

AC_INIT([ibus-senn], [ibus_senn_version], [], [ibus-senn])
AM_INIT_AUTOMAKE([1.10])
AC_GNU_SOURCE

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])

AC_PROG_CXX
LT_INIT


## To suppress "autoreconf: configure.ac: not using Gettext."
AM_GNU_GETTEXT_VERSION([0.19.8])
AM_GNU_GETTEXT([external])

# Check packages.
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


# AC_PREFIX_DEFAULT(/usr/lib/ibus)

dnl libtool versioning
dnl
dnl If LT_VERSION_INFO="lt_current:lt_revision:lt_age", libibus is
dnl libibus-ibus_api_version.so.(lt_current - lt_age).lt_age.lt_revision
dnl If the abi is changed, but it is compatible with last version,
dnl ibus_abi_current++, ibus_abi_age++;
dnl If the abi is not compatible with last version,
dnl ibus_abi_current++, ibus_abi_age = 0;
m4_define([lt_current], [ibus_abi_current])
m4_define([lt_revision], [ibus_abi_revision])
m4_define([lt_age], [ibus_abi_age])
LT_VERSION_INFO="lt_current:lt_revision:lt_age"
AC_SUBST(LT_VERSION_INFO)

dnl - OUTPUT files
AC_CONFIG_FILES([
Makefile
engine/Makefile
])

AC_OUTPUT
echo "
                IBus-Senn version ibus_senn_version

LT_VERSION_INFO             $LT_VERSION_INFO
"
