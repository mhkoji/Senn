#!/bin/sh
# Run this to generate all the initial makefiles, etc.

touch NEWS README AUTHORS ChangeLog

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="ibus-engine-senn"

(test -f $srcdir/configure.ac) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

which gnome-autogen.sh || {
    echo "Not found gnome-autogen.sh. You may need to install gnome-common"
    exit 1
}

CFLAGS=${CFLAGS-"-Wall -Wformat -Werror=format-security"}

# need --enable-gtk-doc for gnome-autogen.sh to make dist
ACLOCAL_FLAGS="$ACLOCAL_FLAGS -I m4" REQUIRED_AUTOMAKE_VERSION=1.11 CFLAGS="$CFLAGS" . gnome-autogen.sh "$@"
