# vim:set noet ts=4:

SUBDIRS = \
	engine \
	kkc \
	$(NULL)

install-data-hook:
##  dh_auto_install
##       make -j2 -O install DESTDIR=/home/koji/git/Hachee/senn/package/ibus-senn/debian/ibus-senn AM_UPDATE_INFO_DIR=no
##  DESTDIR can be used according the log below.
	install -d                                           ${DESTDIR}/usr/lib/senn/ibus/ecl/lib/
	install -p /usr/lib/senn/ibus/ecl/lib/libecl.so.21.2 ${DESTDIR}/usr/lib/senn/ibus/ecl/lib/
	cp      -r /usr/lib/senn/ibus/ecl/lib/ecl-21.2.1     ${DESTDIR}/usr/lib/senn/ibus/ecl/lib/
# Make sure that the kkc directory is created.
	install -d            ${DESTDIR}/usr/lib/senn/ibus/kkc/
	install -p kkc/engine ${DESTDIR}/usr/lib/senn/ibus/kkc/
# https://bugs.launchpad.net/sbcl/+bug/310108
	chmod   -x ${DESTDIR}/usr/lib/senn/ibus/kkc/engine
