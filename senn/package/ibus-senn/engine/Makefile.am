# vim:set noet ts=4:

AUTOMAKE_OPTIONS = \
	subdir-objects \
	$(NULL)

AM_CPPFLAGS = \
	-I/usr/include/ibus-1.0/ \
	-I/usr/include/glib-2.0/ \
	-I/usr/lib/x86_64-linux-gnu/glib-2.0/include \
	-I../../../third-party/ \
	-I../../../src-cpp/ \
	-std=c++11 \
	$(NULL)

AM_CFLAGS = \
	@GLIB2_CFLAGS@ \
	@GIO2_CFLAGS@ \
	@GTHREAD2_CFLAGS@ \
	-DG_LOG_DOMAIN=\"IBUS\" \
	-Wno-unused-variable \
	-Wno-unused-but-set-variable \
	-Wno-unused-function \
	$(NULL)

# AM_LDFLAGS = \
# 	../dep-ecl/senn-lib-ibus--all-systems.a \
# 	-L/usr/lib/senn/ \
# 	-Wl,-rpath /usr/lib/senn/ \
# 	-lecl \
# 	$(NULL)

# AM_LDFLAGS = \
# 	-lsenn_ibus \
# 	-L/usr/lib/senn/ \
# 	-Wl,-R /usr/lib/senn/ \
# 	$(NULL)

AM_LDADD = \
	@IBUS_LIBS@ \
	@GOBJECT2_LIBS@ \
	@GLIB2_LIBS@ \
	@GIO2_LIBS@ \
	@GTHREAD2_LIBS@ \
	$(NULL)

libexec_PROGRAMS = \
	ibus-engine-senn \
	$(NULL)

ibus_engine_senn_SOURCES = \
	main.cpp \
	../../../src-cpp/ipc/ipc.cpp \
	../../../src-cpp/process/process.cpp \
	../../../src-cpp/ibus/im/stateful_ime_proxy.cpp \
	../../../src-cpp/ibus/im/stateful_ime_exec.cpp \
	$(NULL)
#	../../../src-cpp/ibus/im/stateful_ime_ipc.cpp \
#	../../../src-cpp/ibus/im/stateful_ime_sbcl.cpp \
#	../../../src-cpp/ibus/im/stateful_ime_ecl.cpp
ibus_engine_senn_CFLAGS = \
	$(AM_CFLAGS) \
	$(NULL)
ibus_engine_senn_LDADD = \
	$(AM_LDADD) \
	$(NULL)
# ibus_engine_senn_DEPENDENCIES = \
# 	$(NULL)

component_DATA = \
	senn.xml \
	$(NULL)

# Locations to which components and binaries are installed.
libexecdir = /usr/lib/ibus
componentdir = /usr/share/ibus/component

CLEANFILES = \
	senn.xml \
	$(NULL)

EXTRA_DIST = \
	senn.xml.in \
	$(NULL)

senn.xml: senn.xml.in
	$(AM_V_GEN) sed \
		-e 's|@package_version[@]|$(PACKAGE_VERSION)|g' \
		-e 's|@libexecdir[@]|$(libexecdir)|g' $< > $@.tmp && \
		mv $@.tmp $@

-include $(top_srcdir)/git.mk
