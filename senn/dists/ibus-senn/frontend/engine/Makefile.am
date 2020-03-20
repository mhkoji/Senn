# vim:set noet ts=4:

AUTOMAKE_OPTIONS = \
	subdir-objects \
	$(NULL)

AM_CPPFLAGS = \
  -I/usr/include/ibus-1.0/ \
  -I/usr/include/glib-2.0/ \
  -I/usr/lib/x86_64-linux-gnu/glib-2.0/include \
	-I../../../../third-party/ \
  -I../../../../src/cpp/ \
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
  ../../../../src/cpp/ipc/ipc.cpp \
  ../../../../src/cpp/ipc/request.cpp \
  ../../../../src/cpp/senn_ibus/stateful_im_proxy_ipc.cpp \
  ../../../../src/cpp/senn_fcitx/stateful_im_proxy_ipc_server.cpp \
	$(NULL)
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
