# libtoolize: Consider adding '-I m4' to ACLOCAL_AMFLAGS in Makefile.am.
ACLOCAL_AMFLAGS = -I m4

libexec_PROGRAMS = \
	ibus-engine-senn \
	$(NULL)

if SENN_IME_ECL
ibus_engine_senn_SOURCES = \
	main.cpp \
	../../src-cpp/ibus/im/stateful_ime_proxy.cpp \
	../../src-cpp/ibus/im/stateful_ime_ecl.cpp \
	$(NULL)

ibus_engine_senn_LDADD = \
	@IBUS_LIBS@ \
	@GOBJECT2_LIBS@ \
	@GLIB2_LIBS@ \
	@GIO2_LIBS@ \
	@GTHREAD2_LIBS@ \
	-lecl \
	./senn-lib-ibus--all-systems.a \
	$(NULL)

senn-lib-ibus--all-systems.a:
## Intended to be used from docker/ibus-senn
	ecl \
	  -eval '(require :asdf)' \
	  -eval '(push #p"$(shell pwd)/../../" asdf:*central-registry*)' \
	  -eval '(asdf:make-build :senn-lib-ibus :type :static-library :move-here #p"./" :monolithic t :init-name "init_senn")' \
	  -eval '(quit)'
else
if  SENN_IME_SOCKET
ibus_engine_senn_SOURCES = \
	main.cpp \
	../../src-cpp/ibus/im/stateful_ime_proxy.cpp \
	../../src-cpp/ibus/im/stateful_ime_socket.cpp
	../../src-cpp/ipc/socket.cpp \
	../../src-cpp/process/process.cpp \
	$(NULL)

ibus_engine_senn_LDADD = \
	@IBUS_LIBS@ \
	@GOBJECT2_LIBS@ \
	@GLIB2_LIBS@ \
	@GIO2_LIBS@ \
	@GTHREAD2_LIBS@ \
	$(NULL)
endif
endif

ibus_engine_senn_CFLAGS = \
	@GLIB2_CFLAGS@ \
	@GIO2_CFLAGS@ \
	@GTHREAD2_CFLAGS@ \
	-DG_LOG_DOMAIN=\"IBUS\" \
	-Wno-unused-variable \
	-Wno-unused-but-set-variable \
	-Wno-unused-function \
	$(NULL)

ibus_engine_senn_CPPFLAGS = \
	-I/usr/include/ibus-1.0/ \
	-I/usr/include/glib-2.0/ \
	-I/usr/lib/x86_64-linux-gnu/glib-2.0/include \
	-I../../src-cpp/ \
	-std=c++11 \
	-DSENN_IME_@SENN_IME@ \
	$(NULL)

ibus_engine_senn_LDFLAGS = \
	$(NULL)

component_DATA = \
	senn.xml \
	$(NULL)

componentdir = \
	 $(datadir)/ibus/component

CLEANFILES = \
	senn.xml \
	senn-lib-ibus--all-systems.a \
	$(NULL)

senn.xml: senn.xml.in
	$(AM_V_GEN) sed \
		-e 's|@package_version[@]|$(PACKAGE_VERSION)|g' \
		-e 's|@libexecdir[@]|$(libexecdir)|g' $< > $@.tmp && \
		mv $@.tmp $@
