# vim:set noet ts=4:

lib_LTLIBRARIES = \
	libuserdict.la \
	$(NULL)
libuserdict_la_SOURCES = \
	../../../../senn-kkc-engine/hachee/src-cpp/user_dict.cpp  \
	$(NULL)
libdir = /usr/lib/senn/ibus/kkc/

bin_PROGRAMS =  \
	userdict \
	$(NULL)
userdict_SOURCES = \
	../../../../senn-kkc-engine/hachee/src-cpp/user_dict_main.cpp  \
	$(NULL)
userdict_LDADD = \
	libuserdict.la
	$(NULL)
bindir = /usr/lib/senn/ibus/kkc/
