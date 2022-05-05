# vim:set noet ts=4:

lib_LTLIBRARIES = \
	libuserdict.la \
	$(NULL)
libdir = /usr/lib/senn/ibus/kkc/
libuserdict_la_SOURCES = \
	../../../../senn-kkc-engine/hachee/src-cpp/user_dict.cpp  \
	$(NULL)

bin_PROGRAMS =  \
	engine \
	userdict \
	$(NULL)
bindir = /usr/lib/senn/ibus/kkc/

userdict_SOURCES = \
	../../../../senn-kkc-engine/hachee/src-cpp/user_dict_main.cpp  \
	$(NULL)
userdict_LDADD = \
	libuserdict.la
	$(NULL)

engine$(EXEEXT):
	ros use sbcl
	ros dump executable ../../../../senn-kkc-engine/hachee/ros/main-lm.ros -o $@
