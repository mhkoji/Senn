fcitx-engine: engine.cpp utf8_fcitx.cpp
	g++ -o $@ $^ -lanthy -lfcitx-utils -I ../../senn/third-party/

ibus-engine: engine.cpp utf8_ibus.cpp
	g++ -o $@ $^ -lanthy -I ../../senn/third-party/ `pkg-config --libs glib-2.0` `pkg-config --cflags glib-2.0`
