MAIN = main
LIB = senn-lib-fcitx--all-systems.a

.PHONY: test clean

test: $(LIB) $(MAIN)
	./test.sh

$(LIB):
	ros use ecl
## If the folloing error occurs, adding `:prologue-code '(require :asdf)` to asdf:make-build can resolve this.
## Condition of type: SIMPLE-ERROR
## The packages
##   ((UIOP/OS . #<UIOP/OS package>) (UIOP/PATHNAME . #<UIOP/PATHNAME package>))
## were referenced in compiled file
##   NIL
## but have not been created
	ros \
	  -s senn-lib-fcitx \
	  -e "(asdf:make-build :senn-lib-fcitx :type :static-library :monolithic t :init-name \"init_senn\" :move-here \"./\")" \
	  -q

$(MAIN): main.cpp
	g++ \
	  `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --cflags` \
	  -I../../../third-party \
	  -I../../src-cpp \
	  main.cpp \
	  ../../src-cpp/fcitx/im/stateful_ime_ecl.cpp \
	  ../../src-cpp/fcitx/im/stateful_ime_proxy.cpp \
	  $(LIB) \
	  `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --ldflags` \
	  -o $(MAIN)

clean:
	rm -f $(LIB) $(MAIN)
