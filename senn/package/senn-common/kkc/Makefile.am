bin_PROGRAMS = engine
bindir = /usr/lib/senn/kkc/

engine$(EXEEXT):
	sbcl \
	  --dynamic-space-size 2048 \
	  --noinform \
	  --no-userinit \
	  --no-sysinit \
	  --non-interactive \
	  --eval '(require :asdf)' \
	  --eval '(push #p"/build/engine/" asdf:*central-registry*)' \
	  --eval '(push #p"/build/hachee/" asdf:*central-registry*)' \
	  --eval '(push #p"/build/senn-ipc/" asdf:*central-registry*)' \
	  --eval '(asdf:load-system :senn-kkc-engine-hachee-class-2-gram)' \
	  --eval '(senn-kkc-engine.hachee.engine.class-2-gram:set-kkc "./data-class-2-gram/")' \
	  --eval "(sb-ext:save-lisp-and-die \"engine\" :toplevel #'senn-kkc-engine.hachee.engine.class-2-gram:main :executable t)"
