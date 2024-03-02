bin_PROGRAMS = engine-sbcl
bindir = $(libdir)/senn/kkc/

engine-sbcl$(EXEEXT):
## Intended to be used from docker/senn
	sbcl \
	  --dynamic-space-size 2048 \
	  --noinform \
	  --no-userinit \
	  --no-sysinit \
	  --non-interactive \
	  --eval '(require :asdf)' \
	  --eval '(push #p"$(shell pwd)/../../../hachee/" asdf:*central-registry*)' \
	  --eval '(push #p"$(shell pwd)/../../../senn-ipc/" asdf:*central-registry*)' \
	  --eval '(push #p"$(shell pwd)/../../kkc-engine/hachee/" asdf:*central-registry*)' \
          --eval '(asdf:load-system :senn-kkc-engine-hachee-class-2-gram)' \
	  --eval '(senn-kkc-engine.hachee.engine.class-2-gram:set-kkc "./data-class-2-gram/")' \
	  --eval "(sb-ext:save-lisp-and-die \"engine-sbcl\" :toplevel #'senn-kkc-engine.hachee.engine.class-2-gram:main :executable t)"
