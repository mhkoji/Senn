## libtoolize: Consider adding '-I m4' to ACLOCAL_AMFLAGS in Makefile.am.
ACLOCAL_AMFLAGS = -I m4

bin_PROGRAMS = kkc-engine
bindir = $(libdir)/senn/

kkc-engine$(EXEEXT):
## Intended to be used from docker/senn
	sbcl \
	  --dynamic-space-size 2048 \
	  --noinform \
	  --no-userinit \
	  --no-sysinit \
	  --non-interactive \
	  --eval '(require :asdf)' \
	  --eval '(push #p"$(srcdir)/../../hachee/" asdf:*central-registry*)' \
	  --eval '(push #p"$(srcdir)/../../senn-ipc/" asdf:*central-registry*)' \
	  --eval '(push #p"$(srcdir)/../kkc-engine/hachee/" asdf:*central-registry*)' \
	  --eval '(asdf:load-system :senn-kkc-engine-hachee-class-ngram)' \
	  --eval '(senn-kkc-engine.hachee.engine.class-ngram:set-kkc-2gram "$(srcdir)/kkc-engine-data/")' \
	  --eval "(sb-ext:save-lisp-and-die \"kkc-engine\" :toplevel #'senn-kkc-engine.hachee.engine.class-ngram:main :executable t)"
