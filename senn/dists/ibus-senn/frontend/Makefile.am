# vim:set noet ts=4:

SUBDIRS = \
	engine \
	$(NULL)

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = \
	autogen.sh \
	$(NULL)

noinst_DIST = \
	$(NULL)

DISTCLEANFILES = \
	$(NULL)

# dist-hook:
# 	if test -d .git ; then \
# 		git log --name-status --date=iso > $(distdir)/ChangeLog ; \
# 		PREV_MICRO_VERSION=`expr $(PACKAGE_VERSION_MICRO) - 1`; \
# 		PREV_VERSION=\
# "$(PACKAGE_VERSION_MAJOR).$(PACKAGE_VERSION_MINOR).$$PREV_MICRO_VERSION"; \
# 		echo "Changes in $(PACKAGE_NAME) $(PACKAGE_VERSION)" \
# 		        > $(distdir)/NEWS; \
# 		echo "" >> $(distdir)/NEWS; \
# 		git shortlog $$PREV_VERSION...$(PACKAGE_VERSION) \
# 			>> $(distdir)/NEWS; \
# 		echo "" >> $(distdir)/NEWS; \
# 		git log $$PREV_VERSION...$(PACKAGE_VERSION) --reverse \
# 		        --pretty=format:'%s (%an) %h' >> $(distdir)/NEWS; \
# 	fi

