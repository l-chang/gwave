## $Id: Makefile.am,v 1.6 2003-12-24 20:42:48 sgt Exp $
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

EXTRA_DIST = ChangeLog gwave-doc-snarf.in guile-doc-snarf \
	guile-func-name-check.in doc-split.in 

noinst_SCRIPTS = gwave-doc-snarf guile-doc-snarf \
	guile-func-name-check doc-split 

all:
	chmod +x $(noinst_SCRIPTS)
