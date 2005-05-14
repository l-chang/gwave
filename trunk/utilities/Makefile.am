## $Id: Makefile.am,v 1.7 2005-05-14 20:33:22 sgt Exp $
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

EXTRA_DIST = ChangeLog gwave-doc-snarf.in guile-doc-snarf \
	guile-func-name-check.in doc-split.in 

bin_SCRIPTS=sweepsplit

noinst_SCRIPTS = gwave-doc-snarf guile-doc-snarf \
	guile-func-name-check doc-split 

all:
	chmod +x $(noinst_SCRIPTS)
