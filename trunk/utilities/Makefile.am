## $Id: Makefile.am,v 1.4 2000-08-10 04:39:27 sgt Exp $
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

EXTRA_DIST = ChangeLog guile-doc-snarf.in guile-func-name-check.in guile-snarf.awk.in

noinst_SCRIPTS = guile-doc-snarf guile-func-name-check guile-snarf.awk doc-split

all:
	chmod +x $(noinst_SCRIPTS)
