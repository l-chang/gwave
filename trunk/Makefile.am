# -*- mode: Makefile -*-
## Process this file with automake to produce Makefile.in

SUBDIRS=spicefile src scheme

EXTRA_DIST=autogen.sh

# distribute some extra directories that don't have anything to build 
# or install in them
dist-hook:
	mkdir $(distdir)/doc
	rsync -C -p doc/* $(distdir)/doc
	mkdir $(distdir)/examples
	rsync -C -p examples/* $(distdir)/examples
	mkdir $(distdir)/icons
	rsync -C -p icons/* $(distdir)/icons
