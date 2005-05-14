# -*- mode: Makefile -*-
## Process this file with automake to produce Makefile.in

SUBDIRS=spicefile utilities src scheme doc remote

EXTRA_DIST=autogen.sh gwave.spec TODO FAQ plotutils-2.4.1.patch

# distribute some extra directories that don't have anything to build 
# or install in them
dist-hook:
	mkdir $(distdir)/examples
	rsync -C -p examples/* $(distdir)/examples
	mkdir $(distdir)/icons
	rsync -C -p icons/* $(distdir)/icons

# build RPMs - but only on a redhat system
rpm:	$(distdir).tar.gz
	cp gwave.spec $(distdir).tar.gz /usr/src/redhat/SOURCES
	cd /usr/src/redhat/SOURCES; rpmbuild -ba gwave.spec
