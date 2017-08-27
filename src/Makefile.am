# -*- mode: Makefile -*-
## Process this file with automake to produce Makefile.in

SUBDIRS=app

INCLUDES=-I$(top_srcdir) -I$(top_srcdir)/spicefile -I$(top_srcdir)/src

bin_PROGRAMS = gwave

gwave_SOURCES = cmd.c wavewin.c draw.c gwave.c event.c \
	gtkmisc.c pixmaps.c wavelist.c dnd.c dnd.h \
	scwm_guile.c guile-compat.c init_scheme_string.c \
	gwave.h gtkmisc.h wavewin.h wavelist.h  wavepanel.c \
	guile-compat.h arg_unused.h scwm_guile.h validate.h  \
	rgeval.c xgserver.c measurebtn.c measurebtn.h \
	GtkTable_indel.c GtkTable_indel.h  xsnarf.h

gwave_LDADD = ../spicefile/libspicefile.a  @GTK_LIBS@ @GUILE_GNOME_LIBS@ 
gwave_LDFLAGS = -lm @GUILE_LDFLAGS@

AM_CFLAGS = @GTK_CFLAGS@ @GUILE_CFLAGS@ @GUILE_GNOME_CFLAGS@ \
	-DDATADIR=\"$(datadir)\" -DBINGWAVE=\"$(bindir)/gwave\"

DOT_X_FILES = gwave.x cmd.x wavewin.x wavelist.x scwm_guile.x event.x \
	draw.x gtkmisc.x wavepanel.x

DOT_DOC_FILES = gwave.doc cmd.doc wavewin.doc wavelist.doc scwm_guile.doc \
	event.doc draw.doc

BUILT_SOURCES=init_scheme_string.c $(DOT_X_FILES) 

#$(DOT_DOC_FILES)

init_scheme_string.c: $(top_srcdir)/scheme/minimal.scm
	echo "char *init_scheme_string = " > $@
	sed -e 's/;.*$$//g' -e 's/"/\\"/g' -e "s/'/\\\\'/g" -e 's/$$/\\n"/' -e 's/^/"/g' < $(top_srcdir)/scheme/minimal.scm >> $@
	echo >> $@
	echo ';' >> $@

SUFFIXES = .x .doc

utils = $(top_srcdir)/utilities

.c.x:
	guile-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS)  $(AM_CFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

.c.doc:
	guile-tools doc-snarf $< > $@

$(DOT_DOC_FILES): $(utils)/gwave-doc-snarf xsnarf.h

MOSTLYCLEANFILES = core.* *~
CLEANFILES = core.* *~ $(BUILT_SOURCES)
DISTCLEANFILES = core.* *~
MAINTAINERCLEANFILES = core.* *~

EXTRA_DIST=$(noinst_SCRIPTS) gwave.gtkrc
