# -*- mode: Makefile -*-
## Process this file with automake to produce Makefile.in

SUBDIRS=app

INCLUDES=-I$(top_srcdir)/spicefile

bin_PROGRAMS = gwave

gwave_SOURCES = cmd.c wavewin.c draw.c gwave.c event.c \
	gtkmisc.c pixmaps.c wavelist.c dnd.c \
	scwm_guile.c guile-compat.c init_scheme_string.c print.c \
	gwave.h gtkmisc.h wavewin.h wavelist.h  wavepanel.c \
	guile-compat.h arg_unused.h scwm_guile.h validate.h guile-ext.h \
	rgeval.c xgserver.c measurebtn.c measurebtn.h \
	GtkTable_indel.c 

gwave_LDADD = ../spicefile/libspicefile.a  @GTK_LIBS@ @GUILEGTK_LIBS@ 
gwave_LDFLAGS =  @GUILE_LDFLAGS@

CFLAGS = @CFLAGS@ @GTK_CFLAGS@ @GUILE_CFLAGS@ @GUILEGTK_CFLAGS@ \
	-DDATADIR=\"$(datadir)\" -DBINGWAVE=\"$(bindir)/gwave\" @ggtk_hack_cflags@

DOT_X_FILES = gwave.x cmd.x wavewin.x wavelist.x scwm_guile.x event.x \
	draw.x print.x gtkmisc.x wavepanel.x

DOT_DOC_FILES = gwave.doc cmd.doc wavewin.doc wavelist.doc scwm_guile.doc \
	event.doc draw.doc print.doc

BUILT_SOURCES=init_scheme_string.c scm_init_funcs.h \
	$(DOT_X_FILES) $(DOT_DOC_FILES)

init_scheme_string.c: $(top_srcdir)/scheme/minimal.scm
	echo "char *init_scheme_string = \"\\" > $@
	sed -e 's/;.*$$//g' -e 's/"/\\"/g' -e "s/'/\\\\'/g" -e 's/$$/\\n\\/' < $(top_srcdir)/scheme/minimal.scm >> $@
	echo >> $@
	echo '";' >> $@

SUFFIXES = .x .doc

utils = $(top_srcdir)/utilities

.c.x:
	utildir=$(utils) $(utils)/guile-doc-snarf $< $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }
.c.doc:
	utildir=$(utils) $(utils)/guile-doc-snarf $< $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > /dev/null \
	|| { rm $@; false; }

scm_init_funcs.h: $(top_srcdir)/config.status $(DOT_X_FILES)
	@echo "Generating scm_init_funcs.h..."
	@$(srcdir)/generate_scm_init_funcs $(DOT_X_FILES) > $@

MOSTLYCLEANFILES = core *~
CLEANFILES = core *~ $(BUILT_SOURCES)
DISTCLEANFILES = core *~
MAINTAINERCLEANFILES = core *~

EXTRA_DIST=gwave.gtkrc generate_scm_init_funcs
