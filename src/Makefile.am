# -*- mode: Makefile -*-
## Process this file with automake to produce Makefile.in

INCLUDES=-I$(top_srcdir)/spicefile

bin_PROGRAMS = gwave

gwave_SOURCES = cmd.c gtkmisc.h wavewin.c draw.c gwave.c event.c gwave.h \
	gtkmisc.c pixmaps.c wavelist.c dnd.c \
	scwm_guile.c guile-compat.c init_scheme_string.c print.c
gwave_LDADD = $(top_srcdir)/spicefile/libspicefile.a  @GTK_LIBS@ @GUILEGTK_LIBS@ 
gwave_LDFLAGS =  @GUILE_LDFLAGS@

CFLAGS = @CFLAGS@ @GTK_CFLAGS@ @GUILE_CFLAGS@ @GUILEGTK_CFLAGS@ \
	-DDATADIR=\"$(datadir)\" @ggtk_hack_cflags@

guile_snarfs = cmd.x wavewin.x wavelist.x scwm_guile.x event.x draw.x print.x

##BUILT_SOURCES = init_scheme_string.c scm_init_funcs.h $(guile_snarfs)

init_scheme_string.c: $(top_srcdir)/scheme/minimal.scm
	echo "char *init_scheme_string = \"\\" > $@
	sed -e 's/;.*$$//g' -e 's/"/\\"/g' -e "s/'/\\\\'/g" -e 's/$$/\\n\\/' < $(top_srcdir)/scheme/minimal.scm >> $@
	echo >> $@
	echo '";' >> $@

SUFFIXES = .x

%.x: %.c
	$(srcdir)/scwm-snarf $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $< > $@ \
	|| { rm $@; false; }

scm_init_funcs.h: $(top_srcdir)/config.status $(guile_snarfs)
	@echo "Generating scm_init_funcs.h..."
	@$(srcdir)/generate_scm_init_funcs $(guile_snarfs) > $@

MOSTLYCLEANFILES = core *~
CLEANFILES = core *~ $(BUILT_SOURCES)
DISTCLEANFILES = core *~
MAINTAINERCLEANFILES = core *~

EXTRA_DIST=gwave.gtkrc

