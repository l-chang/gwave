## $Id: Makefile.am,v 1.2 2000-08-08 06:41:49 sgt Exp $
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

# Lots of this documentation stuff was borrowed from scwm.  It doesn't 
# all work yet.

gwavedocDATA=gwave-procedures.txt gwave-variables.txt gwave-hooks.txt gwave-concepts.txt gwave.sgml
EXTRA_DIST=Guile.notes HPUX.notes hspice.txt spice.txt sp2sp.stml $(gwavedocDATA)

# Use scwmdoc to build the documentation files
# Set envvar $SCWMDOC_TEST_OPTIONS to  "-F" for a faster test
gwave.sgml gwave-procedures.txt gwave-variables.txt gwave-hooks.txt gwave-concepts.txt : $(gwave_SOURCES)
	abs_builddir_doc=`pwd`; \
	outdir=$$abs_builddir_doc; \
	echo Outputting to directory: $$outdir; \
	cd $(top_builddir); abs_top_builddir=`pwd`; \
	cd $$abs_builddir_doc; cd $(top_srcdir); \
	abs_top_srcdir=`pwd`; \
	echo 'make[99]: Entering directory `'$$abs_top_srcdir"'"; \
	perl $$abs_top_builddir/utilities/scwmdoc $$SCWMDOC_TEST_OPTIONS -o $$outdir/gwave.sgml -O $$outdir/gwave-variables.txt -H $$outdir/gwave-hooks.txt -N $$outdir/gwave-concepts.txt </dev/null $$GWAVEDOC_ARGS src/*.c scheme/*.scm > $$outdir/gwave-procedures.txt; \
	echo 'make[99]: Leaving directory `'$$abs_top_srcdir"'"

html: gwave.sgml
	mkdir -p html && cd html && \
	jade -t sgml -d $(DOCBOOK_HOME)/html/docbook.dsl ../$<

tex gwave.tex: gwave.sgml
	jade -t tex -d $(DOCBOOK_HOME)/print/docbook.dsl $< || true

dvi gwave.dvi: gwave.tex
	jadetex $<

ps gwave.ps: gwave.dvi
	dvicmps -o gwave.ps $<

pdf gwave.pdf: gwave.sgml
	db2pdf $<

SUFFIXES=.sgml .1 .5

.sgml.1:
	docbook-to-man $*.sgml > $@

.PHONY: html tex dvi ps pdf

