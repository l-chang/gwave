## $Id: Makefile.am,v 1.6 2001-08-22 23:38:32 sgt Exp $
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

# Lots of this documentation stuff was borrowed from scwm.  It doesn't 
# all work yet; the next big project is a tool to build gwave.sgml
# in part from gwave-procedures.txt etc.
#
# If you have docbook-to-man, you should be able to build
# sp2sp.1 from sp2sp.sgml, however.

noinst_DATA=gwave-procedures.txt gwave-variables.txt gwave-hooks.txt \
	gwave-concepts.txt

man_MANS=sp2sp.1

EXTRA_DIST=Guile.notes HPUX.notes hspice.txt hspice-output.txt sp2sp.sgml sp2sp.1

# Make the reference-document textfiles
gwave-procedures.txt gwave-variables.txt gwave-hooks.txt gwave-concepts.txt:
	$(top_srcdir)/utilities/doc-split --basename gwave $(top_srcdir)/src/*.doc

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

