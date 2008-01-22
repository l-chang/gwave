# Makefile.am: process this file with automake to produce Makefile.in

SUFFIXES = .doc .scm

guiledir=$(datadir)/guile/app/gwave

guile_DATA=system.gwaverc cmds.scm dynlink.scm globals.scm minimal.scm \
	std-args.scm std-menus.scm std-toolbar.scm gtk-helpers.scm \
	extra-menus.scm visiblewave-ops.scm gwave-startup.scm \
	utils.scm export.scm export-gnugraph.scm export-gnuplot.scm \
	gwave-config.scm

DOT_DOC_FILES=cmds.doc export.doc

BUILT_SOURCES=$(DOT_DOC_FILES)

EXTRA_DIST=$(guile_DATA)

.scm.doc:
	guile-tools doc-snarf --lang scheme $<  > $@
