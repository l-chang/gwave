## $Id: Makefile.am,v 1.1 2000-05-09 06:31:20 sgt Exp $
## process this file with automake to create Makefile.in
## (You should run ./autogen.sh from the top level directory)

# The whole point of this Makefile and target is to create
# symlinks so that when GWAVE_GUILE_DIR=<gwave/src directory>
# it turns out that $GWAVE_GUILE_DIR/app/gwave points to the
# scheme directory.  All this so that we can run without installing.

noinst_DATA = gwave

DISTCLEANFILES = gwave

gwave: ${top_srcdir}/scheme
	if [ "$(LN_S)" = "ln -s" ]; then \
		$(LN_S) ${top_srcdir}/scheme ./gwave ; \
	else \
		mkdir ./gwave ; \
		(cd gwave ; $(LN_S) ${top_srcdir}/scheme/*.scm .) ; \
	fi
