dnl Process this file with autoconf to produce a configure script.
dnl ## Initialize autoconf ##

AC_INIT(src/gwave.c)
AM_INIT_AUTOMAKE(gwave2, 20070515)
AM_MAINTAINER_MODE
AM_CONFIG_HEADER(config.h)

dnl Use ANSI mode
AM_PROG_CC_STDC

dnl ## Checks for programs ##
AC_PROG_CC
AC_PROG_RANLIB
AC_PROG_LN_S
AC_PATH_PROGS(PERL, perl perl5, no)
AC_SUBST(PERL)

# this tries to find GNU graph only, not traditional berkeley/sysv graph,
# so we do the right thing on solaris, where an unusable one is in /usr/bin
# and the graph we want is usually in /usr/local/bin or some such.
AC_ARG_WITH(graph, [full pathname to GNU graph for postscript plotting])
dnl AC_MSG_RESULT(with_graph is $with_graph)
if test "$with_graph" != no; then
	if test "$with_graph" != yes && test ! -z "$with_graph" ; then
		AC_DEFINE_UNQUOTED(PROG_GRAPH, "$with_graph", [If present, the pathname of graph, from GNU plotutils.])
	else
	    	AC_PROG_GREPSTDOUT(GRAPH, graph --version, GNU, $ac_dir/$ac_word)
		AC_DEFINE_UNQUOTED(PROG_GRAPH, "$GRAPH", [If present, the pathname of graph, from GNU plotutils.])
	fi
fi

dnl do we need libtool? 
dnl even though we aren't building shared libraries, it seemed to help on HP-UX
dnl AM_PROG_LIBTOOL

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

dnl Checks for library functions.
AC_FUNC_MEMCMP

dnl check for posix regular expression routines
AC_CHECK_HEADER([regex.h], [AC_CHECK_FUNC([regcomp], [AC_DEFINE(HAVE_POSIX_REGEXP,1,[defined if we have POSIX regular expression routines])])])

AC_CHECK_HEADERS([sys/types.h])

dnl check for GTK+
AM_PATH_GTK_2_0(2.0.8, AC_DEFINE(HAVE_GTK2,1,defined if we have a new enough GTK2), AC_MSG_ERROR(Can not find GTK+ 2.0.8 or later on this system))

dnl check for readline library
AC_CHECK_LIB(termcap, tgoto, [
        AC_CHECK_LIB(readline, readline, [
        READLINE_LIB="-lreadline -ltermcap"
        AC_DEFINE(HAVE_READLINE,1,[Define this if you have the readline library])
        AC_CHECK_LIB(readline, add_history, AC_DEFINE(HAVE_HISTORY,1,[Define this if your readline also has add_history]),,"-ltermcap"
)
        ], READLINE_LIB="", "-ltermcap")  
], AC_CHECK_LIB(ncurses, tgoto, [
        AC_CHECK_LIB(readline, readline, [
                READLINE_LIB="-lreadline -lncurses"
                AC_DEFINE(HAVE_READLINE,1,[Define this if you have the readline library])
                AC_CHECK_LIB(readline, add_history, AC_DEFINE(HAVE_HISTORY,1,[Define this if your readline also has add_history]),,"-l
ncurses")
        ], READLINE_LIB="", "-lncurses")
], READLINE_LIB=""))

AC_SUBST(READLINE_LIB)


dnl check for guile
GUILE_FLAGS

dnl checks for guile version-specific features, if any


dnl check for guile-gnome-platform

GUILE_MODULE_REQUIRED(gnome-0)
dnl GUILE_MODULE_REQUIRED(gnome gtk)

PKG_CHECK_MODULES(GUILE_GNOME, guile-gnome-gtk-0, ,AC_MSG_ERROR(Can not find Guile-gnome-platform))

dnl doesn't work if prefix isn't specified.
dnl AC_DEFINE_UNQUOTED(DATADIR,"${datadir}",Installation prefix for finding necessary guile code)

AC_OUTPUT(Makefile \
src/Makefile spicefile/Makefile scheme/Makefile \
src/app/Makefile \
remote/Makefile \
utilities/Makefile \
utilities/gwave-doc-snarf \
utilities/doc-split \
utilities/sweepsplit \
gwave.spec  \
scheme/gwave-config.scm)