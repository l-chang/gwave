# Makefile.am: process this file with automake to produce Makefile.in

EXTRA_DIST=README
AM_CFLAGS = @GTK_CFLAGS@ 
bin_PROGRAMS=gwave-exec gwaverepl
gwave_exec_SOURCES=gwave-exec.c xgclient.c xgexec.h
gwave_exec_LDADD= @GTK_LIBS@

gwaverepl_SOURCES=gwaverepl.c xgclient.c xgexec.h repllib.c
gwaverepl_LDADD= @GTK_LIBS@ @READLINE_LIB@
