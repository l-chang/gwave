# Makefile.am: process this file with automake to produce Makefile.in

EXTRA_DIST=README

noinst_LIBRARIES = libspicefile.a

libspicefile_a_SOURCES = spicestream.c ss_cazm.c ss_hspice.c ss_spice3.c ss_spice2.c spicestream.h wavefile.c wavefile.h spice2.h

CFLAGS = @GTK_CFLAGS@

noinst_PROGRAMS = test_read
test_read_SOURCES =  test_read.c
test_read_LDFLAGS = @GTK_LIBS@
test_read_LDADD = libspicefile.a

bin_PROGRAMS=sp2sp
sp2sp_SOURCES=sp2sp.c
sp2sp_LDFLAGS= @GTK_LIBS@
sp2sp_LDADD= libspicefile.a

