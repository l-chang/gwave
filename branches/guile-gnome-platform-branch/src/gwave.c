/*
 * gwave - waveform viewer
 * Copyright (C) 1998, 1999, 2000 Stephen G. Tell.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include <ctype.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <sys/time.h>

#include <gtk/gtk.h>
#include <config.h>
#include <scwm_guile.h>
#include "guile-compat.h"

#include <gwave.h>

#include <wavelist.h>
#include <wavewin.h>

extern void init_scwm_guile();
extern void init_gtkmisc();
extern void init_gwave();
extern void init_cmd();
extern void init_wavewin();
extern void init_wavelist();
extern void init_wavepanel();
extern void init_event();
extern void init_draw();

extern void xg_init(void *display);
 
/* globals */
char *prog_name = PACKAGE;
char *prog_version = VERSION;
int colors_initialized = 0;
int v_flag;
int x_flag;
WaveTable *wtable;
const int NWColors = 6;  /* # of wavecolorN styles expected in the .gtkrc */

char *bg_color_name  = "black" ;  /* panel background color */
GdkColor bg_gdk_color;
GdkGC *bg_gdk_gc;

char *pg_color_name  = "grey30" ;  /* panel graticule */
GdkColor pg_gdk_color;
GdkGC *pg_gdk_gc;

char *hl_color_name  = "white" ;  /* panel highlight: select-outline */
GdkColor hl_gdk_color;
GdkGC *hl_gdk_gc;

GdkColormap *win_colormap; /* colormap for main waveform window */


/* variables accessible from both C and guile */

SCM_VARIABLE_INIT(scm_gwave_version, "gwave-version-string",  scm_makfrom0str(VERSION));
/*,
"This variable is initialized to contain the version string for gwave, as"
"set in configure.in."); */

SCM_VARIABLE_INIT(scm_gwave_datadir, "gwave-datadir",  scm_makfrom0str(DATADIR));
/*"This variable is initialized to contain the compiled-in pathname to"
"the installed data directory, typicaly PREFIX/share, as set by configure."
"It is used by the startup code as a default location for finding gwave's"
"guile modules.");*/

SCM_VARIABLE_INIT(scm_gwave_bingwave, "gwave-bin-gwave-path", scm_makfrom0str(BINGWAVE));
/*,
"This variable is initialized to contain the compiled-in pathname to"
"the installed gwave executable, typicaly PREFIX/bin/gwave, as set by configure."
"It is used by the procedures that write out gwave configuration-restoring"
"scripts so that when run from the command line command line, the scripts"
"can use gwave as their interpreter.");*/

SCM_GLOBAL_VARIABLE(scm_gwave_debug, "gwave-debug");
/*"This variable is set to #t very early in gwave's startup when the -x flag"
"is passed on the command line.  It enables debugging output to stdout"
"in the startup code and in various modules.");*/



/* if we don't set up some colors, users without a gwave.gtkrc get black
 * waves on black background
 */
static const gchar *gwave_base_gtkrc = ""
"style 'wavecolor0' { fg[NORMAL] = {1.0, 0.0, 0.0} }"
"style 'wavecolor1' { fg[NORMAL] = {1.0, 1.0, 0.0} }"
"style 'wavecolor2' { fg[NORMAL] = {0.0, 1.0, 0.0} }"
"style 'wavecolor3' { fg[NORMAL] = {0.4, 0.5, 1.0} }"
"style 'wavecolor4' { fg[NORMAL] = {0.0, 1.0, 1.0} }"
"style 'wavecolor5' { fg[NORMAL] = {1.0, 0.0, 1.0} }"
"widget '*wavecolor0' style 'wavecolor0'"
"widget '*wavecolor1' style 'wavecolor1'"
"widget '*wavecolor2' style 'wavecolor2'"
"widget '*wavecolor3' style 'wavecolor3'"
"widget '*wavecolor4' style 'wavecolor4'"
"widget '*wavecolor5' style 'wavecolor5'"
"style 'cursor0color' { fg[NORMAL] = {1.0, 1.0, 1.0} }"
"style 'cursor1color' { fg[NORMAL] = {1.0, 1.0, 0.0} }"
"style 'cursorDcolor' { fg[NORMAL] = {00, 1.0, 0.0} }"
"widget '*cursor1color' style 'cursor1color'"
"widget '*cursor0color' style 'cursor0color'"
"widget '*cursorDcolor' style 'cursorDcolor'"
"style 'wavebutton' { bg[NORMAL] = { 0.25, 0.25, 0.25 } }"
"widget '*wavebutton' style 'wavebutton'"
"";

int
main(int argc, char **argv)
{
	void gwave_main(void *p, int argc, char **argv);
	/* disable the deprecated warnings in guile 1.6; we can't clean them
	   up until we drop support for guile older than 1.6 */
	if (getenv("GUILE_WARN_DEPRECATED") == NULL)
		putenv("GUILE_WARN_DEPRECATED=no");

	scm_boot_guile(argc, argv, gwave_main, NULL);
	return 0;
}

void gwave_main(void *p, int argc, char **argv)
{
	int i;
	int nobacktrace = 0;

	/* In guile-1.5 and later, need to use scm_primitive_eval_x
	 * in order to change modules so that our C primitives
	 * registered below become globals, instead of hidden away
	 * in the guile-user module
	 */
	{
		SCM exp = scm_c_read_string("(define-module (guile))");
		scm_primitive_eval_x(exp);
	}

	init_scwm_guile();
	init_gtkmisc();
	init_gwave();
	init_cmd();
	init_wavewin();
	init_wavelist();
	init_wavepanel();
	init_event();
	init_draw();
	
	gtk_init(&argc, &argv);
	setlocale(LC_NUMERIC, "POSIX");

#ifdef HAVE_G_SLICE_SET_CONFIG
	/* default mode causes frequent crashes; this probably masks it rather
	   than fixing it.  still, its better than crashing */
	if(!getenv("G_SLICE"))
		putenv("G_SLICE=always-malloc");		
#endif

	prog_name = argv[0];

	/* simple pre-processing of debugging options that we need to set up
	 * before we get into guile.   These options cannot be bundled.
	 * Most of the general user options are handled in std-args.scm  */
	for(i = 1; i < argc; i++) {
		if(strcmp(argv[i], "-n") == 0) {
			nobacktrace = 1;
		} else if (strcmp(argv[i], "-v") == 0) {
			v_flag = 1;
		} else if (strcmp(argv[i], "-x") == 0) {
			x_flag = 1;
			SCM_SETCDR(scm_gwave_debug, SCM_BOOL_T);
		}
	}

	gtk_rc_parse_string(gwave_base_gtkrc);
	gtk_rc_parse("gwave.gtkrc");

#ifdef GUILE_GTK_EXTRA_LOADPATH
	scm_c_eval_string("(set! %load-path (cons \"" GUILE_GTK_EXTRA_LOADPATH "\" %load-path))");
#endif

	if (!nobacktrace) {
		scm_c_eval_string("(debug-enable 'debug)(debug-enable 'backtrace) (read-enable 'positions)");
	}

	wtable = g_new0(WaveTable, 1);
	wtable->cursor[0] = g_new0(VBCursor, 1);
	wtable->cursor[1] = g_new0(VBCursor, 1);
	wtable->srange = g_new0(SelRange, 1);
	wtable->npanels = 0;
	wtable->panels = NULL;

	/* the compiled-in initial scheme code comes from minimal.scm,
	   built into init_scheme_string.c by the Makefile
	   Among other things, it finds and loads system and user .gwaverc
	   files.
	*/
	{ /* scope */
		extern char *init_scheme_string;
		SCM res;
		if(v_flag) {fprintf(stderr, "running init_scheme_string\n");}
		res = scwm_safe_eval_str(init_scheme_string);
		if(v_flag) {
			printf("result="); fflush(stdout);
			scm_display(res, scm_cur_outp);
			printf("\n"); fflush(stdout);
		}
                if(!SCM_NFALSEP(res)) {
                        fprintf(stderr, "gwave: aborting due to errors.\n");
                        exit(1);
                }

	} /* end scope */

	setup_colors(wtable);
	setup_waveform_window();

	xg_init(NULL);  /* X-server interprocess communication for Gtk+ */

	gtk_main();
	exit(0);
}

/* guile initialization */
void init_gwave()
{
#ifndef XSCM_MAGIC_SNARF_INITS
#include "gwave.x"
#endif
}
