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

#ifndef SCM_MAGIC_SNARFER
#include <scm_init_funcs.h>
#endif

#include <gwave.h>

#include <wavelist.h>
#include <wavewin.h>
 
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

GdkColormap *win_colormap; /* colormap for main waveform window */

/* TODO: make these members of the global wtable structure instead of
 * globals in their own right */
GtkAdjustment *win_hsadj;
GtkWidget *win_hsbar;
GtkWidget *win_xlabel_left, *win_xlabel_right;
GtkWidget *win_status_label;
GtkTooltips *gwave_tooltips;


/* variables accessible from C and guile */

SCM_VCELL_INIT(scm_gwave_version, "gwave-version-string",  gh_str02scm(VERSION),
"This variable is initialized to contain the version string for gwave, as
set in configure.in.");

SCM_VCELL_INIT(scm_gwave_datadir, "gwave-datadir",  gh_str02scm(DATADIR),
"This variable is initialized to contain the compiled-in pathname to
the installed data directory, typicaly PREFIX/share, as set by configure.
It is used by the startup code as a default location for finding gwave's
guile modules.");

SCM_VCELL(scm_gwave_debug, "gwave-debug",
"This variable is set to #t very early in gwave's startup when the -x flag
is passed on the command line.  It enables debugging output to stdout
in the startup code and in various modules.");

SCM_GLOBAL_VCELL(scm_gwave_tooltips, "gwave-tooltips",
"This variable is a GtkTooltips object used for controlling all
of the popup tooltips in the user interface.");

/*
 * usage -- prints the standard switch info, then exits.
 *
 * TODO: coordinate C- and guile-parsed options
 */
static void usage(char *fmt, ...)
{
	va_list args;
	fflush(stdout);
	if (fmt) {
		va_start(args, fmt);
		fprintf(stderr, "%s: ", prog_name);
		vfprintf(stderr, fmt, args);
		fprintf(stderr, "\n");
		va_end(args);
	}
	fprintf(stderr, "Usage: %s [options] [initial-waveform-file] ...\n", prog_name);
	fprintf(stderr, " options:\n");
	fprintf(stderr, " -p N     Start up with N panels\n");
	fprintf(stderr, "(%s version %s)\n", prog_name, prog_version);
}

/* if we don't set up some colors, users without a gwave.gtkrc get black
 * waves on black background
 */
static const gchar *gwave_base_gtkrc = "
style 'wavecolor0' { fg[NORMAL] = {1.0, 0.0, 0.0} }
style 'wavecolor1' { fg[NORMAL] = {1.0, 1.0, 0.0} }
style 'wavecolor2' { fg[NORMAL] = {0.0, 1.0, 0.0} }
style 'wavecolor3' { fg[NORMAL] = {0.4, 0.5, 1.0} }
style 'wavecolor4' { fg[NORMAL] = {0.0, 1.0, 1.0} }
style 'wavecolor5' { fg[NORMAL] = {1.0, 0.0, 1.0} }
widget '*wavecolor0' style 'wavecolor0'
widget '*wavecolor1' style 'wavecolor1'
widget '*wavecolor2' style 'wavecolor2'
widget '*wavecolor3' style 'wavecolor3'
widget '*wavecolor4' style 'wavecolor4'
widget '*wavecolor5' style 'wavecolor5'
style 'wavebutton' { bg[NORMAL] = { 0.25, 0.25, 0.25 } }
widget '*wavebutton' style 'wavebutton'
";

int
main(int argc, char **argv)
{
	void gwave_main(int argc, char **argv);
	scwm_gh_enter(argc, argv, gwave_main);
	return 0;
}

void gwave_main(int argc, char **argv)
{
	int c;
	extern int optind;
	extern char *optarg;
	int errflg = 0;
	int nobacktrace = 0;
	int i;

	SCM_REDEFER_INTS;
	init_scwm_guile();
	init_gtkmisc();
	init_gwave();
	init_cmd();
	init_wavewin();
	init_wavelist();
	init_event();
	init_draw();
	init_print();
	gh_allow_ints();
	
	gtk_init(&argc, &argv);

	prog_name = argv[0];
	while ((c = getopt (argc, argv, "np:vx")) != EOF) {
		switch(c) {
		case 'n':
			nobacktrace = 1;
			break;
		case 'v':
			v_flag = 1;
			break;
		case 'x':
			x_flag = 1;
			SCM_SETCDR(scm_gwave_debug, SCM_BOOL_T);
			break;
		case 'p':
			break;
		default:
			errflg = 1;
			break;
		}
	}
	if(errflg) {
		usage(NULL);
	}

	gtk_rc_parse_string(gwave_base_gtkrc);
	gtk_rc_parse("gwave.gtkrc");
	gwave_tooltips = gtk_tooltips_new();
	assert( SCM_CONSP(scm_gwave_tooltips) );

/*	SCM_SETCDR(scm_gwave_tooltips, sgtk_wrap_gtkobj(GTK_OBJECT(gwave_tooltips))); */

#ifdef GUILE_GTK_EXTRA_LOADPATH
	gh_eval_str("(set! %load-path (cons \"" GUILE_GTK_EXTRA_LOADPATH "\" %load-path))");
#endif

	/* the default for this seems to have changed between guile-1.3
	   and guile-1.3.2;  only the first clause is needed when 
	   we drop support for guile-1.3.2 */
	if (!nobacktrace) {
		gh_eval_str("(debug-enable 'debug)(debug-enable 'backtrace) (read-enable 'positions)");
	} /* else {
 		gh_eval_str("(debug-disable 'debug)(read-disable 'positions)");
		}*/

	/* the compiled-in initial scheme code comes from minimal.scm,
	   built into init_scheme_string.c by the Makefile
	   Among other things, it finds and loads system and user .gwaverc
	   files.
	*/
	{ /* scope */
		extern char *init_scheme_string;
		if(v_flag) {fprintf(stderr, "running init_scheme_string\n");}
		scwm_safe_eval_str(init_scheme_string);
	} /* end scope */

	wtable = g_new0(WaveTable, 1);
	wtable->cursor[0] = g_new0(VBCursor, 1);
	wtable->cursor[1] = g_new0(VBCursor, 1);
	wtable->srange = g_new0(SelRange, 1);
	wtable->npanels = 0;
	wtable->panels = NULL;

	setup_colors(wtable);
	setup_waveform_window();

	gtk_main();
	exit(0);
}

/* guile initialization */
void init_gwave()
{
#ifndef SCM_MAGIC_SNARFER
#include "gwave.x"
#endif
}
