/*
 * gwave - waveform viewer
 * Copyright (C) 1998  University of North Carolina at Chapel Hill
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
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.10  1998/09/30 21:42:13  tell
 * Add stuff for zoom-window, and update version to 0.0.4
 *
 * Revision 1.9  1998/09/17 18:36:56  tell
 * Changed default "blue" color for better contrast.
 * Support for multiple files.
 *
 * Revision 1.8  1998/09/01 21:27:43  tell
 * Move most all functions out into seperate, more managably-sized files
 *
 * Revision 1.7  1998/08/31 20:56:10  tell
 * Various things as we rename from wv to gwave, implement drag-and-drop
 * for adding signals to WavePanels, and finish making the file-reader handle
 * multiple formats.
 *
 * Revision 1.6  1998/08/26 19:13:58  tell
 * handles multiple VisibleWaves per WavePanel, and
 * can delete waveforms (but not add them)
 *
 * Revision 1.5  1998/08/25 21:17:25  tell
 * Revised handling of multiple waveform colors, now gets them from styles
 * on the waveform-label widgets, which are set from the wv.gtkrc.
 *
 * Revision 1.4  1998/08/25 17:29:31  tell
 * Support for multiple panels
 *
 * Revision 1.3  1998/08/25 13:49:47  tell
 * added support for second vertical-bar cursor
 *
 * Revision 1.2  1998/08/24 17:48:03  tell
 * Convert to table for arranging wavepanel and labels
 * Got basic Y labels working, although not arranged quite perfectly
 * Now reads a wv.gtkrc file
 * Added basic status-label to show times, etc.
 *
 * Revision 1.1  1998/08/21 19:11:38  tell
 * Initial revision
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
#include <sys/time.h>

#include <gtk/gtk.h>

#include "reader.h"
#include "gwave.h"

/* globals */
char *prog_name = "gwave";
char *prog_version = "0.0.5";
int colors_initialized = 0;
int x_flag, v_flag;
WaveTable *wtable;
const int NWColors = 6;  /* # of wavecolorN styles expected in the .gtkrc */

char *bg_color_name  = "black" ;
GdkColor bg_gdk_color;
GdkGC *bg_gdk_gc;

GtkAdjustment *win_hsadj;
GtkWidget *win_main;
GtkWidget *win_hsbar;
GtkWidget *win_xlabel_left, *win_xlabel_right;
GtkWidget *win_status_label;
GdkColormap *win_colormap; /* colormap for main waveform window */

/*
 * usage -- prints the standard switch info, then exits.
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
	fprintf(stderr, " -p N     Use N panels\n");
	fprintf(stderr, " -t T     Specify that files are of type T\n");
	fprintf(stderr, "(%s version %s)\n", prog_name, prog_version);
	exit(EXIT_FAILURE);
}

/* if we don't set up some colors, users without a gwave.gtkrc get black
 * waves on black background
 */
static const gchar *gwave_base_gtkrc = "
style wavecolor0 { fg[NORMAL] = {0.4, 0.5, 1.0} }
style wavecolor1 { fg[NORMAL] = {1.0, 0.0, 0.0} }
style wavecolor2 { fg[NORMAL] = {0.0, 1.0, 0.0} }
style wavecolor3 { fg[NORMAL] = {1.0, 1.0, 0.0} }
style wavecolor4 { fg[NORMAL] = {0.0, 1.0, 1.0} }
style wavecolor5 { fg[NORMAL] = {1.0, 0.0, 1.0} }
widget '*wavecolor0' style wavecolor0
widget '*wavecolor1' style wavecolor1
widget '*wavecolor2' style wavecolor2
widget '*wavecolor3' style wavecolor3
widget '*wavecolor4' style wavecolor4
widget '*wavecolor5' style wavecolor5
style 'wavebutton' { bg[NORMAL] = { 0.25, 0.25, 0.25 } }
widget '*wavebutton' style 'wavebutton'
";

int main(int argc, char **argv)
{
	int c;
	extern int optind;
	extern char *optarg;
	char *filetype = NULL;
	int errflg = 0;
	int fillpanels = 0;
	int npanels = 2;
	int i;

	gtk_init(&argc, &argv);

	prog_name = argv[0];
	while ((c = getopt (argc, argv, "fp:t:vx")) != EOF) {
		switch(c) {
		case 'f':
			fillpanels = 1;
			break;
		case 'p':
			npanels = atoi(optarg);
			break;
		case 't':
			filetype = optarg;
			break;
		case 'v':
			v_flag = 1;
			break;
		case 'x':
			x_flag = 1;
			break;
		default:
			errflg = 1;
			break;
		}
	}
	if(errflg) {
		usage(NULL);
		exit(1);
	}

	gtk_rc_parse_string(gwave_base_gtkrc);
	gtk_rc_parse("gwave.gtkrc");

	for(; optind < argc; optind++) {
		if(load_wave_file(argv[optind], filetype) < 0) {
			fprintf(stderr, "unable to read data file: %s\n", argv[optind]);
		}
	}
	if(npanels > 8)
		npanels = 8;

	wtable = g_new0(WaveTable, 1);
	wtable->npanels = npanels;
	wtable->panels = g_new0(WavePanel, npanels);
	wtable->cursor[0] = g_new0(VBCursor, 1);
	wtable->cursor[1] = g_new0(VBCursor, 1);
	wtable->srange = g_new0(SelRange, 1);

	/* manualy set up the waves into specific WavePanels
	* Now that the gui allows (re)configuration of the wave/panel setup,
	* this doesn't have much use except for quicker testing things.
	*/
	if(fillpanels) {
		GWDataFile *wdata = g_list_nth_data(wdata_list, 0);
		if(wdata) {
			for(i = 0; i < wdata->df->ndv; i++) {
				add_var_to_panel(&wtable->panels[i % npanels],
						 wdata->df->dv[i]);
			}
		}
	}
	wtable->start_xval = wtable->min_xval;
	wtable->end_xval = wtable->max_xval;

	setup_colors(wtable);
	setup_waveform_window();

	gtk_main();
	return EXIT_SUCCESS;
}
