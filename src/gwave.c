/*
 * gwave - waveform viewer
 * Copyright (C) 1998, 1999 Stephen G. Tell.
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
 * You should have received a copy of the GNU General Public
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
#include <sys/time.h>

#include <gtk/gtk.h>
#include <config.h>
#include "gwave.h"

/* globals */
char *prog_name = PACKAGE;
char *prog_version = VERSION;
int colors_initialized = 0;
int x_flag, v_flag;
WaveTable *wtable;
const int NWColors = 6;  /* # of wavecolorN styles expected in the .gtkrc */

char *bg_color_name  = "black" ;  /* panel background color */
GdkColor bg_gdk_color;
GdkGC *bg_gdk_gc;

char *pg_color_name  = "grey30" ;  /* panel graticule */
GdkColor pg_gdk_color;
GdkGC *pg_gdk_gc;

GtkWidget *win_main;
GdkColormap *win_colormap; /* colormap for main waveform window */

/* TODO: make these members of the global wtable structure instead of
 * globals in their own right */
GtkAdjustment *win_hsadj;
GtkWidget *win_hsbar;
GtkWidget *win_xlabel_left, *win_xlabel_right;
GtkWidget *win_status_label;

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
style 'wavecolor0' { fg[NORMAL] = {0.4, 0.5, 1.0} }
style 'wavecolor1' { fg[NORMAL] = {1.0, 0.0, 0.0} }
style 'wavecolor2' { fg[NORMAL] = {0.0, 1.0, 0.0} }
style 'wavecolor3' { fg[NORMAL] = {1.0, 1.0, 0.0} }
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

/* 
 * adaptor stub to call cmd_show_wave_list from a g_list_foreach
 */
void fe_show_wave_list(GWDataFile *wdata)
{
	cmd_show_wave_list(NULL, wdata);
}

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

	/* spicestream library messages need more cleanup before this is
	 * anything but annoying.
	 * ss_error_hook = create_message_window; 
	 */
	for(; optind < argc; optind++) {
		if(load_wave_file(argv[optind], filetype) < 0) {
			fprintf(stderr, "unable to read data file: %s\n", argv[optind]);
		}
	}
	if(npanels > 8)
		npanels = 8;

	wtable = g_new0(WaveTable, 1);
	wtable->npanels = npanels;
	wtable->panels = g_new0(WavePanel*, npanels);
	for(i = 0; i < npanels; i++) {
		wtable->panels[i] = g_new0(WavePanel, 1);
	}
	wtable->cursor[0] = g_new0(VBCursor, 1);
	wtable->cursor[1] = g_new0(VBCursor, 1);
	wtable->srange = g_new0(SelRange, 1);

	/* manualy set up the waves into specific WavePanels
	* Now that the gui allows (re)configuration of the wave/panel setup,
	* this doesn't have much use except for testing.
	*/
	if(fillpanels) {
		GWDataFile *wdata = g_list_nth_data(wdata_list, 0);
		if(wdata) {
			for(i = 0; i < wdata->wf->wf_ndv; i++) {
				add_var_to_panel(wtable->panels[i % npanels],
						 &wdata->wf->dv[i]);
			}
		}
	}
	wtable->start_xval = wtable->min_xval;
	wtable->end_xval = wtable->max_xval;

	setup_colors(wtable);
	setup_waveform_window();

	/* this is not enough to get the main window mapped and displayed 
	   so that the wave_list windows can be positioned relative to it.
	   At least by waiting until now to build the wavelist windows, 
	   they will probably end up on top of the main window, instead
	   of under it.*/
	/* gtk_main_iteration();  */
	g_list_foreach(wdata_list, 
		       (GFunc)fe_show_wave_list, NULL);
	gtk_main();
	return EXIT_SUCCESS;
}

void
create_about_window()
{
	GtkWidget *win, *box, *label, *btn;
	char buf[256];

	sprintf(buf, "About %s", PACKAGE);

	win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_name(win, "About GWave");
	gtk_window_set_title(GTK_WINDOW(win), "About Gwave");
	gtk_widget_set_usize(win, 200, 100);

	box = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(win), box);
	gtk_widget_show(box);

	sprintf(buf, "%s version %s", PACKAGE, VERSION);
	label = gtk_label_new(buf);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_widget_show(label);
	gtk_box_pack_start (GTK_BOX (box), label, TRUE, TRUE, 0);

	label = gtk_label_new("By Steve Tell");
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_widget_show(label);
	gtk_box_pack_start (GTK_BOX (box), label, TRUE, TRUE, 0);

	label = gtk_label_new("tell@cs.unc.edu");
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_widget_show(label);
	gtk_box_pack_start (GTK_BOX (box), label, TRUE, TRUE, 0);

	btn = gtk_button_new_with_label ("Close");
	gtk_box_pack_end(GTK_BOX(box), btn, FALSE, TRUE, 0);
	gtk_signal_connect_object (GTK_OBJECT (btn), "clicked",
                                 GTK_SIGNAL_FUNC(gtk_widget_destroy),
                                 GTK_OBJECT (win));
	gtk_widget_show (btn);

	gtk_widget_show(win);
}

/*
 * pop up a window with an information or error message.
 * window stays up until the Close button is clicked.
 */
void
create_message_window(char *s)
{
	GtkWidget *win, *box, *label, *btn;
	char buf[256];

	sprintf(buf, "%s message", PACKAGE);

	win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_name(win, buf);
	gtk_window_set_title(GTK_WINDOW(win), buf);
	gtk_widget_set_usize(win, 200, 100);

	box = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(win), box);
	gtk_widget_show(box);

	label = gtk_label_new(s);
	gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_CENTER);
	gtk_widget_show(label);
	gtk_box_pack_start (GTK_BOX (box), label, TRUE, TRUE, 0);

	btn = gtk_button_new_with_label ("Close");
	gtk_box_pack_end(GTK_BOX(box), btn, FALSE, TRUE, 0);
	gtk_signal_connect_object (GTK_OBJECT (btn), "clicked",
                                 GTK_SIGNAL_FUNC(gtk_widget_destroy),
                                 GTK_OBJECT (win));
	gtk_widget_show (btn);

	gtk_widget_show(win);

}
