/*
 * wavelist.c - part of gwave
 * routines to handle the scrolling list of potentialy-displayable waveforms,
 * and other stuff related to loading of data files.
 *
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
 * $Log: not supported by cvs2svn $
 * Revision 1.3  1998/09/17 18:38:29  tell
 * Added load_wave_file function and other stuff for multiple files.
 * Change variable box packing so it looks better (no fill/expand).
 *
 * Revision 1.2  1998/09/01 21:29:04  tell
 * add copyright notice, misc cleanup
 *
 * Revision 1.1  1998/08/31 20:58:56  tell
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
#include "gtkmisc.h"

char *possible_drag_types[] = {"x-gwave/dvar"};
char *accepted_drop_types[] = {"x-gwave/dvar"};

GList *wdata_list = NULL;
static char file_tag_chars[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static const int n_file_tags = sizeof(file_tag_chars)/sizeof(char);
static int next_file_tagno = 0;

/*
 * Load a waveform file, adding it to the list of files from which
 * variables can be chosed to add to the display.
 */
int
load_wave_file(char *fname, char *ftype)
{
	GWDataFile *wdata;
	int i;

	wdata = g_new0(GWDataFile, 1);
	wdata->df = analog_read_file(fname, ftype);
	
	if(wdata->df == NULL) {
		g_free(wdata);
		return -1;
	}

	/* give the file a short (fow now, 1-character) "tag" to identify it
	 * in the menu and variable labels.  
	 * TODO: let user set the tag if they so choose.
	 */
	wdata->ftag = g_new(char, 2);
	wdata->ftag[0] = file_tag_chars[next_file_tagno];
	wdata->ftag[1] = '\0';
	next_file_tagno = (next_file_tagno + 1) % n_file_tags;

	/* userdata pointer in variable gets backpointer to wdata struct */
	for(i = 0; i < wdata->df->ndv; i++) {
		DVar *dv = wdata->df->dv[i];
		dv->udata = wdata;
	}

	wdata_list = g_list_append(wdata_list, wdata);

	if(var_list_submenu) {
		create_wdata_submenuitem(wdata, var_list_submenu);
	}
	cmd_show_wave_list(NULL, wdata);

	return 0;
}

/*
 * callback triggered when OK is selected in file requester.
 */
static void
file_selection_ok(GtkWidget        *w,
                   GtkFileSelection *fs)
{
	char *fname = gtk_file_selection_get_filename(GTK_FILE_SELECTION (fs));
	/* note: filename only valid until GtkFileSelection widget is destroyed */
	if(load_wave_file(fname, NULL) < 0)
		return;

	gtk_widget_destroy (GTK_WIDGET (fs));
}


/*
 * put up a filename requester, get a file, and load it.
 */
void
get_fname_load_file(GtkWidget *w, gpointer d)
{
	static GtkWidget *window = NULL;
	
	if (!window) {
		window = gtk_file_selection_new ("file selection dialog");

		gtk_file_selection_hide_fileop_buttons (GTK_FILE_SELECTION (window));
		gtk_window_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);

		gtk_signal_connect (GTK_OBJECT (window), "destroy",
				    GTK_SIGNAL_FUNC(gtk_widget_destroyed),
				    &window);
		gtk_signal_connect (GTK_OBJECT(GTK_FILE_SELECTION(window)->ok_button),
				    "clicked",
				    GTK_SIGNAL_FUNC(file_selection_ok), window);
		gtk_signal_connect_object (GTK_OBJECT (GTK_FILE_SELECTION (window)->cancel_button),
					   "clicked", GTK_SIGNAL_FUNC(gtk_widget_destroy),
					   GTK_OBJECT (window));
	}

	if (!GTK_WIDGET_VISIBLE (window))
		gtk_widget_show (window);
	else
		gtk_widget_destroy (window);
}

static void
dnd_drag_request (GtkWidget *button, GdkEvent *event, gpointer d)
{
	DVar *dvar = (DVar *)d;
	GWDnDData dd;

	dd.dv = dvar;

	gtk_widget_dnd_data_set (button, event, 
				 (gpointer)&dd, sizeof(GWDnDData));
}

/*
 * Toggle visibility of the scrolling variable list window for
 * a waveform data file.
 */
void
cmd_show_wave_list(GtkWidget *w, GWDataFile *wdata)
{
	GtkWidget *box1;
	GtkWidget *box2;
        GtkWidget *scrolled_window;
	GtkWidget *button;
	GtkWidget *label;
	int i;

	static GtkWidget *drag_icon = NULL;
	static GtkWidget *drop_icon = NULL;

	if(!wdata) {
		fprintf(stderr, "cmd_show_wave_list: wdata is NULL");
		return;
	}

	if(!wdata->wlist_win) {
		char buf[256];
		GdkPoint icon_hotspot = {5,5};
		if (!drag_icon) {
			drag_icon = shape_create_icon_d (drag_no_xpm,
					 440, 140, 0,0, GTK_WINDOW_POPUP);
	  
			gtk_signal_connect (GTK_OBJECT (drag_icon), "destroy",
				    GTK_SIGNAL_FUNC(gtk_widget_destroyed),
				    &drag_icon);
			gtk_widget_hide (drag_icon);
		}
		if (!drop_icon)	{
			drop_icon = shape_create_icon_d (wave_drag_ok_xpm,
					 440, 140, 0,0, GTK_WINDOW_POPUP);
	  
			gtk_signal_connect (GTK_OBJECT (drop_icon), "destroy",
				    GTK_SIGNAL_FUNC(gtk_widget_destroyed),
				    &drop_icon);
			gtk_widget_hide (drop_icon);
		}

		gdk_dnd_set_drag_shape(drag_icon->window,
				       &icon_hotspot,
				       drop_icon->window,
				       &icon_hotspot);

		wdata->wlist_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
		gtk_widget_set_name(wdata->wlist_win, "data list window");
		sprintf(buf, "gwave: %.64s", wdata->df->filename);
		gtk_window_set_title(GTK_WINDOW(wdata->wlist_win), buf);
		gtk_widget_set_usize(wdata->wlist_win, 150, 300);
		gtk_widget_set_uposition(wdata->wlist_win, 600, 20);

		gtk_signal_connect (GTK_OBJECT (wdata->wlist_win), "destroy",
                          GTK_SIGNAL_FUNC(gtk_widget_destroyed),
                          &(wdata->wlist_win));

		box1 = gtk_vbox_new(FALSE, 0);
		gtk_container_add(GTK_CONTAINER(wdata->wlist_win), box1);
		gtk_widget_show(box1);

		sprintf(buf, "%s: %.64s", wdata->ftag, wdata->df->filename);
		label = gtk_label_new(buf);
		gtk_label_set_justify(GTK_LABEL(label), GTK_JUSTIFY_LEFT);
		gtk_widget_show(label);
		gtk_box_pack_start (GTK_BOX (box1), label, FALSE, FALSE, 0);

		scrolled_window = gtk_scrolled_window_new (NULL, NULL);
		gtk_container_border_width (GTK_CONTAINER (scrolled_window), 10);
		gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
                                  GTK_POLICY_AUTOMATIC, 
                                  GTK_POLICY_AUTOMATIC);
		GTK_WIDGET_UNSET_FLAGS (GTK_SCROLLED_WINDOW (scrolled_window)->vscrollbar, GTK_CAN_FOCUS);
		gtk_box_pack_start(GTK_BOX (box1), scrolled_window,
				   TRUE, TRUE, 0);
		gtk_widget_show (scrolled_window);


		box2 = gtk_vbox_new (FALSE, 0);
		gtk_container_border_width (GTK_CONTAINER (box2), 10);
		gtk_container_add (GTK_CONTAINER(scrolled_window), box2);
		gtk_container_set_focus_vadjustment(GTK_CONTAINER (box2),
						    gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(scrolled_window)));
		gtk_widget_show (box2);

		for(i = 0; i < wdata->df->ndv; i++) {
			DVar *dv = wdata->df->dv[i];
			button = gtk_button_new_with_label(dv->d.name);

			gtk_box_pack_start (GTK_BOX (box2), button, FALSE, FALSE, 0);
			gtk_widget_show (button);

			/*
			 * currently (Gtk+ 1.0.4), the widget has to be 
			 * realized to set dnd on it, according to coments
			 * in testgtk.c.  likely change in future Gtk+...
			 */
			gtk_widget_realize (button);
			gtk_signal_connect (GTK_OBJECT (button),
					    "drag_request_event",
					    GTK_SIGNAL_FUNC(dnd_drag_request),
					    wdata->df->dv[i]);
			gtk_widget_dnd_drag_set (button,
						 TRUE, possible_drag_types, 1);

		}
	}

	if(!GTK_WIDGET_VISIBLE(wdata->wlist_win))
		gtk_widget_show(wdata->wlist_win);
	else
		gtk_widget_destroy(wdata->wlist_win);
}

