/*
 * wavelist.c - part of gwave
 * routines to handle the scrolling list of potentialy-displayable waveforms
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

static GtkWidget *wlist_win;

static void
dnd_drag_request (GtkWidget *button, GdkEvent *event, gpointer d)
{
	DVar *dvar = (DVar *)d;

	gtk_widget_dnd_data_set (button, event, 
				 (gpointer)&dvar, sizeof(gpointer));
}

void
cmd_show_wave_list(GtkWidget *widget)
{
	GtkWidget *box1;
	GtkWidget *box2;
        GtkWidget *scrolled_window;
	GtkWidget *button;
	GtkWidget *label;
	int i;

	static GtkWidget *drag_icon = NULL;
	static GtkWidget *drop_icon = NULL;

	if(!wlist_win) {
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

		wlist_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
		gtk_widget_set_name(wlist_win, "data list window");
		gtk_widget_set_usize(wlist_win, 150, 300);
		gtk_widget_set_uposition(wlist_win, 20, 20);

		gtk_signal_connect (GTK_OBJECT (wlist_win), "destroy",
                          GTK_SIGNAL_FUNC(gtk_widget_destroyed),
                          &wlist_win);

		box1 = gtk_vbox_new(FALSE, 0);
		gtk_container_add(GTK_CONTAINER(wlist_win), box1);
		gtk_widget_show(box1);

		label = gtk_label_new(waveData->df->filename);
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

		for(i = 0; i < waveData->df->ndv; i++) {
			DVar *dv = waveData->df->dv[i];
			button = gtk_button_new_with_label(dv->d.name);

			gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
			gtk_widget_show (button);

			/*
			 * currently (Gtk+ 1.0.4), the widget has to be 
			 * realized to set dnd on it.  this should change.
			 */
			gtk_widget_realize (button);
			gtk_signal_connect (GTK_OBJECT (button),
					    "drag_request_event",
					    GTK_SIGNAL_FUNC(dnd_drag_request),
					    waveData->df->dv[i]);
			gtk_widget_dnd_drag_set (button,
						 TRUE, possible_drag_types, 1);

		}
	}

	if(!GTK_WIDGET_VISIBLE(wlist_win))
		gtk_widget_show(wlist_win);
	else
		gtk_widget_destroy(wlist_win);
}

