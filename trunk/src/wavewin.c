/*
 * wavewin.c, part of the gwave waveform viewer tool
 *
 * Functions in this file set up the main waveform window GUI.
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

/*
 * vw_wp_create_button -- called from g_list_foreach to 
 * create button and label widgets for one VisibleWave in a WavePanel
 */
void
vw_wp_create_button(VisibleWave *vw, WavePanel *wp)
{
	char lbuf[128];

	if(wtable->cursor[0]->shown) {
		double dval = an_interp_value(vw->var, wtable->cursor[0]->xval);
		sprintf(lbuf, "%.15s %.3f", vw->var->d.name, dval);
	} else {
		sprintf(lbuf, "%.15s      ", vw->var->d.name);
	}
	vw->label = gtk_label_new(lbuf);
	vw->button = gtk_toggle_button_new();
	gtk_container_add(GTK_CONTAINER(vw->button), vw->label);
	gtk_box_pack_start(GTK_BOX(wp->lvbox), vw->button,
			   FALSE, FALSE, 0);
	sprintf(lbuf, "wavecolor%d", vw->colorn);
	gtk_widget_set_name(vw->label, lbuf);
	gtk_widget_show(vw->label);
	gtk_widget_show(vw->button);
}

/*
 * Construct main window and its widgets
 */
void setup_waveform_window(void)
{
	int i;
	GtkWidget *box1, *hbox, *bbox, *btn;
	/* some size information. */
	const int min_w=80, min_h=50, nom_w=600, nom_h=100;

	/* Create a top-level window. Set the title and establish delete and
	   destroy event handlers. */
	win_main = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_name(win_main, prog_name);
	gtk_signal_connect(
		GTK_OBJECT(win_main), "destroy",
		GTK_SIGNAL_FUNC(destroy_handler), NULL);
	gtk_signal_connect(
		GTK_OBJECT(win_main), "delete_event",
		GTK_SIGNAL_FUNC(destroy_handler), NULL);
	gtk_container_border_width (GTK_CONTAINER (win_main), 10);

	/* create the vertical box, and add it to the window */
	box1 = gtk_vbox_new(FALSE, 0);
	gtk_container_add (GTK_CONTAINER (win_main), box1);

	/* create horizontal button box, add to top, put some buttons in it.
	* If we add ANY more commands, need to go to pulldown menus. */
	bbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_START);
	gtk_button_box_set_spacing(GTK_BUTTON_BOX(bbox), 5);
	gtk_box_pack_start(GTK_BOX(box1), bbox, FALSE, FALSE, 0);

	btn = gtk_button_new_with_label ("Quit");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(destroy_handler), NULL);
	gtk_widget_show (btn);

	btn = gtk_button_new_with_label ("Zoom In");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(cmd_zoom_in), NULL);
	gtk_widget_show (btn);
	
	btn = gtk_button_new_with_label ("Zoom Out");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(cmd_zoom_out), NULL);
	gtk_widget_show (btn);

	btn = gtk_button_new_with_label ("Zoom Full");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(cmd_zoom_full), NULL);
	gtk_widget_show (btn);

	btn = gtk_button_new_with_label ("Delete");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(cmd_delete_selected_waves), NULL);
	gtk_widget_show (btn);

	btn = gtk_button_new_with_label ("Variables");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(cmd_show_wave_list), NULL);
	gtk_widget_show (btn);
	gtk_widget_show(bbox);

	/* label with cursor status */
	win_status_label = gtk_label_new(" ");
	gtk_box_pack_start(GTK_BOX(box1), win_status_label, FALSE, FALSE, 0);
	gtk_widget_show(win_status_label);

	/* table containing labels and waveforms */
	wtable->table = gtk_table_new(wtable->npanels+2,2,FALSE);
	gtk_widget_show(wtable->table);
	gtk_box_pack_start(GTK_BOX(box1), wtable->table, TRUE, TRUE, 5);

	for(i = 0; i < wtable->npanels; i++) {
		char lbuf[128];
		WavePanel *wp = &wtable->panels[i];

		/* y-axis labels and signal names, all in a vbox */
		wp->lvbox = gtk_vbox_new(FALSE, 0);
		gtk_widget_show(wp->lvbox);

		gtk_table_attach(GTK_TABLE(wtable->table), wp->lvbox, 
			 0, 1, i, i+1, 
			 GTK_FILL, GTK_EXPAND|GTK_FILL, 4, 0);

		gtk_signal_connect (GTK_OBJECT (wp->lvbox), 
			  "drop_data_available_event",
			  GTK_SIGNAL_FUNC(wavepanel_dnd_drop),
			  (gpointer)wp);

		/* FIX: really want these labels to be right-justified. */
		sprintf(lbuf, "%.3f", wp->max_yval);
		wp->lab_max = gtk_label_new(lbuf);
		gtk_box_pack_start(GTK_BOX(wp->lvbox), wp->lab_max,
			 FALSE, FALSE, 0);
		gtk_widget_show(wp->lab_max);

		sprintf(lbuf, "%.3f", wp->min_yval);
		wp->lab_min = gtk_label_new(lbuf);
		gtk_box_pack_end(GTK_BOX(wp->lvbox), wp->lab_min,
			   FALSE, FALSE, 0);
		gtk_widget_show(wp->lab_min);

		g_list_foreach(wp->vwlist, (GFunc)vw_wp_create_button, wp);

		/* drawing area for waveform */
		wp->drawing = gtk_drawing_area_new();
		gtk_drawing_area_size(GTK_DRAWING_AREA(wp->drawing), nom_w, nom_h);
		gtk_widget_show(wp->drawing);
		gtk_table_attach(GTK_TABLE(wtable->table), wp->drawing, 
			 1, 2, i, i+1, 
			 GTK_EXPAND|GTK_FILL, GTK_EXPAND|GTK_FILL, 0, 3);

		gtk_signal_connect(
			GTK_OBJECT(wp->drawing), "expose_event", 
			(GtkSignalFunc)expose_handler, (gpointer)wp);
		gtk_signal_connect(
			GTK_OBJECT(wp->drawing), "button_release_event", 
			(GtkSignalFunc)click_handler, (gpointer)wp);

		gtk_signal_connect (GTK_OBJECT (wp->drawing), 
			  "drop_data_available_event",
			  GTK_SIGNAL_FUNC(wavepanel_dnd_drop),
			  (gpointer)wp);

		gtk_widget_set_events(wp->drawing, 
		      GDK_EXPOSURE_MASK|GDK_BUTTON_RELEASE_MASK);
	}
	
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = &wtable->panels[i];
		gtk_widget_dnd_drop_set (wp->lvbox, TRUE,
					 accepted_drop_types, 1, FALSE);
	
		gtk_widget_dnd_drop_set (wp->drawing, TRUE,
					 accepted_drop_types, 1, FALSE);
	}

	/* horizontal box for X-axis labels */
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_table_attach(GTK_TABLE(wtable->table), hbox,
			 1, 2, wtable->npanels, wtable->npanels+1,
			 GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);

	win_xlabel_left = gtk_label_new("L");
	gtk_box_pack_start(GTK_BOX(hbox), win_xlabel_left, FALSE, FALSE, 0);
	gtk_widget_show(win_xlabel_left);

	win_xlabel_right = gtk_label_new("R");
	gtk_box_pack_end(GTK_BOX(hbox), win_xlabel_right, FALSE, FALSE, 0);
	gtk_widget_show(win_xlabel_right);

	gtk_widget_show(hbox);

	/* scrollbar */
	{
		double dwidth;
		dwidth = wtable->max_xval - wtable->min_xval;
	win_hsadj = (GtkAdjustment *)
		gtk_adjustment_new(wtable->start_xval, /* value */
				   wtable->min_xval, /* lower */
				   wtable->max_xval, /* upper */
				   dwidth/100,	/* step increment = 1% */
				   dwidth/2, 	/* page increment = 50% */
				   dwidth	/* page_size */
			);
	}
	win_hsbar = gtk_hscrollbar_new(GTK_ADJUSTMENT(win_hsadj));
	gtk_range_set_update_policy (GTK_RANGE (win_hsbar), 
			       GTK_UPDATE_CONTINUOUS);
	gtk_signal_connect(
		GTK_OBJECT(win_hsadj), "value_changed", 
		(GtkSignalFunc)scroll_handler, (gpointer)wtable);
	gtk_table_attach(GTK_TABLE(wtable->table), win_hsbar,
			 1, 2, wtable->npanels+1, wtable->npanels+2,
			 GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);
	gtk_widget_show(win_hsbar);

	/* Show the top-level window, set its minimum size */
	gtk_widget_show(box1);
	gtk_widget_show(win_main);
	gdk_window_set_hints(win_main->window, 0,0,  min_w, min_h, 0,0,
			     GDK_HINT_MIN_SIZE);
}
