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
 * Revision 1.4  1999/01/08 22:40:24  tell
 * substantial changes and modularization in support of wavepanel add/delete
 * create right-button popup menu for wavepanels
 *
 * Revision 1.3  1998/09/30 21:55:39  tell
 * Add menu items for new zoom commands
 * add signal handlers to support dragging of cursors and cmd_zoom_window
 *
 * Revision 1.2  1998/09/17 18:28:22  tell
 * Added pulldown menus, removed redundant toolbar buttons.
 * Added support for multiple files.
 * Finally got y-axis labels right-justfied by putting them in hboxes.
 *
 * Revision 1.1  1998/09/01 21:27:24  tell
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
#include "gwave.h"

GtkWidget *var_list_submenu;

/* Create a (sub)menu and the item that activates it.
 * Returns GtkMenu widget pointer.
 */
GtkWidget *
create_menu(char *label, GtkWidget *parent)
{
	GtkWidget *item, *menu;
	
	if(label)
		item = gtk_menu_item_new_with_label(label);
	else
		item = gtk_menu_item_new();

	menu = gtk_menu_new();
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), menu);

	if(parent) {
		if(GTK_IS_MENU_BAR(parent))
			gtk_menu_bar_append (GTK_MENU_BAR (parent), item);
		else if(GTK_IS_MENU(parent))
			gtk_menu_append(GTK_MENU(parent), item);
	}
	gtk_widget_show(item);
	return menu;
}

/*
 * helper function for making menu items.  Returns menu item widget pointer,
 * but it can often be ignored, since the item is already added to the parent.
 */
GtkWidget *
create_menuitem(char *label, GtkWidget *parent, GtkSignalFunc action, 
		gpointer p)
{
	GtkWidget *item;
	if(label)
		item = gtk_menu_item_new_with_label(label);
	else
		item = gtk_menu_item_new();

	if(action)
		gtk_signal_connect (GTK_OBJECT (item), "activate", action, p);
	if(parent)
		gtk_menu_append (GTK_MENU(parent), item);
	gtk_widget_show (item);
	return item;
}

/*
 * create a submenu item for a waveform file that opens/closes the
 * corresponding variable list window.
 * Called by g_list_foreach() out of create_gwave_menu().
 */
void
create_wdata_submenuitem(GWDataFile *wdata, GtkWidget *submenu)
{
	char buf[128];

	sprintf(buf, "%s: %.120s", wdata->ftag, wdata->wf->wf_filename);
	wdata->menu_item = gtk_menu_item_new_with_label(buf);
	gtk_menu_append(GTK_MENU(submenu), wdata->menu_item);
	gtk_signal_connect (GTK_OBJECT (wdata->menu_item), "activate",
			    GTK_SIGNAL_FUNC(cmd_show_wave_list), wdata);
	gtk_widget_show(wdata->menu_item);
}

/*
 * Create menu bar for main gwave window.
 */
static GtkWidget *
create_gwave_menu()
{
	GtkWidget *menubar;
	GtkWidget *menu;
       
	menubar = gtk_menu_bar_new();
	gtk_widget_show(menubar);

	menu = create_menu("File", menubar);
	create_menuitem("About Gwave", menu, create_about_window, NULL);
	create_menuitem("Read File...", menu, 
			GTK_SIGNAL_FUNC(get_fname_load_file), NULL);
	create_menuitem(NULL, menu, NULL, NULL); /* separator */
	create_menuitem("Quit", menu, GTK_SIGNAL_FUNC(destroy_handler), NULL);
	
	menu = create_menu("View", menubar);
	create_menuitem("Zoom Full", menu, 
			GTK_SIGNAL_FUNC(cmd_zoom_full), NULL);
	create_menuitem("Zoom Cursors", menu, 
			GTK_SIGNAL_FUNC(cmd_zoom_cursors), NULL);
	create_menuitem("Zoom Area...", menu, 
			GTK_SIGNAL_FUNC(cmd_zoom_window), NULL);
	var_list_submenu = create_menu("Variable List", menu);

	g_list_foreach(wdata_list, 
		       (GFunc)create_wdata_submenuitem, var_list_submenu);

	create_menuitem("Add Panel", menu, 
			GTK_SIGNAL_FUNC(cmd_append_panel), NULL);
	return menubar;
}

/* popmenu for button 3 in waveform drawing areas */
static GtkWidget *
create_gwave_panel_popup_menu()
{
	GtkWidget *menu;
	menu = create_menu(NULL, NULL);
	create_menuitem("Zoom to Cursors", menu, 
			GTK_SIGNAL_FUNC(cmd_zoom_cursors), NULL);
	create_menuitem("Zoom Area...", menu, 
			GTK_SIGNAL_FUNC(cmd_zoom_window), NULL);
	create_menuitem("Zoom Full", menu, 
			GTK_SIGNAL_FUNC(cmd_zoom_full), NULL);
	create_menuitem("Insert Panel", menu, 
			GTK_SIGNAL_FUNC(cmd_popup_insert_panel), NULL);
	create_menuitem("Delete this panel", menu, 
			GTK_SIGNAL_FUNC(cmd_popup_delete_panel), NULL);
	return menu;
}

/* generate wave button label string, for both initial setup and updating.
 */
void
vw_get_label_string(char *buf, int buflen, VisibleWave *vw)
{
	GWDataFile *gdf;
	double xval, dval;
	int n, l;

	gdf = vw->gdf;
	g_assert(gdf != NULL);

	l = buflen - strlen(gdf->ftag) - 10;
	n = MIN(l, 15);
	xval = wtable->cursor[0]->xval;
	if(vw->var->wv_iv->wds->min <= xval && xval <= vw->var->wv_iv->wds->max) {

		dval = wv_interp_value(vw->var, xval);
		sprintf(buf, "%s: %.*s %.3f",
			gdf->ftag, l, vw->varname, dval);
	} else {
		/* should keep track of label state (name vs. name+val)
		 * and only re-do this if necessary */
		sprintf(buf, "%s: %.*s    ", gdf->ftag, l, vw->varname);
	}

}

/*
 * vw_wp_create_button -- called from g_list_foreach to 
 * create button and label widgets for one VisibleWave in a WavePanel
 */
void
vw_wp_create_button(VisibleWave *vw, WavePanel *wp)
{
	char lbuf[64];

	vw_get_label_string(lbuf, 64, vw);
	vw->label = gtk_label_new(lbuf);
	vw->button = gtk_toggle_button_new();
	gtk_container_add(GTK_CONTAINER(vw->button), vw->label);
	gtk_box_pack_start(GTK_BOX(wp->lvbox), vw->button,
			   FALSE, FALSE, 0);
	sprintf(lbuf, "wavecolor%d", vw->colorn);
	gtk_widget_set_name(vw->label, lbuf);
	gtk_widget_show(vw->label);
	gtk_widget_set_name(vw->button, "wavebutton");
	gtk_widget_show(vw->button);
}

/* create horizontal button box for top of main window */
GtkWidget *create_toolbar()
{
	GtkWidget *bbox, *btn;

	bbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_START);
	gtk_button_box_set_spacing(GTK_BUTTON_BOX(bbox), 5);

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

	btn = gtk_button_new_with_label ("Delete");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(cmd_delete_selected_waves), NULL);
	gtk_widget_show (btn);
	gtk_widget_show(bbox);
	return bbox;
}

/* horizontal box for X-axis labels */
GtkWidget *create_xlabel_hbox()
{
	GtkWidget *hbox;
	hbox = gtk_hbox_new(FALSE, 0);
	win_xlabel_left = gtk_label_new("0");
	gtk_box_pack_start(GTK_BOX(hbox), win_xlabel_left, FALSE, FALSE, 0);
	gtk_widget_show(win_xlabel_left);

	win_xlabel_right = gtk_label_new("0");
	gtk_box_pack_end(GTK_BOX(hbox), win_xlabel_right, FALSE, FALSE, 0);
	gtk_widget_show(win_xlabel_right);

	gtk_widget_show(hbox);
	return hbox;
}

/*
 * Set up widgets for a WavePanel - construct lvbox and drawing area
 */ 
void setup_wave_panel(WavePanel *wp)
{
	GtkWidget *hbox;
	char lbuf[128];
	const int nom_w=500, nom_h=100;

	/* initialize view in case we're not at the default. 
	 * only matters when adding panels to running gwave.
	 */
	wp->start_xval = wtable->start_xval;
	wp->end_xval = wtable->end_xval;
	/* y-axis labels and signal names, all in a vbox */
	wp->lvbox = gtk_vbox_new(FALSE, 0);
	gtk_widget_set_usize(wp->lvbox, 140, -1);
	gtk_widget_show(wp->lvbox);

	hbox = gtk_hbox_new(FALSE, 0);
	sprintf(lbuf, "%.3f", wp->max_yval);
	wp->lab_max = gtk_label_new(lbuf);
	gtk_box_pack_start(GTK_BOX(wp->lvbox), hbox,
			   FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(hbox), wp->lab_max,
			 FALSE, FALSE, 0);
	gtk_widget_show(hbox);
	gtk_widget_show(wp->lab_max);

	hbox = gtk_hbox_new(FALSE, 0);
	sprintf(lbuf, "%.3f", wp->min_yval);
	wp->lab_min = gtk_label_new(lbuf);
	gtk_box_pack_end(GTK_BOX(wp->lvbox), hbox,
			 FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(hbox), wp->lab_min,
			 FALSE, FALSE, 0);
	gtk_widget_show(hbox);
	gtk_widget_show(wp->lab_min);
	
	g_list_foreach(wp->vwlist, (GFunc)vw_wp_create_button, wp);

	/* drawing area for waveform */
	wp->drawing = gtk_drawing_area_new();
	gtk_drawing_area_size(GTK_DRAWING_AREA(wp->drawing), nom_w, nom_h);
	gtk_widget_show(wp->drawing);
	gtk_signal_connect(
		GTK_OBJECT(wp->drawing), "expose_event", 
		(GtkSignalFunc)expose_handler, (gpointer)wp);
	gtk_signal_connect(
		GTK_OBJECT(wp->drawing), "button_press_event", 
		(GtkSignalFunc)button_press_handler, (gpointer)wp);
	gtk_signal_connect(
		GTK_OBJECT(wp->drawing), "button_release_event", 
		(GtkSignalFunc)button_release_handler, (gpointer)wp);
	gtk_signal_connect(
		GTK_OBJECT(wp->drawing), "motion_notify_event", 
		(GtkSignalFunc)motion_handler, (gpointer)wp);
	
	gtk_signal_connect (GTK_OBJECT (wp->drawing), 
			    "drop_data_available_event",
			    GTK_SIGNAL_FUNC(wavepanel_dnd_drop),
			    (gpointer)wp);
	
	gtk_widget_set_events(wp->drawing, 
			      GDK_EXPOSURE_MASK|GDK_BUTTON_RELEASE_MASK|
			      GDK_BUTTON_PRESS_MASK|
			      GDK_BUTTON1_MOTION_MASK|GDK_BUTTON2_MOTION_MASK);

}

/*
 * Delete a wavepanel structure and all data structures referenced from it.
 */
void destroy_wave_panel(WavePanel *wp)
{
	VisibleWave *vw;

	while((vw = g_list_nth_data(wp->vwlist, 0)) != NULL) {
		remove_wave_from_panel(wp, vw);
	}
	gtk_widget_destroy(wp->lvbox);
	gtk_widget_destroy(wp->drawing);
	gdk_pixmap_unref(wp->pixmap);
	g_free(wp);
}

/* build the GtkTable widget for the main window.
 * side effect:
 *	creates wtable->table widget and adds the other widgets
 *	to it. 
 *	wtable->xlhbox, win_hsbar, and the panel widgets must already
 *	be created.
 */
void
wavewin_build_table()
{
	int i;

	wtable->table = gtk_table_new(wtable->npanels+2,2,FALSE);
	gtk_widget_show(wtable->table);
	gtk_box_pack_start(GTK_BOX(wtable->vbox), wtable->table, TRUE, TRUE, 5);
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];

		gtk_table_attach(GTK_TABLE(wtable->table), wp->lvbox, 
			 0, 1, i, i+1, 
			 GTK_FILL, GTK_EXPAND|GTK_FILL, 4, 0);

		gtk_table_attach(GTK_TABLE(wtable->table), wp->drawing, 
			 1, 2, i, i+1, 
			 GTK_EXPAND|GTK_FILL, GTK_EXPAND|GTK_FILL, 0, 3);

	}
	gtk_table_attach(GTK_TABLE(wtable->table), wtable->xlhbox,
			 1, 2, wtable->npanels, wtable->npanels+1,
			 GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);

	gtk_table_attach(GTK_TABLE(wtable->table), win_hsbar,
			 1, 2, wtable->npanels+1, wtable->npanels+2,
			 GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);

	/* can't set up dnd_drop on wavepanel drawing areas until it
	 * is connected to a window and realized so that it has
	 * an X-window.  At least I think that's the deal. */
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		gtk_widget_dnd_drop_set (wp->lvbox, TRUE,
					 accepted_drop_types, 1, FALSE);
	
		gtk_widget_dnd_drop_set (wp->drawing, TRUE,
					 accepted_drop_types, 1, FALSE);
	}
}

/*
 * delete waveform window's GtkTable Widget.
 * arranges so that the child widgets stay around so a new table
 * can be built with more or fewer panels.
 */
void
wavewin_destroy_table()
{
	int i;
	/* bump refcount on table's children so they don't get cleaned up */
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		gtk_widget_ref(wp->lvbox);
		gtk_container_remove(GTK_CONTAINER(wtable->table), wp->lvbox);
		gtk_widget_ref(wp->drawing);
		gtk_container_remove(GTK_CONTAINER(wtable->table),wp->drawing);
	}
	gtk_widget_ref(wtable->xlhbox);
	gtk_container_remove(GTK_CONTAINER(wtable->table), wtable->xlhbox);
	gtk_widget_ref(win_hsbar);
	gtk_container_remove(GTK_CONTAINER(wtable->table), win_hsbar);

	gtk_widget_destroy(wtable->table);
	wtable->table = NULL;

}       

/* remove the extra references to wtable's child widgets
 * that we had to make while rebuilding the table
 */
void
wavewin_finish_table_rebuild()
{
	int i;
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];

		gtk_widget_unref(wp->lvbox);
		gtk_widget_unref(wp->drawing);
	}
	gtk_widget_unref(wtable->xlhbox);
	gtk_widget_unref(win_hsbar);
}

/*
 * Construct main window and its widgets
 */
void setup_waveform_window(void)
{
	int i;
	GtkWidget *box0, *bbox, *menubar;
	/* some size information. */
	const int min_w=80, min_h=50;

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

	/* create the vertical box, and add it to the window */
	box0 = gtk_vbox_new(FALSE, 0);
	gtk_container_add (GTK_CONTAINER (win_main), box0);
	gtk_widget_show(box0);

	menubar = create_gwave_menu();
	gtk_box_pack_start(GTK_BOX(box0), menubar, FALSE, TRUE, 0);

	wtable->vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_border_width (GTK_CONTAINER (wtable->vbox), 5);
	gtk_container_add (GTK_CONTAINER (box0), wtable->vbox);
	bbox = create_toolbar();
	gtk_box_pack_start(GTK_BOX(wtable->vbox), bbox, FALSE, FALSE, 0);

	/* label with cursor status */
	win_status_label = gtk_label_new(" ");
	gtk_box_pack_start(GTK_BOX(wtable->vbox), win_status_label, FALSE, FALSE, 0);
	gtk_widget_show(win_status_label);

	/* set up WavePanels */
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		setup_wave_panel(wp);
	}
	/* popup menu for WavePanel drawing areas */
	wtable->popup_menu = create_gwave_panel_popup_menu();

	/* horizontal box for X-axis labels */
	wtable->xlhbox = create_xlabel_hbox();

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
	gtk_widget_show(win_hsbar);

	/* assemble wavepanels, label, and scrollbar into the table */
	wavewin_build_table();

	/* Show the top-level window, set its minimum size */
	gtk_widget_show(wtable->vbox);
	gtk_widget_show(win_main);
	gdk_window_set_hints(win_main->window, 0,0,  min_w, min_h, 0,0,
			     GDK_HINT_MIN_SIZE);
	wtable->button_down = -1;
}

/*
 * Delete and rebuild the GtkTable for the waveform window.
 * prototype for adding/deleting panels
 */
void
wavewin_rebuild_table()
{
	wavewin_destroy_table();
	/* change # of panels or just rearrange wtable->panels array here */
	wavewin_build_table();
	wavewin_finish_table_rebuild();
}

/*
 * Create new WavePanel before the specified panel, 
 * or at the end if no panel specified.
 */
void
wavewin_insert_panel(WavePanel *ppos)
{
	int p, n;
	WavePanel **owp;
	int found = 0;

	wavewin_destroy_table();

	owp = wtable->panels;
	wtable->npanels++;
	wtable->panels = g_new0(WavePanel*, wtable->npanels);

	for(p = 0, n = 0; p < wtable->npanels - 1 ; p++) {
		if(ppos == owp[p]) {
			wtable->panels[n] = g_new0(WavePanel, 1);
			setup_wave_panel(wtable->panels[n]);
			/*HACK: protect new widgets from finish_table_rebuild*/
			gtk_widget_ref(wtable->panels[n]->lvbox);
			gtk_widget_ref(wtable->panels[n]->drawing);
			found = 1;
			n++;
		}
		wtable->panels[n++] = owp[p];
	}
	if(!found) {
		wtable->panels[n] = g_new0(WavePanel, 1);
		setup_wave_panel(wtable->panels[n]);
		/*HACK: protect new widgets from finish_table_rebuild*/
		gtk_widget_ref(wtable->panels[n]->lvbox);
		gtk_widget_ref(wtable->panels[n]->drawing);
	}
	g_free(owp);
	wavewin_build_table();
	wavewin_finish_table_rebuild();
}

/*
 * Delete the specified WavePanel.
 */
void
wavewin_delete_panel(WavePanel *dwp)
{
	int i, p;
	WavePanel **nwp;
	if(wtable->npanels == 1) {
		fprintf(stderr, "cmd_delete_panel: can't delete last panel\n");
		return;
	}

	wavewin_destroy_table();

	nwp = g_new0(WavePanel*, wtable->npanels - 1);
	for(p = 0, i = 0; i < wtable->npanels; i++) {
		if(wtable->panels[i] == dwp) {
			destroy_wave_panel(wtable->panels[i]);
			dwp = NULL;
			wtable->panels[i] = NULL;
		} else {
			nwp[p++] = wtable->panels[i];
		}
	}
	if(dwp) {
		fprintf(stderr, "cmd_delete_panel: specified panel not found\n");
		/* some memory may have leaked */
	}
	g_free(wtable->panels);
	wtable->npanels--;
	wtable->panels = nwp;

	wavewin_build_table();
	wavewin_finish_table_rebuild();
}

/* some silly little menu callback adaptor routines.  One of
 * the things that will go away when we add guile support and
 * program the GUI from the guile side.
 */

/*
 * menu-bar menu callback to append a panel to the bottom of the table
 */
void
cmd_append_panel(GtkWidget *w)
{
	wavewin_insert_panel(NULL);
}

/*
 * popup-menu callback to insert a panel before the indicated one
 */
void
cmd_popup_insert_panel(GtkWidget *w)
{
	wavewin_insert_panel(wtable->popup_panel);
	wtable->popup_panel = NULL;
}

/*
 * popup-menu callback to insert a panel before the indicated one
 */
void
cmd_popup_delete_panel(GtkWidget *w)
{
	wavewin_delete_panel(wtable->popup_panel);
	wtable->popup_panel = NULL;
}
