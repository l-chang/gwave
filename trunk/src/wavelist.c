/*
 * wavelist.c - part of gwave
 * routines to handle the scrolling list of potentialy-displayable waveforms,
 * and other stuff related to loading of data files.
 *
 * Copyright (C) 1998, 1999 Stephen G. Tell
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
#include <gwave.h>

GList *wdata_list = NULL;
static char file_tag_chars[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static const int n_file_tags = sizeof(file_tag_chars)/sizeof(char);
static int next_file_tagno = 0;

static GtkWidget *create_wavelist_menu(GWDataFile *wdata);
void add_variables_to_list(GWDataFile *wdata);
static gint wavelist_button_click(GtkWidget *widget,
				  GdkEventButton *event, gpointer data);

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
	wdata->wf = wf_read(fname, ftype);
	
	if(wdata->wf == NULL) {
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
	for(i = 0; i < wdata->wf->wf_ndv; i++) {
		WaveVar *dv = &wdata->wf->dv[i];
		dv->udata = wdata;
	}

	wdata_list = g_list_append(wdata_list, wdata);

	if(var_list_submenu) {
		create_wdata_submenuitem(wdata, var_list_submenu);
	}
	if(win_main)
		cmd_show_wave_list(NULL, wdata);

	return 0;
}

/*
 * Delete a wave file.
 * callback from menu: wavelist->file->delete
 */
void
delete_wave_file(GtkWidget *w, GWDataFile *wdata)
{
/* remove references from displayed waves */
	remove_wfile_waves(wdata);

/* remove per-file GUI stuff */
	if(wdata->wlist_win && GTK_WIDGET_VISIBLE(wdata->wlist_win))
		gtk_widget_destroy(wdata->wlist_win);
	gtk_container_remove(GTK_CONTAINER(var_list_submenu), wdata->menu_item);
	wf_free(wdata->wf);
	wdata_list = g_list_remove(wdata_list, wdata);
	g_free(wdata);
}


/*
 * command or callback from menu: wavelist->file->reload
 */
void
reload_wave_file(GtkWidget *w, GWDataFile *wdata)
{
	int i;
	WaveFile *new_wf;
	WaveFile *old_wf;

	/* FIXME:sgt: get file type from old file, if it was specified
	 * when loading it originaly
	 */
	new_wf = wf_read(wdata->wf->wf_filename, NULL);
	if(new_wf == NULL) {
		fprintf(stderr, "reload_wave_file: failed to read %s\n", wdata->wf->wf_filename);
		/* FIXME:sgt put up error message in window */
		return;
	}
	old_wf = wdata->wf;
	wdata->wf = new_wf;
/*	printf("reload_wave_file(%s) old=%lx new=%lx\n",
	       wdata->wf->wf_filename, old_wf, new_wf); */
	for(i = 0; i < wdata->wf->wf_ndv; i++) {
		WaveVar *dv = &wdata->wf->dv[i];
		dv->udata = wdata;
	}

	update_wfile_waves(wdata);

/* remove old buttons from list, and add new ones */	
	if(wdata->wlist_win && GTK_WIDGET_VISIBLE(wdata->wlist_win)) {
		gtk_container_foreach(GTK_CONTAINER(wdata->wlist_box),
				      (GtkCallback) gtk_widget_destroy, NULL);
		add_variables_to_list(wdata);
	}
	
	g_free(old_wf);
}

void
reload_wave_file_w(gpointer p, gpointer d)
{ 
	GWDataFile *wdata = (GWDataFile *)p;
	reload_wave_file(NULL, wdata);
} 

/*
 * Reload all files.
 * plan: replace this with update_all_wave_files(), which checks
 * file dates/sizes and only reloads those that need it.
 */
void
reload_all_wave_files(GtkWidget *w)
{
	WaveFile *wf;
	g_list_foreach(wdata_list, reload_wave_file_w, NULL);
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

/*
 * Add a button for each variable in the file 
 * to the win_wlist box for it.  
 * Arrange for the buttons to be drag-and-drop sources for placing the
 * variables into wavepanels.
 */
void
add_variables_to_list(GWDataFile *wdata)
{
	int i;
	GtkWidget *button;

	for(i = 0; i < wdata->wf->wf_ndv; i++) {
		WaveVar *dv = &wdata->wf->dv[i];
		button = gtk_button_new_with_label(dv->wv_name);
		
		gtk_box_pack_start (GTK_BOX (wdata->wlist_box), button, FALSE, FALSE, 0);
		gtk_widget_show (button);
		dnd_setup_source(wdata->wlist_win, button, dv);

		gtk_signal_connect (GTK_OBJECT(button), "button-press-event",
			    GTK_SIGNAL_FUNC(wavelist_button_click), 
			    (gpointer) dv);

	}
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
	GtkWidget *menubar;
	int i;

	if(!wdata) {
		fprintf(stderr, "cmd_show_wave_list: wdata is NULL");
		return;
	}

	if(!wdata->wlist_win) {
		char buf[256];
		
		wdata->wlist_win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
		gtk_widget_set_name(wdata->wlist_win, "data list window");
		sprintf(buf, "gwave: %.64s", wdata->wf->wf_filename);
		gtk_window_set_title(GTK_WINDOW(wdata->wlist_win), buf);
		gtk_widget_set_usize(wdata->wlist_win, 150, 300);
		{ /* suggest that the window manager try to put the wavelist
		   *   window somewhere to the left of the main window.
		   * This nonsense really belongs in a smart window manager, 
		   * but users are demanding somthing.  Don't like this
		   * positioning algorithm?  get SCWM. 
		   */
			static int diddle=0;
			int x, y;
			x = 200+175;
			y = 200;
			if(win_main && win_main->window) {
				gdk_window_get_position(win_main->window, &x, &y);
				y += diddle * 25;
				x -= diddle * 20;
				diddle = (diddle + 1) % 4;
			}
			gtk_widget_set_uposition(wdata->wlist_win, x-175, y);
		}
		gtk_signal_connect (GTK_OBJECT (wdata->wlist_win), "destroy",
                          GTK_SIGNAL_FUNC(gtk_widget_destroyed),
                          &(wdata->wlist_win));

		box1 = gtk_vbox_new(FALSE, 0);
		gtk_container_add(GTK_CONTAINER(wdata->wlist_win), box1);
		gtk_widget_show(box1);
		menubar = create_wavelist_menu(wdata);
		gtk_box_pack_start (GTK_BOX (box1), menubar, FALSE, FALSE, 0);

		if(strlen(wdata->wf->wf_filename) > 16) {
			char *cp = strrchr(wdata->wf->wf_filename, '/');
			if(cp)
				sprintf(buf, "%s: .../%.64s", wdata->ftag, cp+1);
			else
				sprintf(buf, "%s: .../%.64s", wdata->ftag, wdata->wf->wf_filename);
		} else {
			sprintf(buf, "%s: %.64s", wdata->ftag, wdata->wf->wf_filename);
		}
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


		wdata->wlist_box = gtk_vbox_new (FALSE, 0);
		gtk_container_border_width (GTK_CONTAINER (wdata->wlist_box), 10);
#ifdef GTK_V12
		gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window),
						      wdata->wlist_box);
#else
		gtk_container_add (GTK_CONTAINER(scrolled_window),
				   wdata->wlist_box);
#endif
		gtk_container_set_focus_vadjustment(
			GTK_CONTAINER (wdata->wlist_box),
			gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(scrolled_window)));
		gtk_widget_show (wdata->wlist_box);

		dnd_init(wdata->wlist_win);
		add_variables_to_list(wdata);
	}

	if(!GTK_WIDGET_VISIBLE(wdata->wlist_win))
		gtk_widget_show(wdata->wlist_win);
	else
		gtk_widget_destroy(wdata->wlist_win);
}

/*
 * Create menu bar for wavefile window.
 */
static GtkWidget *
create_wavelist_menu(GWDataFile *wdata)
{
	GtkWidget *menubar;
	GtkWidget *menu;
       
	menubar = gtk_menu_bar_new();
	gtk_widget_show(menubar);

	menu = create_menu("File", menubar);
	create_menuitem("Reload this file", menu,
			GTK_SIGNAL_FUNC(reload_wave_file), wdata);
	create_menuitem("Delete this file", menu, 
			GTK_SIGNAL_FUNC(delete_wave_file), wdata);
	create_menuitem(NULL, menu, NULL, NULL); /* separator */
	create_menuitem("Close", menu, 
			GTK_SIGNAL_FUNC(cmd_show_wave_list), wdata);
	
	return menubar;
}

/*
 * Called for all button presses on wavelist button.
 * If it is a doubleclick, add variable to the "current" wavepanel immediately.
 */
static gint
wavelist_button_click(GtkWidget *widget, 
		      GdkEventButton *bevent, 
		      gpointer data)
{
	WaveVar *dv = (WaveVar *)data;
	if(bevent->type == GDK_2BUTTON_PRESS) {
/*		printf("doubleclicked %s %s\n", dv->wfile->ss->filename,
		       dv->sv->name); */
		add_var_to_panel(NULL, dv);
	}
}
