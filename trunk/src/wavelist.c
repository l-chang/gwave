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
 * License along with this software; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.9  2000/05/18 07:03:26  sgt
 * draw.c: fix embarassing mistake in x2val; Log X scale now works
 * wavewin.c: add wtable-wavepanels and wavepanel-visiblewaves in the
 * 	name of completeness of data structure access from guile.
 * The usual tweaks for another snapshot release.
 * minor comment fixes: wavewin.h cmds.scm wavelist.c
 *
 * Revision 1.8  2000/01/07 06:33:44  tell
 * Merged in the guile and guile-gtk stuff
 *
 * Revision 1.6  1999/05/28 23:06:12  tell
 * change to use spicefile library to read data.
 * Add "file" menu to wavelist window
 * add file-delete and file-reload operations
 *
 * Revision 1.5  1999/01/08 22:41:13  tell
 * when loading file, defer generating wavelist window if main window
 * isn't present yet.  Attempt nicer placement of wavelist windows.
 *
 * Revision 1.4  1998/11/09 20:29:53  tell
 * always display variable-select list after loading file
 *
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
#include <config.h>
#include <scwm_guile.h>
#include <gwave.h>

#define WAVELIST_IMPLEMENTATION
#include <wavelist.h>

GList *wdata_list = NULL;
static char file_tag_chars[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static const int n_file_tags = sizeof(file_tag_chars)/sizeof(char);
static int next_file_tagno = 0;

static GtkWidget *create_wavelist_menu(GWDataFile *wdata);
void add_variables_to_list(GWDataFile *wdata);
static gint wavelist_button_click(GtkWidget *widget,
				  GdkEventButton *event, gpointer data);

SCWM_HOOK(new_wavefile_hook,"new-wavefile-hook", 1);
  /** This hook is invoked when a new waveform file is successfully loaded.
   it is called with the new GWDataFile as its only argument */
SCWM_HOOK(new_wavelist_hook,"new-wavelist-hook", 1);
  /** This hook is invoked when the variable list window for a
GWDataFile is created.  The GWDataFile object is passed as an
argument.  Note that variable-list windows can be created and
destroyed many times during the life of a GWDataFile */

/*
 * Load a waveform file, adding it to the list of files from which
 * variables can be chosen to add to the display.
 */
int
load_wave_file(char *fname, char *ftype)
{
	GWDataFile *wdata;
	SCM swdata;
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

/*	if(var_list_submenu) {
		create_wdata_submenuitem(wdata, var_list_submenu);
	}
*/
	wdata->outstanding_smob = 1;
	SGT_NEWCELL_SMOB(wdata->smob, GWDataFile, wdata);
	call1_hooks(new_wavefile_hook, wdata->smob);

	if(win_main)
		cmd_show_wave_list(NULL, wdata);

	return 0;
}

SCWM_PROC(load_wavefile_x, "load-wavefile!", 1, 1, 0, (SCM file, SCM filetype))
  /* Load waveform data from FILE into memory, and make it available for
   * display.  If FILETYPE is specified, it indicates the format of the file
   * and which wavefile reader to use, otherwise the format is inferred
   * from the filename and file contents */
#define FUNC_NAME s_load_wavefile_x
{
	char *fname, *ftype;
	int rc;
	VALIDATE_ARG_STR_NEWCOPY(1, file, fname);
	VALIDATE_ARG_STR_NEWCOPY_USE_NULL(2, filetype, ftype);
	rc = load_wave_file(fname, ftype);
	g_free(fname);
	if(ftype)
		g_free(ftype);
	if(rc < 0)
		return SCM_BOOL_F;
	else
		return SCM_BOOL_T;
}
#undef FUNC_NAME

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

	wf_free(wdata->wf);
	wdata->wf = NULL;
	wdata_list = g_list_remove(wdata_list, wdata);

	if(wdata->outstanding_smob) {
		if(v_flag)
			fprintf(stderr, "defering free of GWDataFile\n");
	} else {
		if(v_flag)
			fprintf(stderr, "free GWDataFile 0x%x\n", wdata);
		g_free(wdata);
	}
}

SCWM_PROC(datafile_delete_x, "wavefile-delete!", 1, 0, 0, 
           (SCM obj))
/** Delete from memory the waveform data from OBJ.
*/
#define FUNC_NAME s_datafile_delete_x
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	if(wdata->wf)
		delete_wave_file(NULL, wdata);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

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

SCWM_PROC(reload_all_files_x, "reload-all-files!", 0, 0, 0, ())
  /** Reload all files
*/
#define FUNC_NAME s_reload_all_files
{
	WaveFile *wf;
	g_list_foreach(wdata_list, reload_wave_file_w, NULL);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(datafile_reload_x, "wavefile-reload!", 1, 0, 0, 
           (SCM obj))
/** Reread the data file for OBJ.  Useful for updating the display
    after simulation has been rerun.
*/
#define FUNC_NAME s_datafile_reload_x
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	if(wdata->wf)
		reload_wave_file(NULL, wdata);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#if 0
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

#endif

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
 * Show the variable-list window for a waveform data file.
 * If the window already exists, simply raise it to the top.
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
		wdata->wlist_menubar = gtk_menu_bar_new();
		gtk_widget_show(wdata->wlist_menubar);
		gtk_box_pack_start (GTK_BOX (box1), wdata->wlist_menubar, FALSE, FALSE, 0);

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

		call1_hooks(new_wavelist_hook, wdata->smob);

		gtk_widget_show(wdata->wlist_win);
	} else {
		gdk_window_raise(wdata->wlist_win->window);
	}
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

SCWM_PROC(wavefile_show_listwin_x, "wavefile-show-listwin!", 1, 0, 0,
           (SCM obj))
/** Displays the scrolling list of the variables in OBJ, from which they
    can be dragged into a waveform display panel. */
#define FUNC_NAME s_wavefile_show_listwin_x
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	if(v_flag)
		fprintf(stderr, "%s wdata=0x%x\n", FUNC_NAME, wdata);
	if(wdata->wf)
		cmd_show_wave_list(NULL, wdata);
	return SCM_UNSPECIFIED;

}
#undef FUNC_NAME

/* maybe I should just expose the GTkWindow itself, and destroy from guile */
SCWM_PROC(wavefile_remove_listwin_x, "wavefile-remove-listwin!", 1, 0, 0,
           (SCM obj))
/** Removes the variable-list window for OBJ */
#define FUNC_NAME s_wavefile_remove_listwin_x
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	if(wdata->wf && wdata->wlist_win)
		gtk_widget_destroy(wdata->wlist_win);
	return SCM_UNSPECIFIED;

}
#undef FUNC_NAME

/* Primitives for accessing GWDataFile info from scheme */

SCWM_PROC(wavefile_file_name, "wavefile-file-name", 1, 0, 0,
           (SCM obj))
/** Returns the filename from which the GWDataFile OBJ was loaded. 
    If OBJ is invalid because the datafile has been deleted,
    #f is returned. */
#define FUNC_NAME s_wavefile_file_name
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	if(wdata->wf)
		return gh_str02scm(wdata->wf->wf_filename);
	else
		return SCM_BOOL_F;

}
#undef FUNC_NAME

SCWM_PROC(wavefile_tag, "wavefile-tag", 1, 0, 0,
           (SCM obj))
/** Returns the short identifying tag for the GWDataFile OBJ.
 */
#define FUNC_NAME s_wavefile_tag
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	return gh_str02scm(wdata->ftag);
}
#undef FUNC_NAME

SCWM_PROC(wavefile_listwin_menubar, "wavefile-listwin-menubar", 1, 0, 0,
           (SCM obj))
/** Returns the GTK Menubar for the variable-list window of the
 * GWDataFile OBJ, or #f if the window doesn't exist.
 */
#define FUNC_NAME s_wavefile_listwin_menubar
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	if(wdata->wlist_win && wdata->wlist_menubar)
		return sgtk_wrap_gtkobj(GTK_OBJECT(wdata->wlist_menubar));
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM
wavefile_to_scm(void *vp)
{
	GWDataFile *wdata = (GWDataFile *)vp;
	return wdata->smob;
}

SCM
glist2scm(GList *list, SCM (*toscm)(void*))
{
	  SCM result = SCM_EOL;
	  while(list) {
		  result = scm_cons(toscm(list->data), result);
		  list = list->next;
	  }
	  return result;
}

SCWM_PROC(wavefile_list, "wavefile-list", 0, 0, 0, ())
/** Returns a list containing all waveform data files */
#define FUNC_NAME s_wavefile_list
{
	return glist2scm(wdata_list, wavefile_to_scm);
}
#undef FUNC_NAME


/* standard SMOB functions for GWDataFile: free, mark, print, GWDataFile? */
scm_sizet
free_GWDataFile(SCM obj)
{
	GWDataFile *wdata =GWDataFile(obj);
	wdata->outstanding_smob = 0;

	if(wdata->wf == NULL) { /* if C has already invalidated, free it up */
		if(v_flag)
			fprintf(stderr, "free GWDataFile 0x%x during gc\n", wdata);
		g_free(wdata);
		return sizeof(GWDataFile);
	}
	else
		return 0;
}

SCM
mark_GWDataFile(SCM obj)
{
	return SCM_BOOL_F;
}

int 
print_GWDataFile(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
	scm_puts("#<GWDataFile ", port);
	if(GWDataFile(obj)->wf)
		scm_puts(GWDataFile(obj)->wf->wf_filename, port);
	else
		scm_puts("invalid", port);
	scm_putc('>', port);
	return 1;
}

SCWM_PROC(GWDataFile_p, "GWDataFile?", 1, 0, 0,
           (SCM obj))
     /** Returns #t if OBJ is a gwave data file object, otherwise #f. */
#define FUNC_NAME s_GWDataFile_p
{
	return SCM_BOOL_FromBool(GWDataFile_P(obj));
}
#undef FUNC_NAME

/* guile initialization */

MAKE_SMOBFUNS(GWDataFile);

void init_wavelist()
{
        REGISTER_SCWMSMOBFUNS(GWDataFile);

#ifndef SCM_MAGIC_SNARFER
#include "wavelist.x"
#endif
}
