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
 * Revision 1.14  2000/10/24 05:50:55  sgt
 * SMOBify WaveVar, and add most access methods needed for
 * restoring VisibleWave/Panel configurations
 *
 * Revision 1.13  2000/08/11 06:40:47  sgt
 * Get common GtkTooltips sharable between guile and C, use it in wavelist
 * window.
 * add gtk-tooltips-enabled? to gtkmisc.c
 * add VALIDATE_GTK_COPY to validate.h, addtional snarfing macros to guile-ext.h
 * Try out new .gwaverc features
 *
 * Revision 1.12  2000/08/10 04:43:27  sgt
 * Extend our documentation-snarfing mechanism to handle hooks,
 * variables, and concepts; changed various .c files to use new system.
 * guile-ext.h contains additions to libguile/snarf.h for this.
 * scwm-snarf.h is no longer neeeded.
 *
 * Revision 1.11  2000/08/08 06:41:24  sgt
 * Convert to guile-1.4 style SCM_DEFINE macros, where the docstrings
 * are strings, not comments.  Remove some unused functions.
 * Other guile-1.4 compatibility.  Not tested with earlier guile yet.
 *
 * Revision 1.10  2000/08/08 01:08:58  sgt
 * be more explicit about required pointer casting in wavefile_to_scm()
 *
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
#include <assert.h>
#include <sys/time.h>

#include <gtk/gtk.h>
#include <guile-gtk.h>
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

SCM_HOOK(new_wavefile_hook,"new-wavefile-hook", 1, (SCM DF),
"This hook is invoked when a new waveform file is successfully loaded.
It is called with the new GWDataFile, DF, as its only argument.");

SCM_HOOK(new_wavelist_hook,"new-wavelist-hook", 1, (SCM DF),
"This hook is invoked when the variable list window for a
GWDataFile is created.  The GWDataFile object, DF, is passed as an
argument.  Note that variable-list windows can be created and
destroyed many times during the life of a GWDataFile.  One of the principle
uses of this hook is creating the menus such for the variable-list window.");

/*
 * Load a waveform file, adding it to the list of files from which
 * variables can be chosen to add to the display.
 */
GWDataFile *
load_wave_file(char *fname, char *ftype)
{
	GWDataFile *wdata;
	SCM swdata;
	int i;

	wdata = g_new0(GWDataFile, 1);
	wdata->wf = wf_read(fname, ftype);
	
	if(wdata->wf == NULL) {
		g_free(wdata);
		return NULL;
	}

	/* give the file a short (fow now, 1-character) "tag" to identify it
	 * in the menu and variable labels.  
	 * TODO: let user set the tag if they so choose.
	 */
	wdata->ftag = g_new(char, 2);
	wdata->ftag[0] = file_tag_chars[next_file_tagno];
	wdata->ftag[1] = '\0';
	next_file_tagno = (next_file_tagno + 1) % n_file_tags;
	wdata->wvhs = g_new0(WaveVarH, wdata->wf->wf_ndv);
	wdata->ndv = wdata->wf->wf_ndv;

	/* userdata pointer in variable gets backpointer to wdata struct */
	for(i = 0; i < wdata->wf->wf_ndv; i++) {
		WaveVar *dv = &wdata->wf->dv[i];
		dv->udata = wdata;
		wdata->wvhs[i].df = wdata;
	}

	wdata_list = g_list_append(wdata_list, wdata);
	wdata->outstanding_smob = 1;
	SGT_NEWCELL_SMOB(wdata->smob, GWDataFile, wdata);
	call1_hooks(new_wavefile_hook, wdata->smob);

	if(win_main)
		cmd_show_wave_list(NULL, wdata);

	return wdata;
}

SCM_DEFINE(load_wavefile_x, "load-wavefile!", 1, 1, 0, (SCM file, SCM filetype),
"Load waveform data from FILE into memory, and make it available for
display.  If FILETYPE is specified, it indicates the format of the file
and which wavefile reader to use, otherwise the format is inferred
from the filename and file contents.  Returns a GWDataFile object
which can be used to refer to the loaded data.")
#define FUNC_NAME s_load_wavefile_x
{
	char *fname, *ftype;
	GWDataFile *df;

	VALIDATE_ARG_STR_NEWCOPY(1, file, fname);
	VALIDATE_ARG_STR_NEWCOPY_USE_NULL(2, filetype, ftype);
	df = load_wave_file(fname, ftype);
	g_free(fname);
	if(ftype)
		g_free(ftype);
	if(df)
		return df->smob;
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME

/*
 * Delete a wave file.
 * callback from menu: wavelist->file->delete
 */
void
delete_wave_file(GtkWidget *w, GWDataFile *wdata)
{
	int i;
/* remove references from displayed waves */
	remove_wfile_waves(wdata);

/* remove per-file GUI stuff */
	if(wdata->wlist_win && GTK_WIDGET_VISIBLE(wdata->wlist_win))
		gtk_widget_destroy(wdata->wlist_win);
	
/* invalidate handles to WaveVars */
	for(i = 0; i < wdata->wf->wf_ndv; i++) {
		wdata->wvhs[i].wv = NULL;
	}
/* now nuke the data */
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

SCM_DEFINE(datafile_delete_x, "wavefile-delete!", 1, 0, 0, 
           (SCM obj),
	   "Delete from memory the waveform data from OBJ.")
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

SCM_DEFINE(reload_all_files_x, "reload-all-files!", 0, 0, 0, (),
	   "Reload all files")
#define FUNC_NAME s_reload_all_files_x
{
	WaveFile *wf;
	g_list_foreach(wdata_list, reload_wave_file_w, NULL);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(datafile_reload_x, "wavefile-reload!", 1, 0, 0, 
           (SCM obj),
"Reread the data file for OBJ.  Useful for updating the display
    after simulation has been rerun.")
#define FUNC_NAME s_datafile_reload_x
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	if(wdata->wf)
		reload_wave_file(NULL, wdata);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * Return the GtkTooltips object used for gwave.
 */
GtkTooltips *
get_gwave_tooltips()
{
	extern SCM scm_gwave_tooltips;
	SCM scm_tt;
	assert( SCM_CONSP(scm_gwave_tooltips) );
	scm_tt = SCM_CDR(scm_gwave_tooltips);
	
	if( sgtk_is_a_gtkobj (GTK_TYPE_TOOLTIPS, scm_tt))
		return (GtkTooltips *) sgtk_get_gtkobj (scm_tt);
	else {
		fprintf(stderr, "not a GtkTooltips");
		exit(0);
	}
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
	GtkTooltips *gw_tooltips;
	gw_tooltips = get_gwave_tooltips();

	for(i = 0; i < wdata->wf->wf_ndv; i++) {
		WaveVar *dv = &wdata->wf->dv[i];
		button = gtk_button_new_with_label(dv->wv_name);
		
		gtk_box_pack_start (GTK_BOX (wdata->wlist_box), button, FALSE, FALSE, 0);
		gtk_widget_show (button);

		gtk_tooltips_set_tip(GTK_TOOLTIPS(gw_tooltips), button,
				     "Wavefile Variable.\nDrag-and-Drop to a WavePanel.",
				     "");

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

SCM_DEFINE(wavefile_show_listwin_x, "wavefile-show-listwin!", 1, 0, 0,
           (SCM obj),
"Displays the scrolling list of the variables in OBJ, from which they
can be dragged into a waveform display panel.")
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
SCM_DEFINE(wavefile_remove_listwin_x, "wavefile-remove-listwin!", 1, 0, 0,
           (SCM obj),
	   "Removes the variable-list window for OBJ")
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

SCM_DEFINE(wavefile_file_name, "wavefile-file-name", 1, 0, 0,
           (SCM obj),
"Returns the filename from which the GWDataFile OBJ was loaded. 
If OBJ is invalid because the datafile has been deleted,
#f is returned.")
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

SCM_DEFINE(wavefile_tag, "wavefile-tag", 1, 0, 0,
           (SCM obj),
	   "Returns the short identifying tag for the GWDataFile OBJ.")
#define FUNC_NAME s_wavefile_tag
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	return gh_str02scm(wdata->ftag);
}
#undef FUNC_NAME

SCM_DEFINE(wavefile_listwin_menubar, "wavefile-listwin-menubar", 1, 0, 0,
           (SCM obj),
"Returns the GTK Menubar for the variable-list window of the
 * GWDataFile OBJ, or #f if the window doesn't exist.")
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

SCM_DEFINE(wavefile_list, "wavefile-list", 0, 0, 0, (),
	   "Returns a list containing all waveform data files")
#define FUNC_NAME s_wavefile_list
{
	return glist2scm(wdata_list, wavefile_to_scm);
}
#undef FUNC_NAME


SCM_DEFINE(wavefile_all_variables, "wavefile-all-variables", 1, 0, 0, (SCM df),
	   "Returns a list of WaveVars, composed of all variables in the GWDataFile DF.")
#define FUNC_NAME s_wavefile_all_variables
{
	GWDataFile *wdata;
	SCM result = SCM_EOL;
	SCM wvsmob;
	int i;
	VALIDATE_ARG_GWDataFile_COPY(1, df, wdata);

	if(!wdata->wf)
		return result;
	for(i = 0; i < wdata->wf->wf_ndv; i++) {
		WaveVar *dv = &wdata->wf->dv[i];
		wdata->wvhs[i].wv = dv;
		SGT_NEWCELL_SMOB(wvsmob, WaveVar, &wdata->wvhs[i]);
		result = scm_cons(wvsmob, result);
	}
	return scm_reverse(result);
}
#undef FUNC_NAME


SCM_DEFINE(wavefile_variable, "wavefile-variable", 2, 0, 0,
	   (SCM df, SCM vname),
	   "Returns a WaveVar representing the variable named VNAME in the GWDataFile DF.  Return #f if there is no variable named VNAME")
#define FUNC_NAME s_wavefile_variable
{
	GWDataFile *wdata;
	SCM result = SCM_BOOL_F;
	char *s;
	int i;
	VALIDATE_ARG_GWDataFile_COPY(1, df, wdata);
	VALIDATE_ARG_STR_NEWCOPY(1, vname, s);

	if(wdata->wf) {
		for(i = 0; i < wdata->wf->wf_ndv; i++) {
			WaveVar *dv = &wdata->wf->dv[i];
			if(0==strcmp(s, dv->sv->name)) {
				wdata->wvhs[i].wv = dv;
				SGT_NEWCELL_SMOB(result, WaveVar, &wdata->wvhs[i]);
			}
		}
	}
	g_free(s);
	return result;
}
#undef FUNC_NAME


SCM_DEFINE(variable_signame, "variable-signame", 1, 0, 0,
	   (SCM var),
	   "Return the signal name for the variable VAR.")
#define FUNC_NAME s_variable_signame
{
	WaveVar *wv;
	VALIDATE_ARG_WaveVar_COPY(1,var,wv);
	
	if(wv)
		return gh_str02scm(wv->sv->name);
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE(variable_wavefile, "variable-wavefile", 1, 0, 0,
	   (SCM var),
	   "Return the WaveFile that the variable VAR is contained in.")
#define FUNC_NAME s_variable_wavefile
{
	WaveVarH *wvh;
	VALIDATE_ARG_WaveVarH_COPY(1,var,wvh);
	
	if(wvh->wv) {
		wvh->df->outstanding_smob = 1;
		return wvh->df->smob;
	} else
		return SCM_BOOL_F;
}
#undef FUNC_NAME


/*
 * On the C side we never free WaveVars without freeing the whole
 * WaveFile structure.  When guile GC's one, we invalidate the pointer
 * in the handle, and then check to see if we can dump the whole
 * structure. 
 * Methinks we need a more formal reference-counting scheme instead of
 * all this ad-hockery.
 */
int wavefile_try_free(GWDataFile *wdata)
{
	int i, n;
	if(wdata->outstanding_smob)
		return 0;
	if(wdata->wf)
		return 0;

	for(i = 0; i < wdata->ndv; i++) {
		if(wdata->wvhs[i].wv)
			return 0;
	}
	fprintf(stderr, "free GWDataFile 0x%x during gc\n", wdata);
	n = wdata->ndv;
	g_free(wdata->wvhs);
	g_free(wdata);
	return sizeof(GWDataFile) + n*sizeof(WaveVarH);
}

/* standard SMOB functions for GWDataFile: free, mark, print, GWDataFile? */
scm_sizet
free_GWDataFile(SCM obj)
{
	GWDataFile *wdata =GWDataFile(obj);
	wdata->outstanding_smob = 0;
	return wavefile_try_free(wdata);
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

SCM_DEFINE(GWDataFile_p, "GWDataFile?", 1, 0, 0,
           (SCM obj),
	   "Returns #t if OBJ is a gwave data file object, otherwise #f.")
#define FUNC_NAME s_GWDataFile_p
{
	return SCM_BOOL_FromBool(GWDataFile_P(obj));
}
#undef FUNC_NAME

/* standard SMOB functions for WaveVar: free, mark, print, WaveVar? */

scm_sizet
free_WaveVar(SCM obj)
{
	WaveVarH *wvh = WaveVarH(obj);
	wvh->wv = NULL;
	return wavefile_try_free(wvh->df);
}

SCM
mark_WaveVar(SCM obj)
{
	return SCM_BOOL_F;
}

int 
print_WaveVar(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
	WaveVarH *wvh = WaveVarH(obj);
	
	scm_puts("#<WaveVar ", port);
	if(wvh->wv) {
		scm_puts(wvh->df->wf->wf_filename, port);
		scm_puts(",", port);
		scm_puts(wvh->wv->sv->name, port);
	} else
		scm_puts("invalid", port);

	scm_putc('>', port);
	return 1;
}

SCM_DEFINE(WaveVar_p, "WaveVar?", 1, 0, 0,
           (SCM obj),
	   "Returns #t if OBJ is a wave-file variable object, otherwise #f.")
#define FUNC_NAME s_WaveVar_p
{
	return SCM_BOOL_FromBool(WaveVarH_P(obj));
}
#undef FUNC_NAME

/* guile initialization */

MAKE_SMOBFUNS(GWDataFile);
MAKE_SMOBFUNS(WaveVar);

void init_wavelist()
{
        REGISTER_SCWMSMOBFUNS(GWDataFile);
        REGISTER_SCWMSMOBFUNS(WaveVar);

#ifndef SCM_MAGIC_SNARFER
#include "wavelist.x"
#endif
}
