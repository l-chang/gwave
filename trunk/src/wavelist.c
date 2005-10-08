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
 * Revision 1.27  2005/10/08 04:55:33  sgt
 * fix wavefile-all-variables to work with all sweeps
 * create wavefile-sweeps to return list of sweep info
 *
 * Revision 1.26  2005/09/30 04:31:36  sgt
 * add scrollbar to wavepanel button/measure lists to better control panel
 * height vs. number of buttons
 * fix gc issue with sweep rework
 *
 * Revision 1.25  2005/09/27 05:33:53  sgt
 * sweeps working decently well,
 * although wavelist selection for them is ugly.
 *
 * Revision 1.24  2004/12/26 23:49:32  sgt
 * add notion of "selected" panels.  left-click in panel (including cursor0-move)
 * selects, shift-click selects without unselecting others.
 * double-click in wavelist adds to first selected panel.
 * selected panels have a highlighted outline.
 * add ability to change the "tag" (shorthand identifier) for a file
 * added example of using this to put case name into the tag to
 * experimental .gwaverc
 *
 * Revision 1.23  2003/12/03 03:58:16  sgt
 * clean up string literals containing newlines, all in documentation
 * strings.
 * Modify xsnarf.h to get document-extraction working again
 *
 * Revision 1.22  2003/07/13 23:44:42  sgt
 *
 * some further work on export
 *
 * Revision 1.21  2003/06/02 13:10:33  sgt
 * new code for data-export functions
 * make export-variables able to accept either WaveVar or VisibleWave to export
 *
 * Revision 1.20  2003/05/22 05:03:09  sgt
 * Add C-level primitive export-variables, for use in writing new
 * plot/export features.
 * update TODO list
 * add info about GNU plotutils breakage and patch
 * bump version number.
 *
 * Revision 1.19  2002/04/19 17:21:33  sgt
 * Add configure check for scm_c_read_string, and finish ifdeffing in gwave.c
 * so that we build and run properly on 1.3.4 again.
 * Fix a snarf-macro renaming that we missed in wavelist.c
 *
 * Revision 1.18  2002/03/28 06:35:21  sgt
 * Snarfing overhaul to make it guile-version-independent
 * now compiles and runs under guile-1.5.6
 * some issues remain - bound? in cmds.scm for example.
 *
 * Revision 1.17  2001/06/01 16:11:56  sgt
 * create mbtn_update_var() subroutine, and use it to fix up measure-button
 * pointers into wave data after a reaload.  This avoids a crash after reloading.
 *
 * Revision 1.16  2000/11/28 03:22:35  sgt
 * draw.c: expand to five sig-figs in displayed values
 * wavelist: properly free up old wave data after a reload.
 *
 * Revision 1.15  2000/10/24 06:33:08  sgt
 * cmd.c - make wavepanel-add-variable return the new VisibleWave
 * wavelist.c - fix wavefile-variable.
 *
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
#include <sys/time.h>

#include <gtk/gtk.h>
#include <guile-gtk.h>
#include <config.h>
#include <scwm_guile.h>
#include <gwave.h>

#define WAVELIST_IMPLEMENTATION
#include <wavelist.h>

#include <wavewin.h>

GList *wdata_list = NULL;
static char file_tag_chars[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static const int n_file_tags = sizeof(file_tag_chars)/sizeof(char);
static int next_file_tagno = 0;

static GtkWidget *create_wavelist_menu(GWDataFile *wdata);
void gwfile_add_wv_to_list(gpointer d /*WaveVar* */,
			   gpointer p /*GWDataFile */);

static gint wavelist_button_click(GtkWidget *widget,
				  GdkEventButton *event, gpointer data);

XSCM_HOOK(new_wavefile_hook,"new-wavefile-hook", 1, (SCM DF),
"This hook is invoked when a new waveform file is successfully loaded."
"It is called with the new GWDataFile, DF, as its only argument.");

XSCM_HOOK(new_wavelist_hook,"new-wavelist-hook", 1, (SCM DF),
"This hook is invoked when the variable list window for a"
"GWDataFile is created.  The GWDataFile object, DF, is passed as an"
"argument.  Note that variable-list windows can be created and"
"destroyed many times during the life of a GWDataFile.  One of the principle"
"uses of this hook is creating the menus such for the variable-list window.");

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
	wdata->wf->udata = wdata;

	/* give the file a short (fow now, 1-character) "tag" to identify it
	 * in the menu and variable labels.  
	 */
	wdata->ftag = g_new(char, 2);
	wdata->ftag[0] = file_tag_chars[next_file_tagno];
	wdata->ftag[1] = '\0';
	next_file_tagno = (next_file_tagno + 1) % n_file_tags;
	wdata->ndv = wdata->wf->wf_ndv;
	wdata->wvhl = NULL; /* empty GSList  of WaveVarH* */

	wdata_list = g_list_append(wdata_list, wdata);
	wdata->outstanding_smob = 1;
	SGT_NEWCELL_SMOB(wdata->smob, GWDataFile, wdata);
	call1_hooks(new_wavefile_hook, wdata->smob);

	if(win_main)
		cmd_show_wave_list(NULL, wdata);

	return wdata;
}

XSCM_DEFINE(load_wavefile_x, "load-wavefile!", 1, 1, 0, (SCM file, SCM filetype),
"Load waveform data from FILE into memory, and make it available for"
"display.  If FILETYPE is specified, it indicates the format of the file"
"and which wavefile reader to use, otherwise the format is inferred"
"from the filename and file contents.  Returns a GWDataFile object"
"which can be used to refer to the loaded data.")
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
	GSList *list;
/* remove references from displayed waves */
	remove_wfile_waves(wdata);

/* remove per-file GUI stuff */
	if(wdata->wlist_win && GTK_WIDGET_VISIBLE(wdata->wlist_win))
		gtk_widget_destroy(wdata->wlist_win);
	
/* invalidate WaveVar pointers in handles.
 * Can't free WaveVar because un-GCed smobs may point to them
 */	
	for(list = wdata->wvhl; list; list = list->next) {
		WaveVarH *wvh = (WaveVarH *)list->data;
		wvh->wv = NULL;
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

XSCM_DEFINE(datafile_delete_x, "wavefile-delete!", 1, 0, 0, 
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

	update_wfile_waves(wdata);

/* remove old buttons from list, and add new ones */	
	if(wdata->wlist_win && GTK_WIDGET_VISIBLE(wdata->wlist_win)) {
		gtk_container_foreach(GTK_CONTAINER(wdata->wlist_box),
				      (GtkCallback) gtk_widget_destroy, NULL);
		wf_foreach_wavevar(wdata->wf, gwfile_add_wv_to_list, wdata);
	}

	wf_free(old_wf);
	mbtn_update_all();
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

XSCM_DEFINE(reload_all_files_x, "reload-all-files!", 0, 0, 0, (),
	   "Reload all files")
#define FUNC_NAME s_reload_all_files_x
{
	WaveFile *wf;
	g_list_foreach(wdata_list, reload_wave_file_w, NULL);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

XSCM_DEFINE(datafile_reload_x, "wavefile-reload!", 1, 0, 0, 
           (SCM obj),
"Reread the data file for OBJ.  Useful for updating the display"
"    after simulation has been rerun.")
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
	g_assert( SCM_CONSP(scm_gwave_tooltips) );
	scm_tt = SCM_CDR(scm_gwave_tooltips);
	
	if( sgtk_is_a_gtkobj (GTK_TYPE_TOOLTIPS, scm_tt))
		return (GtkTooltips *) sgtk_get_gtkobj (scm_tt);
	else {
		fprintf(stderr, "not a GtkTooltips");
		exit(0);
	}
}

/*
 * Callback for use with wv_foreach_wavevar:
 *
 * Add a button for each variable in the file to the win_wlist box for it.
 * Arrange for the buttons to be drag-and-drop sources for placing the
 * variables into wavepanels.
 *
 * formerly add_variables_to_list(GWDataFile *wdata)
 */
void
gwfile_add_wv_to_list(gpointer d, gpointer p)
{
	WaveVar *wv = (WaveVar *)d;
	GWDataFile *wdata = (GWDataFile *)p;
	int i;
	GtkWidget *button;
	GtkTooltips *gw_tooltips;
	
	gw_tooltips = get_gwave_tooltips();

	if(wv_is_multisweep(wv)) {
		char lab[4096];
		sprintf(lab, "%s @ %s=%g", wv->wv_name, 
			wv->wtable->name, wv->wtable->swval);
		button = gtk_button_new_with_label(lab);
	} else {
		button = gtk_button_new_with_label(wv->wv_name);
	}
		
	gtk_box_pack_start (GTK_BOX (wdata->wlist_box), button, FALSE, FALSE, 0);
	gtk_widget_show (button);
	gtk_tooltips_set_tip(GTK_TOOLTIPS(gw_tooltips), button,
		     "Wavefile Variable.\nDrag-and-Drop to a WavePanel.",
		     "");

	dnd_setup_source(wdata->wlist_win, button, wv);

	gtk_signal_connect (GTK_OBJECT(button), "button-press-event",
			    GTK_SIGNAL_FUNC(wavelist_button_click), 
			    (gpointer) wv);
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
		   * This nonsense really belongs in a smarter window manager, 
		   * but users are demanding somthing.
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
                                  GTK_POLICY_ALWAYS);
		GTK_WIDGET_UNSET_FLAGS (GTK_SCROLLED_WINDOW (scrolled_window)->vscrollbar, GTK_CAN_FOCUS);
		gtk_box_pack_start(GTK_BOX (box1), scrolled_window,
				   TRUE, TRUE, 0);
		gtk_widget_show (scrolled_window);

		wdata->wlist_box = gtk_vbox_new (FALSE, 0);
		gtk_container_border_width (GTK_CONTAINER (wdata->wlist_box), 10);
		gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scrolled_window),
						      wdata->wlist_box);
		gtk_container_set_focus_vadjustment(
			GTK_CONTAINER (wdata->wlist_box),
			gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(scrolled_window)));
		gtk_widget_show (wdata->wlist_box);

		dnd_init(wdata->wlist_win);
		wf_foreach_wavevar(wdata->wf, gwfile_add_wv_to_list, wdata);

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

XSCM_DEFINE(wavefile_show_listwin_x, "wavefile-show-listwin!", 1, 0, 0,
           (SCM obj),
"Displays the scrolling list of the variables in OBJ, from which they"
"can be dragged into a waveform display panel.")
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
XSCM_DEFINE(wavefile_remove_listwin_x, "wavefile-remove-listwin!", 1, 0, 0,
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

XSCM_DEFINE(wavefile_file_name, "wavefile-file-name", 1, 0, 0,
           (SCM obj),
"Returns the filename from which the GWDataFile OBJ was loaded."
"If OBJ is invalid because the datafile has been deleted,"
"#f is returned.")
#define FUNC_NAME s_wavefile_file_name
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	if(wdata->wf)
		return scm_makfrom0str(wdata->wf->wf_filename);
	else
		return SCM_BOOL_F;

}
#undef FUNC_NAME

XSCM_DEFINE(wavefile_nsweeps, "wavefile-nsweeps", 1, 0, 0,
           (SCM df),
	   "Returns the number of sweeps for which data is present in GWDataFile DF.")
#define FUNC_NAME s_wavefile_nsweeps
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, df, wdata);

	return scm_long2num(wdata->wf->wf_ntables);
}
#undef FUNC_NAME

XSCM_DEFINE(wavefile_sweeps, "wavefile-sweeps", 1, 0, 0,
           (SCM df),
	   "Returns a list of sweeps contained in GWDataFile DF.  Each element of the list is a pair, of the form (sweepname . sweepvalue)")
#define FUNC_NAME s_wavefile_nsweeps
{
	GWDataFile *wdata;
	SCM result = SCM_EOL;
	SCM p;
	WvTable *wt;
	WaveFile *wf;
	int i, j;
	VALIDATE_ARG_GWDataFile_COPY(1, df, wdata);

	if(!wdata->wf)
		return result;

	wf = wdata->wf;
	for(i = 0; i < wf->wf_ntables; i++) {
		wt = wf_wtable(wf, i);
		p = scm_cons(scm_makfrom0str(wt->name), scm_make_real(wt->swval));
		result = scm_cons(p, result);
	}
	return scm_reverse(result);
}
#undef FUNC_NAME

XSCM_DEFINE(wavefile_tag, "wavefile-tag", 1, 0, 0,
           (SCM obj),
	   "Returns the short identifying tag for the GWDataFile OBJ.")
#define FUNC_NAME s_wavefile_tag
{
	GWDataFile *wdata;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);

	return scm_makfrom0str(wdata->ftag);
}
#undef FUNC_NAME

XSCM_DEFINE(wavefile_set_tag_x, "wavefile-set-tag!", 2, 0, 0,
	    (SCM obj, SCM str),
	   "Set the short identifying tag for the GWDataFile OBJ to STR.")
#define FUNC_NAME s_wavefile_set_tag_x
{
	GWDataFile *wdata;
	char *s;
	VALIDATE_ARG_GWDataFile_COPY(1, obj, wdata);
	VALIDATE_ARG_STR_NEWCOPY(1, str, s);
	g_free(wdata->ftag);
	wdata->ftag = s;
	return SCM_UNSPECIFIED;
/* BUG: any visiblewave button labels and wavelist menu entries
 * won't be affected by the change in ftag */
}
#undef FUNC_NAME

XSCM_DEFINE(wavefile_listwin_menubar, "wavefile-listwin-menubar", 1, 0, 0,
           (SCM obj),
"Returns the GTK Menubar for the variable-list window of the"
" * GWDataFile OBJ, or #f if the window doesn't exist.")
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

XSCM_DEFINE(wavefile_list, "wavefile-list", 0, 0, 0, (),
	   "Returns a list containing all waveform data files")
#define FUNC_NAME s_wavefile_list
{
	return glist2scm(wdata_list, wavefile_to_scm);
}
#undef FUNC_NAME


XSCM_DEFINE(wavefile_all_variables, "wavefile-all-variables", 1, 0, 0, (SCM df),
	   "Returns a list of WaveVars, composed of all variables in the GWDataFile DF.")
#define FUNC_NAME s_wavefile_all_variables
{
	GWDataFile *wdata;
	SCM result = SCM_EOL;
	SCM wvsmob;

	WaveFile *wf;
	WvTable *wt;
	int i, j;
	VALIDATE_ARG_GWDataFile_COPY(1, df, wdata);

	if(!wdata->wf)
		return result;

	wf = wdata->wf;
	for(i = 0; i < wf->wf_ntables; i++) {
		wt = wf_wtable(wf, i);
		for(j = 0; j < wf->wf_ndv; j++) {
			WaveVar *wv;
			WaveVarH *wvh;
			wv = &wt->dv[j];
			if(!wv->udata) {
                                wvh = g_new0(WaveVarH, 1);
                                wvh->wv = wv;
                                wvh->df = wdata;
                                wv->udata = wvh;
                                wdata->wvhl = g_slist_prepend(wdata->wvhl, wvh);
			} else {
				wvh = (WaveVarH *)wv->udata;
			}
			SGT_NEWCELL_SMOB(wvsmob, WaveVar, wvh);
			result = scm_cons(wvsmob, result);
		}
	}
	return scm_reverse(result);
}
#undef FUNC_NAME


XSCM_DEFINE(wavefile_variable, "wavefile-variable", 3, 0, 0,
	    (SCM df, SCM vname, SCM swindex),
   "Returns a WaveVar representing the variable named VNAME in sweep/table/segment SWINDEX in the GWDataFile DF.  Return #f if there is no variable named VNAME")
#define FUNC_NAME s_wavefile_variable
{
	GWDataFile *wdata;
	SCM result = SCM_BOOL_F;
	char *s;
	int i;
	int swp;
	VALIDATE_ARG_GWDataFile_COPY(1, df, wdata);
	VALIDATE_ARG_STR_NEWCOPY(2, vname, s);
	VALIDATE_ARG_INT_MIN_COPY(3, swindex, 0, swp);
	
	if(wdata->wf && swp < wdata->wf->wf_ntables) {
		WaveVar *wv = wf_find_variable(wdata->wf, s, swp);
		if(wv) {
			WaveVarH *wvh;
			if(!wv->udata) {
				wvh = g_new0(WaveVarH, 1);
				wvh->wv = wv;
				wvh->df = wdata;
				wv->udata = wvh;
				wdata->wvhl = g_slist_prepend(wdata->wvhl, wvh);
			} else {
				wvh = (WaveVarH *)wv->udata;
			}
			SGT_NEWCELL_SMOB(result, WaveVar, wvh);
		}
	}
	g_free(s);
	return result;
}
#undef FUNC_NAME

XSCM_DEFINE(variable_signame, "variable-signame", 1, 0, 0,
	   (SCM var),
	   "Return the signal name for the variable VAR.")
#define FUNC_NAME s_variable_signame
{
	WaveVar *wv;
	VALIDATE_ARG_VisibleWaveOrWaveVar_COPY(1,var,wv);
	
	if(wv)
		return scm_makfrom0str(wv->sv->name);
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME


XSCM_DEFINE(variable_sweepname, "variable-sweepname", 1, 0, 0,
	   (SCM var),
	   "Return the sweep name or table name for the variable VAR.")
#define FUNC_NAME s_variable_sweepname
{
	WaveVar *wv;
	VALIDATE_ARG_VisibleWaveOrWaveVar_COPY(1,var,wv);
	
	if(wv)
		return scm_makfrom0str(wv->wtable->name);
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME


XSCM_DEFINE(variable_sweepindex, "variable-sweepindex", 1, 0, 0,
	   (SCM var),
	   "Return the sweep table index for the variable VAR.  Sweeps/tables are numbered starting with 0. ")
#define FUNC_NAME s_variable_sweepindex
{
	WaveVar *wv;
	VALIDATE_ARG_VisibleWaveOrWaveVar_COPY(1,var,wv);
	
	if(wv)
		return scm_long2num(wv->wtable->swindex);
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME


XSCM_DEFINE(variable_wavefile, "variable-wavefile", 1, 0, 0,
	   (SCM var),
	   "Return the WaveFile that the variable VAR is contained in.")
#define FUNC_NAME s_variable_wavefile
{
	WaveVarH *wvh;
	VALIDATE_ARG_VisibleWaveOrWaveVar_COPY(1,var,wvh);
	
	if(wvh->wv) {
		wvh->df->outstanding_smob = 1;
		return wvh->df->smob;
	} else
		return SCM_BOOL_F;
}
#undef FUNC_NAME

XSCM_DEFINE(export_variables, "export-variables", 2, 2, 0,
	    (SCM varlist, SCM port, SCM from, SCM to),
"Write the data for all variables in VARLIST to PORT in tabular ascii form"
"If FROM and TO are specified, writes only data points for which the"
"independent variable is between FROM and TO includsive."
"All variables in VARLIST must share the same independent variable")
#define FUNC_NAME s_export_variables
{
	SCM l, v;
	WaveVar *wv;
	WaveVar *iv = NULL;
	double from_val, to_val;
	int starti, endi, i;
	double x,y;
	int idx;
	char buf[128];
	SCM_ASYNC_TICK;
	/* validate varlist and count elements */
	for (l = varlist; SCM_NNULLP(l); l = SCM_CDR (l)) {
                v = SCM_CAR(l);
		VALIDATE_ARG_VisibleWaveOrWaveVar_COPY(1,v,wv);
		if(!wv) {
			scm_misc_error(FUNC_NAME, "invalid WaveVar ~s", SCM_LIST1(v));
		}
		if(iv == NULL)
			iv = wv->wv_iv;
		else if(iv != wv->wv_iv) {
			scm_misc_error(FUNC_NAME, "All WaveVars in VARLIST must relate to the same independent variable", SCM_UNDEFINED);
		}
	}
	VALIDATE_ARG_DBL_COPY_USE_DEF(3,from,from_val, iv->wds[0].min);
	VALIDATE_ARG_DBL_COPY_USE_DEF(4,to,to_val, iv->wds[0].max);
	
	if(from_val > to_val)
		return SCM_UNSPECIFIED;
	starti = wf_find_point(iv, from_val);
	endi = wf_find_point(iv, to_val);
	
	for(i = starti; i <= endi; i++) {
		x = wds_get_point(&iv->wds[0], i);
		sprintf(buf, "%g", x); 
		scm_puts(buf, port);
		for (l = varlist; SCM_NNULLP(l); l = SCM_CDR (l)) {
			v = SCM_CAR(l);
			VALIDATE_ARG_VisibleWaveOrWaveVar_COPY(1,v,wv);
			g_assert(wv);  /* should have been checked above */
			y = wds_get_point(&wv->wds[0], i);
			sprintf(buf, " %g", y); 
			scm_puts(buf, port);
		}
		scm_puts("\n", port);
	}

	return SCM_UNSPECIFIED;
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
	if(wdata->wvhl)  /* nonempty list means outstanding handles remain */
		return 0;

/*	fprintf(stderr, "free GWDataFile 0x%x during gc\n", wdata); */
	n = wdata->ndv;
	g_free(wdata);
	return sizeof(GWDataFile);
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

XSCM_DEFINE(GWDataFile_p, "GWDataFile?", 1, 0, 0,
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
	GWDataFile *df;
	scm_sizet fsize;
	df = wvh->df;

	df->wvhl = g_slist_remove(df->wvhl, wvh);
	fsize = wavefile_try_free(wvh->df);
	wvh->wv = NULL;
	wvh->df = NULL;
	g_free(wvh);

	return fsize + sizeof(WaveVarH);
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
	char buf[128];

	scm_puts("#<WaveVar ", port);
	if(wvh->wv) {
		scm_puts(wvh->df->wf->wf_filename, port);
		scm_puts(",", port);
		sprintf(buf, "%d", wvh->wv->wtable->swindex);
		scm_puts(buf,port);
		scm_puts(",", port);
		scm_puts(wvh->wv->sv->name, port);
	} else
		scm_puts("invalid", port);

	scm_putc('>', port);
	return 1;
}

XSCM_DEFINE(WaveVar_p, "WaveVar?", 1, 0, 0,
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

#ifndef XSCM_MAGIC_SNARF_INITS
#include "wavelist.x"
#endif
}
