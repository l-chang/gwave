/*
 * cmd.c, part of the gwave waveform viewer tool
 *
 * Functions in this file implement basic user-interface functionality.
 * Later they can become callable from the extension language
 * when we add one (most will will require some glue).
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
 * Revision 1.4  1999/01/08 22:42:13  tell
 * minor changes to support wavepanel add/delete
 *
 * Revision 1.1  1998/12/26 04:38:06  tell
 * Initial revision
 *
 * Revision 1.3  1998/09/30 21:54:39  tell
 * Restructure zoom commands, creating cmd_zoom_absolute and cmd_zoom_relative
 * to factor out common code.  Add cmd_zoom_cursor and cmd_zoom_window.
 * Add supress_redraw hook for use in update_wavetable to keep things from
 * getting redrawn before we're ready when adding first wave.
 *
 * Revision 1.2  1998/09/17 18:31:58  tell
 * Fixed longstanding bug deleting multiple waves.
 * update scrollbar if min_xval/max_xval change.
 *
 * Revision 1.1  1998/09/01 21:28:02  tell
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

gint cmd_zoom_absolute(double start, double end)
{
	if(start <= end) {
		wtable->start_xval = start;
		wtable->end_xval = end;
	} else {
		wtable->start_xval = end;
		wtable->end_xval = start;
	}

	if(wtable->start_xval < wtable->min_xval)
		wtable->start_xval = wtable->min_xval;
	if(wtable->end_xval > wtable->max_xval)
		wtable->end_xval = wtable->max_xval;

	win_hsadj->page_size = fabs(wtable->end_xval - wtable->start_xval);
	win_hsadj->page_increment = win_hsadj->page_size/2;
	win_hsadj->step_increment = win_hsadj->page_size/100;
	win_hsadj->value = wtable->start_xval;
	win_hsadj->lower = wtable->min_xval;
	win_hsadj->upper = wtable->max_xval;

	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}

gint cmd_zoom_relative(double factor)
{
	double ocenter, owidth;
	g_assert(factor > DBL_EPSILON);

	ocenter = (wtable->start_xval + wtable->end_xval)/2;
	owidth = wtable->end_xval - wtable->start_xval;
	
	return cmd_zoom_absolute(ocenter - owidth / (factor * 2),
				 ocenter + owidth / (factor * 2));
}

gint cmd_zoom_in(GtkWidget *widget)
{
	return cmd_zoom_relative(2);
}

gint cmd_zoom_out(GtkWidget *widget)
{
	return cmd_zoom_relative(0.5);
}

gint cmd_zoom_full(GtkWidget *widget)
{
	return cmd_zoom_absolute(wtable->min_xval, wtable->max_xval);
}

gint cmd_zoom_cursors(GtkWidget *widget)
{
	if(!wtable->cursor[0]->shown)
		return 0;
	if(!wtable->cursor[1]->shown)
		return 0;
	return cmd_zoom_absolute(wtable->cursor[0]->xval,
			   wtable->cursor[1]->xval);
}

static void
zoom_window_finish(WavePanel *wp, int x1, int x2, gpointer data)
{
	double nstart, nend;
	nstart  = x2val(wp, x1);
	nend = x2val(wp, x2);
	cmd_zoom_absolute(nstart, nend);
}

gint cmd_zoom_window(GtkWidget *widget)
{
	select_x_range(zoom_window_finish, NULL);
	return 0;
}


typedef struct {
	WavePanel *wp;
	VisibleWave *vw;
} VWListItem;

static GList *vw_delete_list;

static void
vw_wp_list_if_selected(gpointer p, gpointer d)
{
	VisibleWave *vw = (VisibleWave *)p;
	WavePanel *wp = (WavePanel *)d;
	GtkToggleButton *btn = GTK_TOGGLE_BUTTON(vw->button);
	
	if(btn->active) {
		VWListItem *vdi = g_new(VWListItem, 1);
		vdi->wp = wp;
		vdi->vw = vw;
		vw_delete_list = g_list_append(vw_delete_list, vdi);
	}
}

/*
 * cmd_delete_selected_waves
 */
gint cmd_delete_selected_waves(GtkWidget *widget)
{
	int i;
	VWListItem *vdi;

	/* 
	 * Have to build a special list while traversing, and then do the
	 * deletes, because removing elements from the wp->vwlist 
	 * during its g_list_foreach is apparently a no-no.
	 */
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		g_list_foreach(wp->vwlist, vw_wp_list_if_selected, wp);
	}
	while((vdi = g_list_nth_data(vw_delete_list, 0)) != NULL) {
		remove_wave_from_panel(vdi->wp, vdi->vw);
		vw_delete_list = g_list_remove(vw_delete_list, vdi);
		g_free(vdi);
	}
	
	return 0;
}

/*
 * remove waveform from panel
 * 	Somthing bad will probably happen if the waveform isn't actually
 * 	in the indicated panel.
 */
void
remove_wave_from_panel(WavePanel *wp, VisibleWave *vw)
{

	wp->vwlist = g_list_remove(wp->vwlist, vw);

	gtk_widget_destroy(vw->button);

	gdk_gc_destroy(vw->gc);
	g_free(vw->varname);
	g_free(vw);

	wavepanel_update_data(wp);
	wavetable_update_data();
}

/*
 * remove_wfile_waves - 
 * Remove from their respective panels all waveforms from the specified file.
 * This works just like delete_selected_waves, except for the test
 * that puts them on the to-delete list.  
 * TODO: factor out this common code.
 */

struct wp_file_pkg {
	WavePanel *wp;
	GWDataFile *gdf;
};

void vw_list_if_wfile(gpointer p, gpointer d)
{
	VisibleWave *vw = (VisibleWave *)p;
	struct wp_file_pkg *foo = (struct wp_file_pkg *)d;

	if(vw->gdf == foo->gdf) {
		VWListItem *vdi = g_new(VWListItem, 1);
		vdi->wp = foo->wp;
		vdi->vw = vw;
		vw_delete_list = g_list_append(vw_delete_list, vdi);
	}
}

void
remove_wfile_waves(GWDataFile *wdata)
{
	int i;
	VWListItem *vdi;
	struct wp_file_pkg foo;
	foo.gdf = wdata;

	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		foo.wp = wp;
		g_list_foreach(wp->vwlist, vw_list_if_wfile, &foo);
	}
	while((vdi = g_list_nth_data(vw_delete_list, 0)) != NULL) {
		/*printf("tearing down %s from %s\n", vdi->vw->varname, wdata->wf->wf_filename); */
		remove_wave_from_panel(vdi->wp, vdi->vw);
		vw_delete_list = g_list_remove(vw_delete_list, vdi);
		g_free(vdi);
	}
}

/*
 * Repoint all visible waves that reference this file to the new data.   
 * Any visiblewaves referencing variables that no longer exist in the new file
 * are deleted.
 */
void
update_wfile_waves(GWDataFile *wdata)
{
	int i;
	VWListItem *vdi;
	struct wp_file_pkg foo;
	WaveVar  *wv;
	int foundone = 0;

	foo.gdf = wdata;

	/* get a list of all VisibleWaves referencing this file */
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		foo.wp = wp;
		g_list_foreach(wp->vwlist, vw_list_if_wfile, &foo);
	}
	while((vdi = g_list_nth_data(vw_delete_list, 0)) != NULL) {
		foundone = 1;
		wv = wf_find_variable(wdata->wf, vdi->vw->varname);
		if(wv) {
			/* printf("updated variable %s to %lx\n", vdi->vw->varname, wdata->wf); */
			vdi->vw->gdf = wdata;
			vdi->vw->var = wv;
		} else {
			/* printf("variable %s no longer in file %s; removing from panel\n", vdi->vw->varname, wdata->wf->wf_filename); */
			remove_wave_from_panel(vdi->wp, vdi->vw);
		}

		vw_delete_list = g_list_remove(vw_delete_list, vdi);
		g_free(vdi);
	}

	if(foundone) {
		for(i = 0; i < wtable->npanels; i++) {
			WavePanel *wp = wtable->panels[i];
			wavepanel_update_data(wp);
		}
		wavetable_update_data();
	}
}

/*
 * Add a new waveform to a WavePanel
 */
void
add_var_to_panel(WavePanel *wp, WaveVar *dv)
{
	VisibleWave *vw;

	vw = g_new0(VisibleWave, 1);
	vw->var = dv;
	vw->varname = g_strdup(dv->wv_name);
	vw->gdf = (GWDataFile *)dv->udata;
	vw->colorn = wp->nextcolor;
	wp->nextcolor = (wp->nextcolor + 1)%NWColors;

	wp->vwlist = g_list_append(wp->vwlist, vw);
	wavepanel_update_data(wp);
	wavetable_update_data();

	if(wp->lvbox)  /* add button to Y-label box */
		vw_wp_create_button(vw, wp);
	if(wp->drawing) {
/*		vw_wp_setup_gc(vw, wp); */
		vw_wp_visit_draw(vw, wp);
	}
}

/*
 * called with g_list_foreach to update a WavePanel from all of its
 * VisibleWaves.
 */
static void
vw_wp_visit_update_data(gpointer p, gpointer d)
{
	VisibleWave *vw = (VisibleWave *)p;
	WavePanel *wp = (WavePanel *)d;

	if(vw->var->wv_iv->wds->min < wp->min_xval)
		wp->min_xval = vw->var->wv_iv->wds->min;
	if(vw->var->wv_iv->wds->max > wp->max_xval)
		wp->max_xval = vw->var->wv_iv->wds->max;

	if(vw->var->wds[0].min < wp->min_yval)
		wp->min_yval = vw->var->wds[0].min;
	if(vw->var->wds[0].max > wp->max_yval)
		wp->max_yval = vw->var->wds[0].max;
}

/* FIXME:sgt: wavepanel_update_data and wavetable_update_data
 * need a rethink and rewrite; they still don't do the right
 * thing in all cases.
 */

/*
 * wavepanel_update_data
 *   update wavepanel values that sumarize things over all of the 
 *   VisibleWaves in the panel.
 */
void
wavepanel_update_data(WavePanel *wp)
{
	char lbuf[128];

	wp->min_xval = G_MAXDOUBLE;
	wp->max_xval = G_MINDOUBLE;
	wp->min_yval = G_MAXDOUBLE;
	wp->max_yval = G_MINDOUBLE;
	g_list_foreach(wp->vwlist, vw_wp_visit_update_data, (gpointer)wp);

	/* set to something reasonable if they didn't change,
	 * like if the panel was empty
	 */
	if(wp->min_xval == G_MAXDOUBLE)
		wp->min_xval = 0; /* wtable->min_xval; */
	if(wp->max_xval == G_MINDOUBLE)
		wp->max_xval = 0; /* wtable->max_xval; */
	if(wp->min_yval == G_MAXDOUBLE)
		wp->min_yval = 0.0;
	if(wp->max_yval == G_MINDOUBLE)
		wp->max_yval = 3.3;

	/* zero height? set to +- 0.1%  so a line is visible in the center */
	if((wp->max_yval - wp->min_yval) < DBL_EPSILON) {
		wp->max_yval *= 1.001;
		wp->min_yval *= 0.999;
	}

	/* if start & end were the same, try updating them
	 * -- this probably isn't quite right.
	 */
	if(fabs(wp->end_xval - wp->start_xval) < DBL_EPSILON) {
		wp->start_xval = wp->min_xval;
		wp->end_xval = wp->max_xval;
	}
	if(wp->start_xval < wp->min_xval)
		wp->start_xval = wp->min_xval;
	if(wp->end_xval > wp->max_xval)
		wp->end_xval = wp->max_xval;

	/* Update y-axis labels */
	if(wp->lab_min) {
		sprintf(lbuf, "%.3f", wp->min_yval);
		gtk_label_set(GTK_LABEL(wp->lab_min), lbuf);
	}
	if(wp->lab_max) {
		sprintf(lbuf, "%.3f", wp->max_yval);
		gtk_label_set(GTK_LABEL(wp->lab_max), lbuf);
	}
}

/* Update parameters in wavetable that depend on all panels */
void
wavetable_update_data()
{
	int i;
	WavePanel *wp;
	double old_min_x, old_max_x;
	old_min_x = wtable->min_xval;
	old_max_x = wtable->max_xval;

	wtable->min_xval = G_MAXDOUBLE;
	wtable->max_xval = G_MINDOUBLE;
	for(i = 0; i < wtable->npanels; i++) {
		wp = wtable->panels[i];
		if(wp == NULL)
			continue; /* deleted panel */
		if(wp->vwlist == NULL)
			continue; /* no waves? min/max for panel are bogus */

		if(wp->min_xval < wtable->min_xval)
			wtable->min_xval = wp->min_xval;
		if(wp->max_xval > wtable->max_xval)
			wtable->max_xval = wp->max_xval;
	}
	/* still nothing? set back to zero */
	if(wtable->min_xval == G_MAXDOUBLE)
		wtable->min_xval = 0;
	if(wtable->max_xval == G_MINDOUBLE)
		wtable->max_xval = 0;

	/* if start & end were the same or out of range, 
	* just zoom-full so we can see somthing.
	*/
	if((fabs(wtable->end_xval - wtable->start_xval) < DBL_EPSILON
	    || wtable->start_xval < wtable->min_xval
	    || wtable->end_xval > wtable->max_xval)
	   && win_hsadj != NULL) {
		wtable->suppress_redraw = 1;
		cmd_zoom_full(NULL);
		wtable->suppress_redraw = 0;
	} else if((wtable->min_xval != old_min_x 
		  || wtable->max_xval != old_max_x) && win_hsadj) {

		/* min/max changed, might have added first (or removed last)
		 * wave from a file with different range.
		 * try to keep start/end same, but make them sane if needed.
		 * then update scrollbar.
		 */
		if(wtable->start_xval < wtable->min_xval)
			wtable->start_xval = wtable->min_xval;
		if(wtable->end_xval > wtable->max_xval)
			wtable->end_xval = wtable->max_xval;
		win_hsadj->page_size = fabs(wtable->end_xval - wtable->start_xval);
		win_hsadj->page_increment = win_hsadj->page_size/2;
		win_hsadj->step_increment = win_hsadj->page_size/100;
		win_hsadj->value = wtable->start_xval;
		win_hsadj->lower = wtable->min_xval;
		win_hsadj->upper = wtable->max_xval;

		wtable->suppress_redraw = 1;
		gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");
		wtable->suppress_redraw = 0;

	}
}

