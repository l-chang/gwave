/*
 * cmd.c, part of the gwave waveform viewer tool
 *
 * Functions in this file implement basic user-interface functionality.
 * Later they can become callable from the extension language
 * when we add one (most will will require some glue).
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
 * You should have received a copy of the GNU Library General Public
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

#include <scwm_guile.h>
#include <gwave.h>
#include <wavelist.h>
#include <wavewin.h>
#include <measurebtn.h>

XSCM_HOOK(new_visiblewave_hook, "new-visiblewave-hook", 1, (SCM vw),
"This hook is invoked with one VisibleWave argument, VW,
when the VisibleWave is first created.   The main purpose of this hook 
will be to create the button and menus attached to the VisibleWave.");

/* reset the x zoom scale of all panels */
gint cmd_zoom_absolute(double start, double end)
{
 	double scroll_start, scroll_end;

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

 	/* Scroll bar always goes from zero to one.
	   Preform an appropriate transform based on lin/log */
 	if (!wtable->logx) {
 		scroll_start = ( wtable->start_xval - wtable->min_xval ) 
			/ ( wtable->max_xval - wtable->min_xval );
 		scroll_end   = ( wtable->end_xval   - wtable->min_xval ) 
			/ ( wtable->max_xval - wtable->min_xval );
 	} else {
 		scroll_start = log( wtable->start_xval / wtable->min_xval ) 
			/ log( wtable->max_xval / wtable->min_xval );
 		scroll_end   = log( wtable->end_xval   / wtable->min_xval ) 
			/ log( wtable->max_xval / wtable->min_xval );
 	}
  	win_hsadj->page_size = fabs( scroll_end - scroll_start );

	win_hsadj->page_increment = win_hsadj->page_size/2;
	win_hsadj->step_increment = win_hsadj->page_size/100;
 	win_hsadj->value = scroll_start;
 	win_hsadj->lower = 0.0;
 	win_hsadj->upper = 1.0;

	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}

XSCM_DEFINE(x_zoom_x, "x-zoom!", 2, 0, 0, (SCM start, SCM end),
	   "zoom/rescale all wavepanels so that the x axis displays from START to END")
#define FUNC_NAME s_x_zoom_x
{
	double dstart, dend;
	VALIDATE_ARG_DBL_COPY(1, start, dstart);
	VALIDATE_ARG_DBL_COPY(1, end, dend);
	cmd_zoom_absolute(dstart, dend);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

gint cmd_zoom_full(GtkWidget *widget)
{
	return cmd_zoom_absolute(wtable->min_xval, wtable->max_xval);
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

XSCM_DEFINE(delete_selected_waves_x, "delete-selected-waves!", 0, 0, 0, (),
  "Remove from panels any VisibleWaves that have been
selected by clicking on their label-buttons.")
#define FUNC_NAME s_delete_selected_waves_x
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
	wtable_redraw_x();

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * remove waveform from panel
 * 	Somthing bad will probably happen if the waveform isn't actually
 * 	in the indicated panel.
 */
void
remove_wave_from_panel(WavePanel *wp, VisibleWave *vw)
{
	int row;
	wp->vwlist = g_list_remove(wp->vwlist, vw);

	mbtn_delete(vw->mbtn[0]);
	mbtn_delete(vw->mbtn[1]);

	row = gtk_table_get_child_row(wp->lmtable, vw->button);
	if(row == -1) {
		fprintf(stderr, "remove_wave_from_panel(): VisibleWave not in this panel\n");
	} else {
		gtk_table_delete_row(wp->lmtable, row);
	}

	gdk_gc_destroy(vw->gc);
	g_free(vw->varname);

	vw->valid = 0;
	scm_unprotect_object(vw->smob);
	if(vw->outstanding_smob == 0)
		g_free(vw);

	wavepanel_update_data(wp);
	wavetable_update_data();
}
/* delete the indicated VisibleWave.
 * TODO: replace calls to remove_wave_from_panel with this.
 */
void
visiblewave_delete(VisibleWave *vw)
{
	WavePanel *wp = vw->wp;
	remove_wave_from_panel(wp, vw);

	draw_wavepanel(wp->drawing, NULL, wp);
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

	/* get a list of all VisibleWaves referencing this file 
	 * The list is pointed to by the global wv_delete_list
	 */
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
			mbtn_update_var(vdi->vw->mbtn[0], wv);
			mbtn_update_var(vdi->vw->mbtn[1], wv);
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
		if(wtable->suppress_redraw == 0)
			for(i = 0; i < wtable->npanels; i++) {
				WavePanel *wp = wtable->panels[i];
				draw_wavepanel(wp->drawing, NULL, wp);
			}
	}
}

XSCM_DEFINE(wavepanel_add_variable_x, "wavepanel-add-variable!", 2, 0, 0,
	   (SCM wavepanel, SCM var),
	   "Add variable VAR to the display in WAVEPANEL.")
#define FUNC_NAME s_wavepanel_add_variable_x
{
	WavePanel *wp;
	WaveVar *wv;
	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);
	VALIDATE_ARG_WaveVar_COPY(1,var,wv);

	if(wv)
		return add_var_to_panel(wp, wv);
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME

/*
 * Add a new waveform to a WavePanel, creating a new VisibleWave.
 * If no wavepanel is specified, try to use the "current" wavepanel,
 * defined for now to be the last one where a signal was dropped.
 * This is the only place that VisibleWave structures are created.
 */
SCM
add_var_to_panel(WavePanel *wp, WaveVar *dv)
{
	VisibleWave *vw;

	if(wp == NULL) {
		if(last_drop_wavepanel)
			wp = last_drop_wavepanel;
		else return;
	} else {
		last_drop_wavepanel = wp;
	}

	vw = g_new0(VisibleWave, 1);
	vw->wp = wp;
	vw->var = dv;
	vw->varname = g_strdup(dv->wv_name);
	vw->gdf = (GWDataFile *)dv->udata;
	vw->colorn = wp->nextcolor;
	wp->nextcolor = (wp->nextcolor + 1)%NWColors;

	wp->vwlist = g_list_append(wp->vwlist, vw);
	wavepanel_update_data(wp);
	wavetable_update_data();

	vw->valid = 1;
	SGT_NEWCELL_SMOB(vw->smob, VisibleWave, vw);
	scm_protect_object(vw->smob);
	vw->outstanding_smob = 1;

	if(wp->lmtable)  /* add button to Y-label box */
		vw_wp_create_button(vw, wp);
	call1_hooks(new_visiblewave_hook, vw->smob);
	if(wp->drawing && (wtable->suppress_redraw == 0)) {
		/* redraw whole panel.
		 * Perhaps this is too much extra work, but it seems fast
		 * enough.
		 * at the very least, we'd have to undraw any cursors
		 * before drawing just the single new waveform
		 */
		draw_wavepanel(wp->drawing, NULL, wp);
	}
	return vw->smob;
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
	wp->max_xval = -G_MAXDOUBLE;
	wp->min_yval = G_MAXDOUBLE;
	wp->max_yval = -G_MAXDOUBLE;
	g_list_foreach(wp->vwlist, vw_wp_visit_update_data, (gpointer)wp);

	/* set to something reasonable if they didn't change,
	 * like if the panel was empty
	 */
	if(wp->min_xval == G_MAXDOUBLE)
		wp->min_xval = 0; /* wtable->min_xval; */
	if(wp->max_xval == -G_MAXDOUBLE)
		wp->max_xval = 0; /* wtable->max_xval; */
	if(wp->min_yval == G_MAXDOUBLE)
		wp->min_yval = 0.0;
	if(wp->max_yval == -G_MAXDOUBLE)
		wp->max_yval = 1.0;

	if(wp->man_yzoom == 0) {
		wp->start_yval = wp->min_yval;
		wp->end_yval = wp->max_yval;
	}

	/* zero height? set to +- 0.1%  so a line is visible in the center */
	if((wp->end_yval - wp->start_yval) < DBL_EPSILON) {
		wp->end_yval *= 1.001;
		wp->start_yval *= 0.999;
		/* still zero?  maybe there's a waveform that is stuck at 0.000 */
		if((wp->end_yval - wp->start_yval) < DBL_EPSILON) {
			wp->end_yval += 1e-6;
			wp->start_yval -= 1e-6;
		}
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
	draw_wavepanel_labels(wp);
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
	wtable->max_xval = -G_MAXDOUBLE;
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
	if(wtable->max_xval == -G_MAXDOUBLE)
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
		cmd_zoom_absolute( wtable->start_xval, wtable->end_xval );

	}
	/* propagate zoom to panels.  Someday: flags and UI to allow
	 * "locking" selected panels so they don't zoom/scroll */
	for(i = 0; i < wtable->npanels; i++) {
		wp = wtable->panels[i];
		wp->start_xval = wtable->start_xval;
		wp->end_xval = wtable->end_xval;
	}
}

/* access routines for VisibleWave */

XSCM_DEFINE(visiblewave_on_top_x, "visiblewave-on-top!", 1, 0, 0, (SCM vw),
	   "Make VisibleWave VW the topmost one drawn in its WavePanel")
#define FUNC_NAME s_visiblewave_on_top_x
{
	VisibleWave *cvw;
	WavePanel *wp;
	int old_row;
	VALIDATE_ARG_VisibleWave_COPY(1,vw,cvw);
	wp = cvw->wp;
	/* remove from the middle of the list and add to the end */
	wp->vwlist = g_list_remove(wp->vwlist, (gpointer)cvw);
	wp->vwlist = g_list_append(wp->vwlist, (gpointer)cvw);
	if(wp->drawing && (wtable->suppress_redraw == 0)) {
		draw_wavepanel(wp->drawing, NULL, wp);
	}
	/* move label & measurements to the top of the table, if not already there */
	old_row = gtk_table_get_child_row(wp->lmtable, cvw->button);
	printf("visible-wave-on-top moving from row %d\n", old_row);
	if(old_row > 0) {
		gtk_table_rotate_rows(wp->lmtable, old_row, 0);
	}

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

XSCM_DEFINE(visiblewave_delete_x, "visiblewave-delete!", 1, 0, 0, (SCM vw),
	   "Delete VisibleWave VW from its WavePanel")
#define FUNC_NAME s_visiblewave_delete_x
{
	VisibleWave *cvw;
	VALIDATE_ARG_VisibleWave_COPY(1,vw,cvw);
	if(v_flag)
		fprintf(stderr, "visiblewave_delete(0x%x)\n", cvw);
	visiblewave_delete(cvw);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

XSCM_DEFINE(visiblewave_file, "visiblewave-file", 1, 0, 0, (SCM vw),
	   "Given a VisibleWave VW, return the DataFile the waveform comes from")
#define FUNC_NAME s_visiblewave_file
{
	VisibleWave *cvw;
	VALIDATE_ARG_VisibleWave_COPY(1,vw,cvw);
	if(v_flag)
		fprintf(stderr, "visiblewave_file(0x%x) -> 0x%x \n",
			cvw, cvw->gdf->smob);
	if(!cvw->valid) 
		return SCM_BOOL_F;
	else {
		cvw->gdf->outstanding_smob = 1;
		return cvw->gdf->smob;
	}
}
#undef FUNC_NAME

XSCM_DEFINE(visiblewave_varname, "visiblewave-varname", 1, 0, 0, (SCM vw),
	   "Given a VisibleWave VW, return wave's variable name as found in the original data file")
#define FUNC_NAME s_visiblewave_varname
{
	VisibleWave *cvw;
	VALIDATE_ARG_VisibleWave_COPY(1,vw,cvw);

	if(!cvw->valid) 
		return SCM_BOOL_F;
	else
		return gh_str02scm(cvw->varname);

}
#undef FUNC_NAME

XSCM_DEFINE(visiblewave_panel, "visiblewave-panel", 1, 0, 0, (SCM vw),
	   "Given a VisibleWave VW, return the WavePanel the waveform is displayed in")
#define FUNC_NAME s_visiblewave_panel
{
	VisibleWave *cvw;
	VALIDATE_ARG_VisibleWave_COPY(1,vw,cvw);

	if(!cvw->valid) 
		return SCM_BOOL_F;
	else {
		cvw->wp->outstanding_smob = 1;
		return cvw->wp->smob;
	}
}
#undef FUNC_NAME

XSCM_DEFINE(visiblewave_button, "visiblewave-button", 1, 0, 0, (SCM vw),
"Given a VisibleWave VW, return the Gtk button associated with the
wave.  Since the button already has a label, all you can do is
add events to the button.")
#define FUNC_NAME s_visiblewave_button
{
	VisibleWave *cvw;
	VALIDATE_ARG_VisibleWave_COPY(1,vw,cvw);

	if(!cvw->valid || !cvw->button) 
		return SCM_BOOL_F;
	else {
		return sgtk_wrap_gtkobj(GTK_OBJECT(cvw->button));
	}
}
#undef FUNC_NAME

XSCM_DEFINE(visiblewave_color, "visiblewave-color", 1, 0, 0, (SCM vw),
	   "Given a VisibleWave VW, return color number it is currently drawn with")
#define FUNC_NAME s_visiblewave_color
{
	VisibleWave *cvw;
	VALIDATE_ARG_VisibleWave_COPY(1,vw,cvw);

	if(!cvw->valid)
		return SCM_BOOL_F;
	else
		return gh_int2scm(cvw->colorn);
}
#undef FUNC_NAME

XSCM_DEFINE(wavevar_min, "wavevar-min", 1, 0, 0, (SCM wv),
	   "Given a VisibleWave or WaveVar VW, return its minimum value")
#define FUNC_NAME s_wavevar_min
{
	VisibleWave *cvw;
	VALIDATE_ARG_VisibleWave_COPY(1,wv,cvw);
	/* TODO: smobify WaveVar, and let this function work on both,
	 as a sort of fake inheritance */
	if(!cvw->valid)
		return SCM_BOOL_F;
	else
		return gh_double2scm(cvw->var->wds->min);
}
#undef FUNC_NAME

XSCM_DEFINE(wavevar_max, "wavevar-max", 1, 0, 0, (SCM wv),
	   "Given a VisibleWave or WaveVar VW, return its minimum value")
#define FUNC_NAME s_wavevar_max
{
	VisibleWave *cvw;
	VALIDATE_ARG_VisibleWave_COPY(1,wv,cvw);

	if(!cvw->valid)
		return SCM_BOOL_F;
	else
		return gh_double2scm(cvw->var->wds->max);
}
#undef FUNC_NAME

XSCM_DEFINE(set_visiblewave_color_x, "set-visiblewave-color!", 2, 0, 0, (SCM vw, SCM num),
	 "Change VW so that it is drawn with color NUM")
#define FUNC_NAME s_set_visiblewave_color_x
{
	VisibleWave *cvw;
	int colorn;
	VALIDATE_ARG_VisibleWave_COPY(1,vw,cvw);
	VALIDATE_ARG_INT_RANGE_COPY(2,num, 0, NWColors, colorn);

	if(cvw->valid) {
		GtkStyle *newstyle;
		char lbuf[64];
		cvw->colorn = colorn;
		sprintf(lbuf, "wavecolor%d", cvw->colorn);
		gtk_widget_set_name(cvw->label, lbuf);
		
		newstyle = gtk_rc_get_style(cvw->label);
		gtk_widget_set_style(cvw->label, newstyle);
		if(cvw->gc) {  
			gdk_gc_set_foreground(cvw->gc,
			      &cvw->label->style->fg[GTK_STATE_NORMAL]);
			draw_wavepanel(cvw->wp->drawing, NULL, cvw->wp);
		}
	}
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

XSCM_DEFINE(set_visiblewave_measure_x, "set-visiblewave-measure!", 3, 0, 0, 
	   (SCM vw, SCM n, SCM func),
 "Change the measurement box numbered N (0 or 1) of displayed waveform
 VW to display the result of the measurement function FUNC")
#define FUNC_NAME s_set_visiblewave_measure_x
{
	VisibleWave *cvw;
	int mno;
	int mfunc;
	VALIDATE_ARG_VisibleWave_COPY(1,vw,cvw);
	VALIDATE_ARG_INT_RANGE_COPY(2, n, 0, 1, mno);
	VALIDATE_ARG_INT_RANGE_COPY(3, func, 0, MBF_VARDIFF, mfunc);

	if(cvw->valid) {
		mbtn_set_func(cvw->mbtn[mno], mfunc);
		mbtn_update(cvw->mbtn[mno], NULL);
	}
}
#undef FUNC_NAME


/* guile initialization */
void init_cmd()
{
#ifndef XSCM_MAGIC_SNARF_INITS
#include "cmd.x"
#endif
}
