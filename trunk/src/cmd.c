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

/* For now: zooms are always done about the center.
 * 	adjust start and end X values for waveform graph, 
 *	adjust scrollbar percentage.
 *
 * To do: replace cmd_zoom_in and cmd_zoom_out with a single function
 * cmd_zoom which takes a value to zoom by, <1 for in, >1 for out.
 */
gint cmd_zoom_in(GtkWidget *widget)
{
	double ocenter, owidth;
	ocenter = (wtable->start_xval + wtable->end_xval)/2;
	owidth = wtable->end_xval - wtable->start_xval;

	wtable->start_xval = ocenter - owidth/4;
	wtable->end_xval = ocenter + owidth/4;

	win_hsadj->value = wtable->start_xval;
	win_hsadj->page_size = fabs(wtable->end_xval - wtable->start_xval);
	win_hsadj->page_increment = win_hsadj->page_size/2;
	win_hsadj->step_increment = win_hsadj->page_size/100;

	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}

gint cmd_zoom_out(GtkWidget *widget)
{
	double ocenter, owidth;
	ocenter = (wtable->start_xval + wtable->end_xval)/2;
	owidth = wtable->end_xval - wtable->start_xval;

	wtable->start_xval = ocenter - owidth;
	if(wtable->start_xval < wtable->min_xval)
		wtable->start_xval = wtable->min_xval;
	wtable->end_xval = ocenter + owidth;
	if(wtable->end_xval > wtable->max_xval)
		wtable->end_xval = wtable->max_xval;

	win_hsadj->page_size = fabs(wtable->end_xval - wtable->start_xval);
	win_hsadj->page_increment = win_hsadj->page_size/2;
	win_hsadj->step_increment = win_hsadj->page_size/100;
	win_hsadj->value = wtable->start_xval;

	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}

gint cmd_zoom_full(GtkWidget *widget)
{
	wtable->start_xval = wtable->min_xval;
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

static void
vw_wp_delete_if_selected(gpointer p, gpointer d)
{
	VisibleWave *vw = (VisibleWave *)p;
	WavePanel *wp = (WavePanel *)d;
	GtkToggleButton *btn = GTK_TOGGLE_BUTTON(vw->button);
	
	if(btn->active) {
		remove_wave_from_panel(wp, vw);
	}
}

/*
 * cmd_delete_selected_waves
 */
gint cmd_delete_selected_waves(GtkWidget *widget)
{
	int i;
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = &wtable->panels[i];
		g_list_foreach(wp->vwlist, vw_wp_delete_if_selected, wp);
	}

	/* BUG: only first selected wave gets deleted,
	 * because removing elements from the list 
	 * during a g_list_foreach is a no-no.
	 */
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

/* BUG: things appear to work OK, but I get gtk runtime error messages when
   removing/destroying widgets.  Somthing subtle must be wrong here.
*/
	gtk_container_remove(GTK_CONTAINER(wp->lvbox), vw->button);
/*	gtk_widget_destroy(vw->button); */

	gdk_gc_destroy(vw->gc);
	g_free(vw);

	wavepanel_update_data(wp);
	wavetable_update_data();
}

/*
 * Add a waveform to a WavePanel
 */
void
add_var_to_panel(WavePanel *wp, DVar *dv)
{
	VisibleWave *vw;

	vw = g_new0(VisibleWave, 1);
	vw->var = dv;
	vw->colorn = wp->nextcolor;
	wp->nextcolor = (wp->nextcolor + 1)%NWColors;

	wp->vwlist = g_list_append(wp->vwlist, vw);
	wavepanel_update_data(wp);
	wavetable_update_data();

	if(wp->lvbox)  /* add button to Y-label box */
		vw_wp_create_button(vw, wp);
	if(wp->drawing) {
		vw_wp_setup_gc(vw, wp);
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

	if(vw->var->iv->d.min < wp->min_xval)
		wp->min_xval = vw->var->iv->d.min;
	if(vw->var->iv->d.max > wp->max_xval)
		wp->max_xval = vw->var->iv->d.max;

	if(vw->var->d.min < wp->min_yval)
		wp->min_yval = vw->var->d.min;
	if(vw->var->d.max > wp->max_yval)
		wp->max_yval = vw->var->d.max;
}

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
		wp->min_xval = wtable->min_xval;
	if(wp->max_xval == G_MINDOUBLE)
		wp->max_xval = wtable->max_xval;
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
	wtable->min_xval = G_MAXDOUBLE;
	wtable->max_xval = G_MINDOUBLE;
	for(i = 0; i < wtable->npanels; i++) {
		wp = &wtable->panels[i];
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

	/* if start & end were the same, try updating them so we can
	* see somthing.
	*/
	if(fabs(wtable->end_xval - wtable->start_xval) < DBL_EPSILON &&
		win_hsadj != NULL) {
		cmd_zoom_full(NULL);
	}
}

