/*
 * event.c, part of the gwave waveform viewer tool
 * Functions for handling low-level events (clicks, drag-and-drop)
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
 * Revision 1.1  1998/09/01 21:28:20  tell
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


void destroy_handler(GtkWidget *widget, gpointer data)
{
	gtk_main_quit();
}

/*
 * event handler called when variable gets dropped on a wavepanel
 */
void
wavepanel_dnd_drop (GtkWidget *button, GdkEvent *event, gpointer d)
{
	WavePanel *wp = (WavePanel *)d;
	GWDnDData dd;

	dd = *(GWDnDData *)(event->dropdataavailable.data);

/*	printf("Drop data of type %s was: 0x%lx\n",
	  event->dropdataavailable.data_type, dvar); */

	add_var_to_panel(wp, dd.dv);
}

/* vw_wp_visit_update_labels -- called from g_list_foreach to update the
 * waveform-value labels for a VisibleWave
 */
void 
vw_wp_visit_update_labels(gpointer p, gpointer d)
{
	VisibleWave *vw = (VisibleWave *)p;
	char lbuf[64];

	vw_get_label_string(lbuf, 64, vw);
	gtk_label_set(GTK_LABEL(vw->label), lbuf);
}

/*
 * draw (or undraw) cursor.
 */
static void
draw_cursor(VBCursor *csp)
{
	int h, x, i;
	WavePanel *wp;
	for(i = 0; i < wtable->npanels; i++) {
		wp = &wtable->panels[i];
		h = wp->drawing->allocation.height;
		if(wp->start_xval <= csp->xval 
		   && csp->xval <= wp->end_xval) {
			x = val2x(wp, csp->xval);
			gdk_draw_line(wp->drawing->window, csp->gdk_gc,
			      x, 0, x, h);
		}
	}
}

/*
 * move cursor at specified location;
 * turn it on if not on already.
 */
static void 
update_cursor(VBCursor *csp, double xval)
{
	WavePanel *wp;
	int i;
	char abuf[128];
	char lbuf[128];

	/* undraw old cursor */
	if(csp->shown) {
		draw_cursor(csp);
	}

	csp->xval = xval;
	csp->shown = 1;
	/* draw cursor in each panel */
	draw_cursor(csp);

	/* update name/value label */
	gtk_container_disable_resize(GTK_CONTAINER(win_main));
	if(csp == wtable->cursor[0]) {
		for(i = 0; i < wtable->npanels; i++) {
			wp = &wtable->panels[i];
			g_list_foreach(wp->vwlist, vw_wp_visit_update_labels, wp);
		}
	}

	/* update status label */
	lbuf[0] = 0;
	if(wtable->cursor[0]->shown) {
		sprintf(abuf, "cursor1: %s", val2txt(wtable->cursor[0]->xval));
		strcat(lbuf, abuf);
	}
	if(wtable->cursor[1]->shown) {
		sprintf(abuf, " cursor2: %s", val2txt(wtable->cursor[1]->xval));
		strcat(lbuf, abuf);
	}
	if(wtable->cursor[0]->shown && wtable->cursor[1]->shown) {
		sprintf(abuf, " delta: %s", val2txt(wtable->cursor[1]->xval - wtable->cursor[0]->xval));
		strcat(lbuf, abuf);
	}

	gtk_label_set(GTK_LABEL(win_status_label), lbuf);
	gtk_container_enable_resize(GTK_CONTAINER(win_main));
}

/*
 * TODO: implement dragging cursors around instead of this 
 * simple click-to-place stuff.
 */
gint click_handler(GtkWidget *widget, GdkEventButton *event, 
			  gpointer data)
{
	double xval;
	WavePanel *wp = (WavePanel *)data;
	VBCursor *csp;

	switch(event->button) {
	case 1:
		csp = wtable->cursor[0];
		break;
	case 2:
		csp = wtable->cursor[1];
		break;
	default:
		return 0;
	}

	xval = x2val(wp, event->x);
	if(fabs(xval - csp->xval) < DBL_EPSILON && csp->shown)
		return 0;	/* clicked in same place */

	update_cursor(csp, xval);
	return 0;
}

gint scroll_handler(GtkWidget *widget)
{
	GtkAdjustment *hsadj = GTK_ADJUSTMENT(widget);
	double owidth;
	int i;
	WavePanel *wp;

	owidth = wtable->end_xval - wtable->start_xval;

	wtable->start_xval = hsadj->value;
	wtable->end_xval = hsadj->value + owidth;

	draw_labels();
	for(i = 0; i < wtable->npanels; i++) {
		wp = &wtable->panels[i];
		wp->start_xval = wtable->start_xval;
		wp->end_xval = wtable->end_xval;
		draw_wavepanel(wp->drawing, NULL, wp);
	}
	return 0;
}


/* Get the foreground color for the waveform and set up its GC
 * by using the GdkColor of the corresponding label.
 */
void vw_wp_setup_gc(VisibleWave *vw, WavePanel *wp)
{
	if(!vw->gc) {
		gdk_color_alloc(win_colormap, &vw->label->style->fg[GTK_STATE_NORMAL]);
		vw->gc = gdk_gc_new(wp->drawing->window);
		gdk_gc_set_foreground(vw->gc, &vw->label->style->fg[GTK_STATE_NORMAL]);
	}
}

/*
 * expose_handler - first time around, do last-minute setup.
 * otherwise, arranges to get waveform panel drawing areas redrawn.
 * Redraw stuff needs an overhaul to make more efficient.
 */
gint expose_handler(GtkWidget *widget, GdkEventExpose *event,
			   WavePanel *wp)
{
	int w = widget->allocation.width;
	int h = widget->allocation.height;

	if(!colors_initialized) {
		alloc_colors(widget);
		colors_initialized = 1;
	}
	/*
	 * Make sure we've got GCs for each visible wave.
	 */
	g_list_foreach(wp->vwlist, (GFunc)vw_wp_setup_gc, wp);

	if ( wp->pixmap ) {
		/* CHECK/FIX: only need to free/new if size changed? */
		gdk_pixmap_unref(wp->pixmap);
	}
	wp->pixmap = gdk_pixmap_new(widget->window, w, h, -1);

	/* since the scrollbar event also redraws everything, 
	 * let its handler do all the work by sending it the signal
	 * bonus: gets the scrollbar updated if this event is due to a resize.
	 */
	/* now that we have many panels, this might cause multiple 
	 *  unnecessary redraws.  Try to figure out if this is the case,
	 *  and how to fix it. 
	 */
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}
