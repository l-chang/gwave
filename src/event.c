/*
 * event.c, part of the gwave waveform viewer tool
 * Functions for handling low-level events (clicks, drag-and-drop)
 * Some drawing things are here if they are related to mouse operations;
 * perhaps they should move.
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

#include "gwave.h"


void destroy_handler(GtkWidget *widget, gpointer data)
{
	gtk_main_quit();
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

void
set_all_wp_cursors(int cnum)
{
	GdkCursor *cursor;
	int i;
	WavePanel *wp;

	if(cnum == -1)
		cursor = NULL;
	else
		cursor = gdk_cursor_new(cnum);
	for(i = 0; i < wtable->npanels; i++) {
		wp = wtable->panels[i];
		gdk_window_set_cursor(wp->drawing->window, cursor);
	}
	if(cursor)
		gdk_cursor_destroy(cursor);
}

/*
 * The next several routines implement the generic operation of 
 * selecting a subset of the visible part of the X axis by dragging
 * with button 1.
 */
void
select_x_range(SRFunc func, gpointer data)
{
	wtable->srange->done_callback = func;
	wtable->srange->done_data = data;
	set_all_wp_cursors(GDK_RIGHT_SIDE);
	wtable->mstate = M_SELRANGE_ARMED;
}

/* draw or undraw srange line, using XOR gc */
void
draw_srange(SelRange *sr)
{
	if(!sr->gc) {
		gdk_color_alloc(win_colormap, &sr->gdk_color);
		sr->gc = gdk_gc_new(sr->wp->drawing->window);
		gdk_gc_set_foreground(sr->gc, &sr->gdk_color);
		gdk_gc_set_background(sr->gc, &bg_gdk_color);
		gdk_gc_set_function(sr->gc, GDK_XOR);
	}
	g_assert(sr->gc != NULL);
	gdk_draw_line(sr->wp->drawing->window, sr->gc,
		      sr->x1, sr->y, sr->x2, sr->y);
}

void
update_srange(SelRange *sr, int newx2, int draw)
{
	if(sr->drawn)	/* undraw old */
		draw_srange(sr);
	sr->drawn = draw;
	sr->x2 = newx2;
	if(draw) {	/* draw new if requested */
		draw_srange(sr);
	}
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
		wp = wtable->panels[i];
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
/*	gtk_container_disable_resize(GTK_CONTAINER(win_main)); */
	if(csp == wtable->cursor[0]) {
		for(i = 0; i < wtable->npanels; i++) {
			wp = wtable->panels[i];
			g_list_foreach(wp->vwlist, vw_wp_visit_update_labels, wp);
		}
	}

	/* update status label */
	lbuf[0] = 0;
	if(wtable->cursor[0]->shown) {
		sprintf(abuf, "cursor1: %s", 
			val2txt(wtable->cursor[0]->xval, 0));
		strcat(lbuf, abuf);
	}
	if(wtable->cursor[1]->shown) {
		sprintf(abuf, " cursor2: %s", 
			val2txt(wtable->cursor[1]->xval, 0));
		strcat(lbuf, abuf);
	}
	if(wtable->cursor[0]->shown && wtable->cursor[1]->shown) {
		sprintf(abuf, " delta: %s", 
		 val2txt(wtable->cursor[1]->xval - wtable->cursor[0]->xval,0));
		strcat(lbuf, abuf);
	}

	gtk_label_set(GTK_LABEL(win_status_label), lbuf);
/*	gtk_container_enable_resize(GTK_CONTAINER(win_main)); */
}

static void
window_update_cursor(WavePanel *wp, VBCursor *csp, int x)
{
	double xval;
	g_assert(csp != NULL);

	xval = x2val(wp, x);
	if(fabs(xval - csp->xval) < DBL_EPSILON && csp->shown)
		return;
	if(xval < wp->start_xval || xval > wp->end_xval)
		return;
	update_cursor(csp, xval);
}

/*
 * button_press in any wave panel.
 */
gint
button_press_handler(GtkWidget *widget, GdkEventButton *event, 
			  gpointer data)
{
	WavePanel *wp = (WavePanel *)data;
	GdkCursor *cursor;

	switch(event->button) {
	case 1:
	case 2:
		switch(wtable->mstate) {
		case M_NONE:
			gtk_grab_add(widget);
			wtable->mstate = M_CURSOR_DRAG;
			wtable->button_down = event->button;

			cursor = gdk_cursor_new(GDK_SB_H_DOUBLE_ARROW);
			gdk_window_set_cursor(widget->window, cursor);
			gdk_cursor_destroy(cursor);
			wtable->drag_cursor = wtable->cursor[event->button-1];
			window_update_cursor(wp, wtable->drag_cursor, event->x);
			break;
		case M_SELRANGE_ARMED:
			gtk_grab_add(widget);
			wtable->button_down = event->button;
			wtable->mstate = M_SELRANGE_ACTIVE;

			wtable->srange->y = event->y;
			wtable->srange->x1 = event->x;
			wtable->srange->wp = wp;
			break;
		/* can't start another drag until first one done */
		case M_CURSOR_DRAG:
		case M_SELRANGE_ACTIVE:
			break;
		default:
			break;
			
		}
		break;
	case 3:
		if(wtable->mstate == M_NONE) {
			wtable->popup_panel = wp;
			gtk_menu_popup (GTK_MENU (wtable->popup_menu),
					NULL, NULL, NULL, NULL, 
					event->button, event->time);
		}
		break;
	default:
		break;
	}
/*	fprintf(stderr, "P%d;mstate=%d\n", event->button, wtable->mstate); */
	return 0;
}

/*
 * button_release in any wave panel.
 */
gint
button_release_handler(GtkWidget *widget, GdkEventButton *event, 
			  gpointer data)
{
	WavePanel *wp = (WavePanel *)data;

	if(wtable->button_down != event->button)
		return 0;

	switch(wtable->mstate) {
	case M_CURSOR_DRAG:
		gtk_grab_remove(widget);
		gdk_window_set_cursor(widget->window, NULL);
		window_update_cursor(wp, wtable->drag_cursor, event->x);
		wtable->drag_cursor = NULL;
		break;
	case M_SELRANGE_ACTIVE:
		gtk_grab_remove(widget);
		set_all_wp_cursors(-1);
		update_srange(wtable->srange, event->x, 0);
		(wtable->srange->done_callback)(wtable->srange->wp,
						wtable->srange->x1, 
						wtable->srange->x2, 
						wtable->srange->done_data);
		break;
	default:
	}
	wtable->mstate = M_NONE;
	wtable->button_down = -1;
/*	fprintf(stderr, "R%d;mstate=%d\n", event->button, wtable->mstate); */
	return 0;
}

/*
 * GDK_MOTION_NOTIFY in any WavePanel's drawing area
 */
gint
motion_handler(GtkWidget *widget, GdkEventMotion *event, 
			  gpointer data)
{
	WavePanel *wp = (WavePanel *)data;
	VBCursor *csp;

	switch(wtable->mstate) {
	case M_CURSOR_DRAG:
		csp = wtable->drag_cursor;
		window_update_cursor(wp, csp, event->x);
		break;
	case M_SELRANGE_ACTIVE:
		/* fputc('r', stderr); */
		update_srange(wtable->srange, event->x, 1);
		break;
	default:
		fputc('.', stderr);
		break;
	}
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

	if(wtable->suppress_redraw == 0)
		for(i = 0; i < wtable->npanels; i++) {
			wp = wtable->panels[i];
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

	/* Make sure we've got GCs for each visible wave. */
/*	g_list_foreach(wp->vwlist, (GFunc)vw_wp_setup_gc, wp); */

	if ( wp->pixmap && (wp->width != w || wp->height != h)) {
		gdk_pixmap_unref(wp->pixmap);
		wp->width = w;
		wp->height = h;
		wp->pixmap = NULL;

	}
	if(!wp->pixmap)
		wp->pixmap = gdk_pixmap_new(widget->window, w, h, -1);

	if(wtable->suppress_redraw == 0)
		draw_wavepanel(wp->drawing, event, wp);

	return 0;
}
