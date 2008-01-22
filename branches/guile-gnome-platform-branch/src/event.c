/*
 * event.c, part of the gwave waveform viewer tool
 * Functions for handling low-level events (clicks, drag-and-drop)
 * Some drawing things are here if they are related to mouse operations;
 * perhaps they should move.
 *
 * Copyright (C) 1998-2002 Stephen G. Tell
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
 * Revision 1.21  2005/11/23 02:38:06  sgt
 * fix marking of WaveVar smobs
 * code cleanup: move several widgets from globals into WaveTable structure
 *
 * Revision 1.20  2005/10/08 05:48:58  sgt
 * get rid of most uses of the deprecated gh_ interfaces to guile
 *
 * Revision 1.19  2004/12/26 23:49:32  sgt
 * add notion of "selected" panels.  left-click in panel (including cursor0-move)
 * selects, shift-click selects without unselecting others.
 * double-click in wavelist adds to first selected panel.
 * selected panels have a highlighted outline.
 * add ability to change the "tag" (shorthand identifier) for a file
 * added example of using this to put case name into the tag to
 * experimental .gwaverc
 *
 * Revision 1.18  2003/12/03 03:58:16  sgt
 * clean up string literals containing newlines, all in documentation
 * strings.
 * Modify xsnarf.h to get document-extraction working again
 *
 * Revision 1.17  2002/03/28 06:35:21  sgt
 * Snarfing overhaul to make it guile-version-independent
 * now compiles and runs under guile-1.5.6
 * some issues remain - bound? in cmds.scm for example.
 *
 * Revision 1.16  2002/01/10 04:04:26  sgt
 * Get Y and XY zoom working, along with zoom dialog box
 * start attempting to get things building with guile-1.5.4, but not done yet.
 *
 * Revision 1.15  2001/12/24 04:10:09  sgt
 * Add primitives and GUI menu items for Y-zoom and XY-area zoom
 * add save/restore of Y-zoom state of wavepanels
 *
 * Revision 1.14  2001/09/22 04:34:58  sgt
 * merge Paul Maurer's patches for better zooming and scrolling in log-xscale
 * mode.
 * Write AC_PROG_GREPSTDOUT and use it to find GNU graph and not the old
 * att/berkeley one
 *
 * Revision 1.13  2001/03/20 06:36:45  sgt
 * Change wavepanel measurements to use new MeasureBtn code.
 *
 * Revision 1.12  2001/03/20 05:53:19  sgt
 * create "measure button" abstraction, and use it for cursor values
 *
 * Revision 1.11  2001/03/13 06:49:51  sgt
 * Change label area on the left side of a panel to use a table,
 * and display two measurements.  measurements are currently the two
 * cursor values.
 *
 * Revision 1.10  2000/11/08 07:41:29  sgt
 * wavewin.c - add guile bindings for set-wtable-vcursor! and wtable-xlogscale?
 * gwave.c - change to handcrafted option processing since getopt prints
 * error messages for the scheme-handled options it doesn't know about.
 *
 * Revision 1.9  2000/08/08 06:41:23  sgt
 * Convert to guile-1.4 style SCM_DEFINE macros, where the docstrings
 * are strings, not comments.  Remove some unused functions.
 * Other guile-1.4 compatibility.  Not tested with earlier guile yet.
 *
 * Revision 1.8  2000/04/27 06:14:38  tell
 * Implement logarithmic axis scaling.
 * Working well for Y, needs fixing for X.
 *
 * Revision 1.7  2000/01/07 06:33:43  tell
 * Merged in the guile and guile-gtk stuff
 *
 * Revision 1.5  1999/09/22 17:29:00  tell
 * add drag&drop support for Gtk+ 1.2
 *
 * Revision 1.4  1999/01/08 22:41:41  tell
 * button-3 popup menu in wavepanel windows
 *
 * Revision 1.1  1998/12/26 04:38:58  tell
 * Initial revision
 *
 * Revision 1.3  1998/09/30 21:58:15  tell
 * Reorganization of mouse-button event handling to support both dragging of
 * cursors and select_x_range primitive used to get the range for cmd_zoom_window.
 * add supress_redraw and some misc stuff while tracking down some bugs.
 * Attempt to make drawing the waveforms a bit more efficient.
 *
 * Revision 1.2  1998/09/17 18:35:49  tell
 * wrap DnD message as type GWDnDData.
 * Split cursor draw/update into several functions in preparation for
 * support of dragging.
 *
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

#include <scwm_guile.h>
#include <gwave.h>
#include <wavewin.h>
#include <guile-gtk.h>

extern sgtk_boxed_info sgtk_gdk_event_info;

void destroy_handler(GtkWidget *widget, gpointer data)
{
	gtk_main_quit();
}

/* 
 * Set the X pointer cursor for all wavepanels: used to provide a
 * hint that we're expecting the user to drag out a line or region.
 */
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
XSCM_DEFINE(select_range_x, "select-range-x", 1, 0, 0,
           (SCM proc),
"Prompt the user to select a range of the visible X axis using"
"button 1 of the mouse.  "
"When finished, PROC is called with 3 arguments, the"
"WavePanel where the range is located, and the"
"begining and ending X pixel value of the selection.")
#define FUNC_NAME s_select_range_x
{
	VALIDATE_ARG_PROC(1, proc);
	
	scm_gc_protect_object(proc);
	wtable->srange->done_proc = proc;
	wtable->srange->type = SR_X;

	set_all_wp_cursors(GDK_RIGHT_SIDE);
	wtable->mstate = M_SELRANGE_ARMED;
}
#undef FUNC_NAME

XSCM_DEFINE(select_range_y, "select-range-y", 1, 0, 0,
           (SCM proc),
"Prompt the user to select a range of the visible Y axis using"
"button 1 of the mouse. " 
"When finished, PROC is called with 3 arguments, the"
"WavePanel where the range is located, and the"
"begining and ending Y pixel value of the selection.")
#define FUNC_NAME s_select_range_y
{
	VALIDATE_ARG_PROC(1, proc);
	
	scm_gc_protect_object(proc);
	wtable->srange->done_proc = proc;
	wtable->srange->type = SR_Y;

	set_all_wp_cursors(GDK_TOP_SIDE);
	wtable->mstate = M_SELRANGE_ARMED;
}
#undef FUNC_NAME

XSCM_DEFINE(select_range_xy, "select-range-xy", 1, 0, 0,
           (SCM proc),
"Prompt the user to select a region of the "
"visible XY plane using button 1 of the mouse.  "
"When finished, PROC is called with 5 arguments, the"
"WavePanel where the range is located, and the"
"begining and ending X and Y pixel values of the selection.")
#define FUNC_NAME s_select_range_xy
{
	VALIDATE_ARG_PROC(1, proc);
	
	scm_gc_protect_object(proc);
	wtable->srange->done_proc = proc;
	wtable->srange->type = SR_XY;

	set_all_wp_cursors(GDK_TOP_LEFT_CORNER);
	wtable->mstate = M_SELRANGE_ARMED;
}
#undef FUNC_NAME

/* done selecting range; do the callback */
void
callback_srange()
{
	if(wtable->srange->wp->valid 
	   && SCM_NFALSEP(scm_procedure_p(wtable->srange->done_proc))) {
		wtable->srange->wp->outstanding_smob = 1;

		switch(wtable->srange->type) {
		case SR_X:
			scwm_safe_call3(wtable->srange->done_proc, 
				wtable->srange->wp->smob,
				scm_long2num(wtable->srange->x1), 
				scm_long2num(wtable->srange->x2));
			break;
		case SR_Y:
			scwm_safe_call3(wtable->srange->done_proc, 
				wtable->srange->wp->smob,
				scm_long2num(wtable->srange->y1), 
				scm_long2num(wtable->srange->y2));
			break;
		case SR_XY:
			scwm_safe_call5(wtable->srange->done_proc, 
				wtable->srange->wp->smob,
				scm_long2num(wtable->srange->x1), 
				scm_long2num(wtable->srange->x2),
				scm_long2num(wtable->srange->y1), 
				scm_long2num(wtable->srange->y2));
			break;
		}
		scm_gc_unprotect_object(wtable->srange->done_proc);
	}
}

/* draw or undraw srange line(s), using XOR gc */
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
	if(sr->type & SR_X)
		gdk_draw_line(sr->wp->drawing->window, sr->gc,
			      sr->x1, sr->y1, sr->x2, sr->y1);
	if(sr->type & SR_Y)
		gdk_draw_line(sr->wp->drawing->window, sr->gc,
			      sr->x1, sr->y1, sr->x1, sr->y2);
	if(sr->type == SR_XY) {
		gdk_draw_line(sr->wp->drawing->window, sr->gc,
			      sr->x1, sr->y2, sr->x2, sr->y2);
		gdk_draw_line(sr->wp->drawing->window, sr->gc,
			      sr->x2, sr->y1, sr->x2, sr->y2);
	}
}

void
update_srange(SelRange *sr,  GdkEventMotion *event, int draw)
{
	int newx2, newy2;

	/* the event->y does goofy things if the motion continues
	 * outside the window, so we generate our own from the root
	 * coordinates.  */
	newx2 = event->x;
	newy2 = sr->y1 + (event->y_root - sr->y1_root);

	if(sr->drawn)	/* undraw old */
		draw_srange(sr);
	sr->drawn = draw;
	if(sr->type & SR_X)
		sr->x2 = newx2;
	if(sr->type & SR_Y)
		sr->y2 = newy2;
	if(draw)	/* draw new if requested */
		draw_srange(sr);
/*	printf("update_srange type=%d newx=%d newy=%d draw=%d\n",
	sr->type, sr->x2, sr->y2, draw);
	printf("m %d %d %d %d\n",
	(int)event->x, (int)event->y, 
	(int)event->x_root, (int)event->y_root);
*/

}

/*
 * draw (or undraw) a vertical-bar cursor.
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
			x = val2x(wp, csp->xval, wtable->logx);
			if(wp->drawing->window)
				gdk_draw_line(wp->drawing->window, csp->gdk_gc,
					      x, 0, x, h);
		}
	}
}

/*
 * move cursor at specified location;
 * turn it on if not on already.
 */
void 
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

	/* update all measurebuttons, those that show the values of the
	 * cursor, and those that show the value of some WaveVar at a cursor.
	 * 
	 * TODO:  pass in some indication of what changed, since only
	 * one cursor (usually) moves at a time.
	 */
	mbtn_update_all();
}

static void
window_update_cursor(WavePanel *wp, VBCursor *csp, int x)
{
	double xval;
	g_assert(csp != NULL);

	xval = x2val(wp, x, wtable->logx);
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

	if(wtable->mstate == M_NONE) {
		if(wavepanel_mouse_binding[event->button]) {
			scwm_safe_call2(
				wavepanel_mouse_binding[event->button],
				wp->smob,
				sgtk_boxed2scm (event, &sgtk_gdk_event_info, 1));
		} 
	}

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

			wtable->srange->y1 = wtable->srange->y2 = event->y;
			wtable->srange->x1 = wtable->srange->x2 = event->x;
			wtable->srange->x1_root = event->x_root;
			wtable->srange->y1_root = event->y_root;
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
		update_srange(wtable->srange, (GdkEventMotion *)event, 0);
		callback_srange();
		break;
	default:
		break;
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
		update_srange(wtable->srange, event, 1);
		break;
	default:
		/* a sort of debugging output if we get in a bad state */
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

 	if (!wtable->logx) {
 		wtable->start_xval = hsadj->value
 			* ( wtable->max_xval - wtable->min_xval ) + wtable->min_xval;
 		wtable->end_xval   = ( hsadj->value + hsadj->page_size )
 			* ( wtable->max_xval - wtable->min_xval ) + wtable->min_xval;
 	} else {
 		wtable->start_xval = wtable->min_xval * pow( wtable->max_xval / wtable->min_xval, hsadj->value );
 		wtable->end_xval   = wtable->min_xval * pow( wtable->max_xval / wtable->min_xval,
 			hsadj->value + hsadj->page_size );
 	}

	draw_labels(wtable);

	for(i = 0; i < wtable->npanels; i++) {
		wp = wtable->panels[i];
		wp->start_xval = wtable->start_xval;
		wp->end_xval = wtable->end_xval;
	}
	if(wtable->suppress_redraw == 0) {
		wtable_redraw_x();
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

/* guile initialization */

void init_event()
{

#ifndef XSCM_MAGIC_SNARF_INITS
#include "event.x"
#endif
}
