/*
 * draw.c, part of the gwave waveform viewer tool
 *
 * Functions for drawing waveforms
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
 * Revision 1.2  1998/09/17 18:33:39  tell
 * Only draw portion of wave for which there is actually data - with multiple
 * files loaded, can scroll off the end of one.
 *
 * Revision 1.1  1998/09/01 21:28:09  tell
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

/* convert value to pixmap y coord */
int val2y(double val, double top, double bot, int height)
{
	return height - ((height-2) * ((val - bot ) / (top - bot))) - 1;
}

double x2val(WavePanel *wp, int x)
{
	int w = wp->drawing->allocation.width;
	return ((double)x/(double)w) * (wp->end_xval - wp->start_xval) 
		+ wp->start_xval;
}

int val2x(WavePanel *wp, double val)
{
	int w = wp->drawing->allocation.width;

	return w * ((val - wp->start_xval) / (wp->end_xval - wp->start_xval));
}

/* convert double value to printable text,
 * using suffixes to make things more precise
 */
char *val2txt(double val)
{
	static char buf[64];
	double aval = fabs(val);
	if(1e-3 <= aval && aval < 0) {
		sprintf(buf, "%.2fm", val * 1000);
	} else if(1e-6 <= aval && aval < 1e-3) {
		sprintf(buf, "%.2fu", val * 1e6);
	} else if(1e-9 <= aval && aval < 1e-6) {
		sprintf(buf, "%.2fn", val * 1e9);
	} else if(1e-12 <= aval && aval < 1e-9) {
		sprintf(buf, "%.2fp", val * 1e12);
	} else {
		sprintf(buf, "%.2f", val);
	}
	
	return buf;
}


/*
 * half-assed wave-drawing routine.
 * gets data value and draws a line for every pixel.
 * will exhibit aliasing if data has samples at higher frequency than
 * the screen has pixels.
 * We know how to do this right, but working on other things have taken
 * precedence.
 * ALSO TODO: smarter partial redraws on scrolling, expose, etc.
 */
/* vw_wp_visit_draw(gpointer p, gpointer d) */
void
vw_wp_visit_draw(VisibleWave *vw, WavePanel *wp)
{
	int x0, x1;
	int y0, y1;
	int i;
	double xstep;
	double xval;
	double yval;
	int w = wp->drawing->allocation.width;
	int h = wp->drawing->allocation.height;

	if(!vw->gc) {
		if(!vw->label) {
			fprintf(stderr, "visit_draw(%s): label=NULL\n",
				vw->var->d.name);
			return;
			
		}
		if(!gdk_color_alloc(win_colormap, 
				    &vw->label->style->fg[GTK_STATE_NORMAL])) {
			fprintf(stderr, 
				"visit_draw(%s): gdk_color_alloc failed\n",
				vw->var->d.name);
			return;
		}
		vw->gc = gdk_gc_new(wp->drawing->window);
		gdk_gc_set_foreground(vw->gc,
				      &vw->label->style->fg[GTK_STATE_NORMAL]);
	}
	g_assert(vw->gc != NULL);

	xstep = (wp->end_xval - wp->start_xval)/w;

	x1 = 0;
	yval = an_interp_value(vw->var, wp->start_xval);
	y1 = val2y(yval, wp->max_yval, wp->min_yval, h);

	for(i = 0, xval = wp->start_xval; i < w; i++, xval += xstep) {
		x0 = x1; y0 = y1;
		x1 = x0 + 1;
		if(vw->var->iv->d.min <= xval && xval <= vw->var->iv->d.max) {
			yval = an_interp_value(vw->var, xval);
			y1 = val2y(yval, wp->max_yval, wp->min_yval, h);
			gdk_draw_line(wp->pixmap, vw->gc, x0,y0, x1,y1);
		}
	}
}

void 
draw_wavepanel(GtkWidget *widget, GdkEventExpose *event, WavePanel *wp)
{
	int w = widget->allocation.width;
	int h = widget->allocation.height;
	int x;
	int i;

	if(wp->pixmap == NULL)
		return;

	gdk_draw_rectangle(wp->pixmap, bg_gdk_gc, TRUE, 0,0, w,h);

	/* draw waves */
	g_list_foreach(wp->vwlist, (GFunc)vw_wp_visit_draw, wp); 

	/* draw cursors */
	for(i = 0; i < 2; i++) {			
		VBCursor *csp = wtable->cursor[i];
		if(csp->shown) {
			if(wp->start_xval <= csp->xval 
			   && csp->xval <= wp->end_xval) {
				x = val2x(wp, csp->xval);
				gdk_draw_line(wp->pixmap, csp->gdk_gc,
					      x, 0, x, h);
			}
		}
	}
	/* draw select-range line, if in this WavePanel */
	if(wtable->srange->drawn && wtable->srange->wp == wp)
		draw_srange(wtable->srange);

	if(event) {
	  /* Draw the exposed portions of the pixmap in its window. */
		gdk_draw_pixmap(widget->window,
				widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		  wp->pixmap,
		  event->area.x, event->area.y,
		  event->area.x, event->area.y,
		  event->area.width, event->area.height);
	} else {
	  /* Draw the whole thing. */
		gdk_draw_pixmap(widget->window,
				widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		  wp->pixmap,
		  0, 0, 0, 0, w, h);
	}
}

/*
 * update text labeling the waveform graphs' X-axis
 */
void draw_labels(void)
{
	gtk_label_set(GTK_LABEL(win_xlabel_left), val2txt(wtable->start_xval));
	gtk_label_set(GTK_LABEL(win_xlabel_right), val2txt(wtable->end_xval));
}

/* Color allocation and related stuff for waveform drawing area
 * background and cursors, done on first expose event.
 * Actually, we do it all on the first expose of the first drawing area,
 * and hope that this is OK.  They are all in the same GtkWindow.
 */
void alloc_colors(GtkWidget *widget)
{
	int i;
	if(win_colormap == NULL)
		win_colormap = gdk_window_get_colormap(widget->window);

	/* background */
	if(bg_color_name) {  /* explicitly set background color */
		gdk_color_alloc(win_colormap, &bg_gdk_color);
		bg_gdk_gc = gdk_gc_new(widget->window);
		gdk_gc_set_foreground(bg_gdk_gc, &bg_gdk_color);
	} else {  /* use the widget's default one - usually grey */
		bg_gdk_color = widget->style->bg[GTK_WIDGET_STATE(widget)];
		bg_gdk_gc = widget->style->bg_gc[GTK_WIDGET_STATE(widget)];
	}

	/* vertical bar cursors */
	for(i = 0; i < 2; i++) {
		VBCursor *csp = wtable->cursor[i];
		gdk_color_alloc(win_colormap, &csp->gdk_color);
		csp->gdk_gc = gdk_gc_new(widget->window);
		if(!csp->gdk_gc) {
			fprintf(stderr, "couldn't allocate cursor %d gc\n", i);
			exit(2);
		}
		gdk_gc_set_foreground(csp->gdk_gc, &csp->gdk_color);
		/* FIX: the GDK_XOR gcs don't work right unless
		   background color is explicitly set to "black",
		   but sometimes it happens to at least be visible */
		gdk_gc_set_background(csp->gdk_gc, &bg_gdk_color);
		gdk_gc_set_function(csp->gdk_gc, GDK_XOR);
	}

}

/* TODO: figure out how to get these colors from styles in wv.gtkrc
 * without the hack that we use for waveform colors (picking them up from
 * labels of the same color).
 */
void setup_colors(WaveTable *wt)
{
	int i;

	/* cursors */
	wt->cursor[0]->color_name = "white";
	wt->cursor[1]->color_name = "yellow";
	for(i = 0; i < 2; i++) {
		if(!gdk_color_parse(wt->cursor[i]->color_name, 
				    &wt->cursor[i]->gdk_color)) {
			fprintf(stderr, "failed to parse cursor %d color\n", i);
			exit(1);
		}
	}

	/* range-select line */
	if(!gdk_color_parse("white", &wt->srange->gdk_color)) {
		fprintf(stderr, "failed to parse selrange color\n");
		exit(1);
	}

	/* waveform background */
	if(bg_color_name) {
		if(!gdk_color_parse(bg_color_name, &bg_gdk_color)) {
			fprintf(stderr, "failed to parse bg color\n");
			exit(1);
		}
	}
}

