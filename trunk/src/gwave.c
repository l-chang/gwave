/*
 * wv - trivial waveform viewer prototype, to test Gtk+ features
 * 	and waveform-drawing strategies.
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.5  1998/08/25 21:17:25  tell
 * Revised handling of multiple waveform colors, now gets them from styles
 * on the waveform-label widgets, which are set from the wv.gtkrc.
 *
 * Revision 1.4  1998/08/25 17:29:31  tell
 * Support for multiple panels
 *
 * Revision 1.3  1998/08/25 13:49:47  tell
 * added support for second vertical-bar cursor
 *
 * Revision 1.2  1998/08/24 17:48:03  tell
 * Convert to table for arranging wavepanel and labels
 * Got basic Y labels working, although not arranged quite perfectly
 * Now reads a wv.gtkrc file
 * Added basic status-label to show times, etc.
 *
 * Revision 1.1  1998/08/21 19:11:38  tell
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

static char *prog_name = "wv";
int x_flag, v_flag;

static char *bg_color_name  = "black" ;
static GdkColor bg_gdk_color;
static GdkGC *bg_gdk_gc;

static GtkAdjustment *win_hsadj;
static GtkWidget *win_hsbar;
static GtkWidget *win_xlabel_left, *win_xlabel_right;
static GtkWidget *win_status_label;

/* Colors for drawing waveforms into drawing areas 
 * in main window
 */
typedef struct {
	char *name;
	GdkColor gdk_color;
	GdkGC *gc;
} MyColor;

int colors_initialized = 0;
const int NWColors = 6;  /* this many wavecolorN styles must be in wv.gtkrc */

/* VBCursor - structure describing a vertical bar cursor */
typedef struct {
	int shown;	/* vertical bar cursor */
	double xval;
	char *color_name;
	GdkColor gdk_color;
	GdkGC *gdk_gc;
} VBCursor;

/* VisibleWave -- a waveform and anciliary stuff needed to show it
 *	in a panel.
 */
typedef struct
{
	DVar *var;
	int colorn;
	GdkGC *gc;
	GtkWidget *label;
	GtkWidget *button;
} VisibleWave;

/*
 * WavePanel -- describes a single panel containing one waveform
 *	later will be split into several linked structures as we expand
 *	to multiple waveforms.
 */
typedef struct
{
	GList *vwlist;	/* list of waves shown in this panel. NULL if none */
	double min_yval; /* min/max data x/y values over whole vwlist */
	double max_yval;
	double min_xval;	
	double max_xval;

	/* starting and ending drawn x-value (independent var),
	* copied from corresponding wtable values when we scroll/zoom,
	* later we may allow individual panels to be "locked" 
	* from global scroll/zoom or otherwise controlled independently */
	double start_xval;	
	double end_xval;

	GtkWidget *lvbox;	/* for Y-labels */
	GtkWidget *lab_min, *lab_max;
	GtkWidget *drawing; /* DrawingArea for waveforms */
	GdkPixmap *pixmap;
} WavePanel;

/*
 * WaveTable - structure describing
 *  all of the waveform-display panels and related elements
 */

typedef struct {
	int npanels;
	WavePanel *panels;
	GtkWidget *table;
	VBCursor *cursor[2];
	double min_xval;	/* minimum and maximum data x values, */
	double max_xval;	/* over all panels */
	double start_xval;	/* starting drawn x-value (independent var) */
	double end_xval;	/* ending drawn x-value */
} WaveTable;

void alloc_colors(GtkWidget *widget);
static void draw_pixmap(GtkWidget *widget, GdkEventExpose *event, WavePanel *wp);
static void draw_labels(void);
void remove_wave_from_panel(WavePanel *wp, VisibleWave *vw);

WaveTable *wtable;

/* convert value to pixmap y coord */
static int val2y(double val, double top, double bot, int height)
{
	return height - ((height-2) * ((val - bot ) / (top - bot))) - 1;
}

static double x2val(WavePanel *wp, int x)
{
	int w = wp->drawing->allocation.width;
	return ((double)x/(double)w) * (wp->end_xval - wp->start_xval) 
		+ wp->start_xval;
}

static int val2x(WavePanel *wp, double val)
{
	int w = wp->drawing->allocation.width;

	return w * ((val - wp->start_xval) / (wp->end_xval - wp->start_xval));
}

/* convert double value to printable text,
 * using suffixes to make things more precise
 */
static char *val2txt(double val)
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
 * Gtk display handler stuff...
 */

static GdkColormap *colormap; /* colormap for main waveform window */

static void destroy_handler(GtkWidget *widget, gpointer data)
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
	WavePanel *wp = (WavePanel *)d;
	double dval;
	char lbuf[64];

	dval = cz_interp_value(vw->var, wtable->cursor[0]->xval);
	sprintf(lbuf, "%.15s %.3f", vw->var->d.name, dval);
	gtk_label_set(GTK_LABEL(vw->label), lbuf);
}

/*
 * TODO: implement dragging cursors around instead of this 
 * simple click-to-place stuff.
 */
static gint click_handler(GtkWidget *widget, GdkEventButton *event, 
			  gpointer data)
{

	double xval;
	WavePanel *wp = (WavePanel *)data;
	VBCursor *csp;
	int h, x,i;
	char abuf[128];
	char lbuf[128];

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

	/* undraw old cursor */
	if(csp->shown) {
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

	csp->xval = xval;
	csp->shown = 1;
	/* draw cursor in each panel */
	for(i = 0; i < wtable->npanels; i++) {
		wp = &wtable->panels[i];
		h = wp->drawing->allocation.height;
		if(wp->start_xval <= csp->xval 
		   && csp->xval <= wp->end_xval) {
			x = val2x(wp, csp->xval);
			gdk_draw_line(wp->drawing->window,
				      csp->gdk_gc, x, 0, x, h);
		}
	}

	/* update name/value label */
	if(event->button == 1) {
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

	return 0;
}

static gint scroll_handler(GtkWidget *widget)
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
		draw_pixmap(wp->drawing, NULL, wp);
	}
	return 0;
}

/* For now: zooms are always done about the center.
 * 	adjust start and end X values for waveform graph, 
 *	adjust scrollbar percentage.
 */
static gint cmd_zoom_in(GtkWidget *widget)
{
	double ocenter, owidth;
	ocenter = (wtable->start_xval + wtable->end_xval)/2;
	owidth = wtable->end_xval - wtable->start_xval;

	wtable->start_xval = ocenter - owidth/4;
	wtable->end_xval = ocenter + owidth/4;

	win_hsadj->value = wtable->start_xval;
	win_hsadj->page_size = fabs(wtable->end_xval - wtable->start_xval);
	win_hsadj->page_increment = win_hsadj->page_size/2;

	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}

static gint cmd_zoom_out(GtkWidget *widget)
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
	win_hsadj->value = wtable->start_xval;

	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}

static gint cmd_zoom_full(GtkWidget *widget)
{
	wtable->start_xval = wtable->min_xval;
	wtable->end_xval = wtable->max_xval;

	win_hsadj->page_size = fabs(wtable->end_xval - wtable->start_xval);
	win_hsadj->page_increment = win_hsadj->page_size/2;
	win_hsadj->value = wtable->start_xval;

	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}

/* Get the foreground color for the waveform by using the GdkColor
 * of the corresponding label.
 */
static void vw_wp_setup_gc(gpointer p, gpointer d)
{
	VisibleWave *vw = (VisibleWave *)p;
	WavePanel *wp = (WavePanel *)d;
	
	if(!vw->gc) {
		gdk_color_alloc(colormap, &vw->label->style->fg[GTK_STATE_NORMAL]);
		vw->gc = gdk_gc_new(wp->drawing->window);
		gdk_gc_set_foreground(vw->gc, &vw->label->style->fg[GTK_STATE_NORMAL]);
	}
}

static gint expose_handler(GtkWidget *widget, GdkEventExpose *event,
			   WavePanel *wp)
{
	int w = widget->allocation.width;
	int h = widget->allocation.height;

	if(!colors_initialized) {
		alloc_colors(widget);
		colors_initialized = 1;
	/*
	 * FIX: only need to call this the first time around for each
	 * wavepanel.
	 */
	}
	g_list_foreach(wp->vwlist, vw_wp_setup_gc, wp);

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

/*
 * half-assed wave-drawing routine.
 * gets data value and draws a line for every pixel.
 * will exhibit aliasing if data has samples at higher frequency than
 * the screen has pixels.
 * We know how to do this right, but other things have taken precedence.
 * ALSO TODO: smarter partial redraws on scrolling, expose, etc.
 */

void
vw_wp_visit_draw(gpointer p, gpointer d)
{
	VisibleWave *vw = (VisibleWave *)p;
	WavePanel *wp = (WavePanel *)d;
	int x0, x1;
	int y0, y1;
	int i;
	double xstep;
	double xval;
	double yval;
	int w = wp->drawing->allocation.width;
	int h = wp->drawing->allocation.height;

	xstep = (wp->end_xval - wp->start_xval)/w;

	x1 = 0;
	yval = cz_interp_value(vw->var, wp->start_xval);
	y1 = val2y(yval, wp->max_yval, wp->min_yval, h);

	for(i = 0, xval = wp->start_xval; i < w; i++, xval += xstep) {
		x0 = x1; y0 = y1;
		x1 = x0 + 1;
		yval = cz_interp_value(vw->var, xval);
		y1 = val2y(yval, wp->max_yval, wp->min_yval, h);
		gdk_draw_line(wp->pixmap, vw->gc, x0,y0, x1,y1);
	}
}

static void 
draw_pixmap(GtkWidget *widget, GdkEventExpose *event, WavePanel *wp)
{
	int w = widget->allocation.width;
	int h = widget->allocation.height;
	int x;
	int i;

	if(wp->pixmap == NULL)
		return;

	gdk_draw_rectangle(
		wp->pixmap, bg_gdk_gc, TRUE, 0,0, w,h);

	g_list_foreach(wp->vwlist, vw_wp_visit_draw, wp); /* draw waves */

	for(i = 0; i < 2; i++) {			/* draw cursors */
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
static void draw_labels(void)
{
	gtk_label_set(GTK_LABEL(win_xlabel_left), val2txt(wtable->start_xval));
	gtk_label_set(GTK_LABEL(win_xlabel_right), val2txt(wtable->end_xval));
}

/*
 * vw_wp_delete_if_selected
 */
void
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
static gint cmd_delete_selected_waves(GtkWidget *widget)
{
	int i;
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = &wtable->panels[i];
		g_list_foreach(wp->vwlist, vw_wp_delete_if_selected, wp);
	}
	return 0;
}

/*
 * vw_wp_create_button -- called from g_list_foreach to 
 * create button and label widgets for one VisibleWave in a WavePanel
 */
void
vw_wp_create_button(gpointer p, gpointer d)
{
	VisibleWave *vw = (VisibleWave *)p;
	WavePanel *wp = (WavePanel *)d;
	char lbuf[128];

	sprintf(lbuf, "%.15s      ", vw->var->d.name);
	vw->label = gtk_label_new(lbuf);
	vw->button = gtk_toggle_button_new();
	gtk_container_add(GTK_CONTAINER(vw->button), vw->label);
	gtk_box_pack_start(GTK_BOX(wp->lvbox), vw->button,
			   FALSE, FALSE, 0);
	sprintf(lbuf, "wavecolor%d", vw->colorn);
	gtk_widget_set_name(vw->label, lbuf);
	gtk_widget_show(vw->label);
	gtk_widget_show(vw->button);
}

/*
 * Construct window and initial widgets, then commence main loop.
 */
static void setup_waveform_window(void)
{
	int i;
	GtkWidget *window,  *box1, *hbox, *bbox, *btn;
	/* Determine the minimum and nominal window sizes. */
	const int min_w=80, min_h=50, nom_w=600, nom_h=100;

	/* Create a top-level window. Set the title and establish delete and
	   destroy event handlers. */
	window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_name(window, prog_name);
	gtk_signal_connect(
		GTK_OBJECT(window), "destroy",
		GTK_SIGNAL_FUNC(destroy_handler), NULL);
	gtk_signal_connect(
		GTK_OBJECT(window), "delete_event",
		GTK_SIGNAL_FUNC(destroy_handler), NULL);
	gtk_container_border_width (GTK_CONTAINER (window), 10);

	/* create the vertical box, and add it to the window */
	box1 = gtk_vbox_new(FALSE, 0);
	gtk_container_add (GTK_CONTAINER (window), box1);

	/* create horizontal button box, add to top, put some buttons in it
	* When we get more than about 5 commands, build a menu. */
	bbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_START);
	gtk_button_box_set_spacing(GTK_BUTTON_BOX(bbox), 5);
	gtk_box_pack_start(GTK_BOX(box1), bbox, FALSE, FALSE, 0);

	btn = gtk_button_new_with_label ("Quit");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(destroy_handler), NULL);
	gtk_widget_show (btn);

	btn = gtk_button_new_with_label ("Zoom In");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(cmd_zoom_in), NULL);
	gtk_widget_show (btn);
	
	btn = gtk_button_new_with_label ("Zoom Out");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(cmd_zoom_out), NULL);
	gtk_widget_show (btn);

	btn = gtk_button_new_with_label ("Delete");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(cmd_delete_selected_waves), NULL);
	gtk_widget_show (btn);
	gtk_widget_show(bbox);

	/* label with cursor status */
	win_status_label = gtk_label_new(" ");
	gtk_box_pack_start(GTK_BOX(box1), win_status_label, FALSE, FALSE, 0);
	gtk_widget_show(win_status_label);

	/* table containing labels and waveforms */
	wtable->table = gtk_table_new(wtable->npanels+2,2,FALSE);
	gtk_widget_show(wtable->table);
	gtk_box_pack_start(GTK_BOX(box1), wtable->table, TRUE, TRUE, 5);

	for(i = 0; i < wtable->npanels; i++) {
		char lbuf[128];
		WavePanel *wp = &wtable->panels[i];

		/* y-axis labels and signal names, all in a vbox */
		wp->lvbox = gtk_vbox_new(FALSE, 0);
		gtk_widget_show(wp->lvbox);

		gtk_table_attach(GTK_TABLE(wtable->table), wp->lvbox, 
			 0, 1, i, i+1, 
			 GTK_FILL, GTK_EXPAND|GTK_FILL, 4, 0);

		sprintf(lbuf, "%.3f", wp->max_yval);
		wp->lab_max = gtk_label_new(lbuf);
		gtk_box_pack_start(GTK_BOX(wp->lvbox), wp->lab_max,
			 FALSE, FALSE, 0);
		gtk_widget_show(wp->lab_max);

		sprintf(lbuf, "%.3f", wp->min_yval);
		wp->lab_min = gtk_label_new(lbuf);
		gtk_box_pack_end(GTK_BOX(wp->lvbox), wp->lab_min,
			   FALSE, FALSE, 0);
		gtk_widget_show(wp->lab_min);

		g_list_foreach(wp->vwlist, vw_wp_create_button, wp);

		/* drawing area for waveform */
		wp->drawing = gtk_drawing_area_new();
		gtk_drawing_area_size(GTK_DRAWING_AREA(wp->drawing), nom_w, nom_h);
		gtk_widget_show(wp->drawing);
		gtk_table_attach(GTK_TABLE(wtable->table), wp->drawing, 
			 1, 2, i, i+1, 
			 GTK_EXPAND|GTK_FILL, GTK_EXPAND|GTK_FILL, 0, 1);

		gtk_signal_connect(
			GTK_OBJECT(wp->drawing), "expose_event", 
			(GtkSignalFunc)expose_handler, (gpointer)wp);
		gtk_signal_connect(
			GTK_OBJECT(wp->drawing), "button_release_event", 
			(GtkSignalFunc)click_handler, (gpointer)wp);
		gtk_widget_set_events(wp->drawing, 
		      GDK_EXPOSURE_MASK|GDK_BUTTON_RELEASE_MASK);
	}
	/* horizontal box for X-axis labels */
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_table_attach(GTK_TABLE(wtable->table), hbox,
			 1, 2, wtable->npanels, wtable->npanels+1,
			 GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);

	win_xlabel_left = gtk_label_new("L");
	gtk_box_pack_start(GTK_BOX(hbox), win_xlabel_left, FALSE, FALSE, 0);
	gtk_widget_show(win_xlabel_left);

	win_xlabel_right = gtk_label_new("R");
	gtk_box_pack_end(GTK_BOX(hbox), win_xlabel_right, FALSE, FALSE, 0);
	gtk_widget_show(win_xlabel_right);

	gtk_widget_show(hbox);

	/* scrollbar */
	{
		double dwidth;
		dwidth = wtable->max_xval - wtable->min_xval;
	win_hsadj = (GtkAdjustment *)
		gtk_adjustment_new(wtable->start_xval, /* value */
				   wtable->min_xval, /* lower */
				   wtable->max_xval, /* upper */
				   dwidth/nom_w,	/* step increment */
				   dwidth/2, 		/* page increment */
				   dwidth		/* page_size */
			);
	}
	win_hsbar = gtk_hscrollbar_new(GTK_ADJUSTMENT(win_hsadj));
	gtk_range_set_update_policy (GTK_RANGE (win_hsbar), 
			       GTK_UPDATE_CONTINUOUS);
	gtk_signal_connect(
		GTK_OBJECT(win_hsadj), "value_changed", 
		(GtkSignalFunc)scroll_handler, (gpointer)wtable);
	gtk_table_attach(GTK_TABLE(wtable->table), win_hsbar,
			 1, 2, wtable->npanels+1, wtable->npanels+2,
			 GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);
	gtk_widget_show(win_hsbar);

	/* Show the top-level window, set its minimum size */
	gtk_widget_show(box1);
	gtk_widget_show(window);
	gdk_window_set_hints(window->window, 0,0,  min_w, min_h, 0,0,
			     GDK_HINT_MIN_SIZE);
}

/* Color allocation and related stuff for waveform drawing areas.
 * done on first expose event.
 * Actually, we do it all on the first expose of the first drawing area,
 * and hope that this is OK.  They are all in the same GtkWindow.
 */
void alloc_colors(GtkWidget *widget)
{
	int i;
	if(colormap == NULL)
		colormap = gdk_window_get_colormap(widget->window);

	/* background */
	if(bg_color_name) {  /* explicitly set background color */
		gdk_color_alloc(colormap, &bg_gdk_color);
		bg_gdk_gc = gdk_gc_new(widget->window);
		gdk_gc_set_foreground(bg_gdk_gc, &bg_gdk_color);
	} else {  /* use the widget's default one - usually grey */
		bg_gdk_color = widget->style->bg[GTK_WIDGET_STATE(widget)];
		bg_gdk_gc = widget->style->bg_gc[GTK_WIDGET_STATE(widget)];
	}

	/* vertical bar cursors */
	for(i = 0; i < 2; i++) {
		VBCursor *csp = wtable->cursor[i];
		gdk_color_alloc(colormap, &csp->gdk_color);
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

/* TODO: figure out how to get these colors from styles in wv.gtkrc */
void setup_colors(WaveTable *wt)
{
	int i;

	wt->cursor[0]->color_name = "white";
	wt->cursor[1]->color_name = "yellow";
	for(i = 0; i < 2; i++) {
		if(!gdk_color_parse(wt->cursor[i]->color_name, 
				    &wt->cursor[i]->gdk_color)) {
			fprintf(stderr, "failed to parse cursor %d color\n", i);
			exit(1);
		}
	}

	/* waveform background */
	if(bg_color_name) {
		if(!gdk_color_parse(bg_color_name, &bg_gdk_color)) {
			fprintf(stderr, "failed to parse bg color\n");
			exit(1);
		}
	}
}

/* vw_visit_update_wp --
 * called with g_list_foreach to update a WavePanel from all of its
 * VisibleWaves.
 */
void
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
 */
void
wavepanel_update_data(WavePanel *wp)
{
	wp->min_xval = G_MAXDOUBLE;
	wp->max_xval = G_MINDOUBLE;
	wp->min_yval = G_MAXDOUBLE;
	wp->max_yval = G_MINDOUBLE;
	g_list_foreach(wp->vwlist, vw_wp_visit_update_data, (gpointer)wp);
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
		if(wp->min_xval < wtable->min_xval)
			wtable->min_xval = wp->min_xval;
		if(wp->max_xval > wtable->max_xval)
			wtable->max_xval = wp->max_xval;
	}
}

/*
 * remove waveform from panel
 * 	Somthing bad will probably happen if the waveform isn't actually
 * 	in the indicated panel.
 */
void
remove_wave_from_panel(WavePanel *wp, VisibleWave *vw)
{
	gtk_container_remove(GTK_CONTAINER(wp->lvbox), vw->button);
/*	gtk_container_remove(GTK_CONTAINER(vw->button), vw->label); */
	gtk_widget_destroy(vw->button);
/*	gtk_widget_destroy(vw->label); */

	wp->vwlist = g_list_remove(wp->vwlist, vw);
	gdk_gc_destroy(vw->gc);
	g_free(vw);

	wavepanel_update_data(wp);
	wavetable_update_data();
}

/*
 * Add a waveform to a WavePanel
 */
void
add_var_to_panel(WavePanel *wp, DVar *dv, int colorno)
{
	VisibleWave *vw;

	vw = g_new0(VisibleWave, 1);
	vw->colorn = colorno;
	vw->var = dv;

	wp->vwlist = g_list_append(wp->vwlist, vw);
	wavepanel_update_data(wp);
	wp->start_xval = wp->min_xval;
	wp->end_xval = wp->max_xval;

	wavetable_update_data();
}


/*
 * usage -- prints the standard switch info, then exits.
 */
static void usage(char *fmt, ...)
{
	va_list args;
	fflush(stdout);
	if (fmt) {
		va_start(args, fmt);
		fprintf(stderr, "%s: ", prog_name);
		vfprintf(stderr, fmt, args);
		fprintf(stderr, "\n");
		va_end(args);
	}
	fprintf(stderr, "Usage: %s [options] waveform-file\n", prog_name);
	fprintf(stderr, " options:\n");
	fprintf(stderr, " -p N     Use N panels\n");
	exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
	int c;
	extern int optind;
	extern char *optarg;
	int errflg = 0;
	int x_flag, v_flag;
	DataFile *df;
	int field = 0;
	int npanels = 1;
	int i;

	gtk_init(&argc, &argv);

	prog_name = argv[0];
	while ((c = getopt (argc, argv, "f:p:vx")) != EOF) {
		switch(c) {
		case 'f':
			field = atoi(optarg);
			break;
		case 'p':
			npanels = atoi(optarg);
			break;
		case 'v':
			v_flag = 1;
			break;
		case 'x':
			x_flag = 1;
			break;
		default:
			errflg = 1;
			break;
		}
	}
	if(errflg) {
		usage(NULL);
		exit(1);
	}

	if(optind >= argc)  {
		usage("no waveform file specified");
		exit(1);
	}
	gtk_rc_parse("wv.gtkrc");

	df = cz_read_file(argv[optind]);
	if(!df) {
		if(errno)
			perror(argv[1]);
		fprintf(stderr, "unable to read data file\n");
		exit(1);
	}
	if(npanels > 8)
		npanels = 8;

	wtable = g_new0(WaveTable, 1);
	wtable->npanels = npanels;
	wtable->panels = g_new0(WavePanel, npanels);
	wtable->cursor[0] = g_new0(VBCursor, 1);
	wtable->cursor[1] = g_new0(VBCursor, 1);

	/* manualy set up the waves into specific WavePanels
	* eventually the gui will allow (re)configuration of this setup
	*/
	if(field >= df->ndv)
		field = 0;
/*	for(i = 0; i < npanels && field < df->ndv; field++, i++) {
		add_var_to_panel(&wtable->panels[i], df->dv[field],
				 i % NWColors);
	}
*/
	for(i = 0; i < df->ndv; i++) {
		add_var_to_panel(&wtable->panels[i % npanels], df->dv[i],
				 i % NWColors);
	}
	wtable->start_xval = wtable->min_xval;
	wtable->end_xval = wtable->max_xval;

	setup_colors(wtable);
	setup_waveform_window();

	gtk_main();
	return EXIT_SUCCESS;
}
