/*
 * wv - trivial waveform viewer prototype, to test Gtk+ features
 * 	and waveform-drawing strategies.
 *
 * $Log: not supported by cvs2svn $
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
static GtkWidget *win_table;
static GtkWidget *win_status_label;

/* Vertical bar cursor */
typedef struct {
	int shown;	/* vertical bar cursor */
	double xval;
	char *color_name;
	GdkColor gdk_color;
	GdkGC *gdk_gc;
} VBCursor;

/*
 * WavePanel -- describes a single panel containing one waveform
 *	later will be split into several linked structures as we expand
 *	to multiple waveforms.
 */
typedef struct
{
	DVar *var;		/* data for y-values.  should be a list? */
	double min_yval;
	double max_yval;
	double min_xval;	/* minimum and maximum data x values */
	double max_xval;
	double start_xval;	/* starting drawn x-value (independent var) */
	double end_xval;	/* ending drawn x-value */

	VBCursor *cursor[2];
	/* using hbox for labels and waveforms doesn't work well,
	 * because the bottom (X) labels then don't line up right.
	 * Perhaps a table will do the right thing
	 */
	GtkWidget *lvbox;	/* for labels */
	GtkWidget *lab_min, *lab_max;
	GtkWidget *lab_wavename;
	GtkWidget *win_drawing; /* DrawingArea for waveform */

	/* will need a color per var */
	GdkColor gdk_color;	/* the rgb values for the color */
	GdkGC *gdk_gc;	/* the graphics context entry for the color */
} WavePanel;

static void draw_pixmap(GtkWidget *widget, GdkEventExpose *event);
static void draw_labels(void);

WavePanel *wpanel;


/* convert value to pixmap y coord */
static int val2y(float val, float top, float bot, int height)
{
	return height - (height * ((val - bot ) / (top - bot)));
}

/*static double x2val(int x, double start, double end, int size)
{
	return ( (double)x/(double)size) * (end - start) + start;
}
*/
static double x2val(WavePanel *wp, int x)
{
	int w = wp->win_drawing->allocation.width;
	return ((double)x/(double)w) * (wp->end_xval - wp->start_xval) 
		+ wp->start_xval;
}

static int val2x(WavePanel *wp, double val)
{
	int w = wp->win_drawing->allocation.width;

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

static GdkPixmap *pixmap;
static GdkColormap *colormap;

static void destroy_handler(GtkWidget *widget, gpointer data)
{
	gtk_main_quit();
}

/*
 * TODO: implement dragging cursors around instead of this 
 * simple click-to-place stuff.
 */
static gint click_handler(GtkWidget *widget, GdkEventButton *event, 
			  gpointer data)
{
	double xval, dval;
	WavePanel *wp = (WavePanel *)data;
	VBCursor *csp;
	int h, x;
	char abuf[128];
	char lbuf[128];
	
	h = wp->win_drawing->allocation.height;
	xval = x2val(wp, event->x);
	switch(event->button) {
	case 1:
		csp = wp->cursor[0];
		break;
	case 2:
		csp = wp->cursor[1];
		break;
	default:
		return 0;
	}

	if(csp->shown) {
		x = val2x(wp, csp->xval);
		/* undraw old cursor */
		gdk_draw_line(wp->win_drawing->window, csp->gdk_gc, x, 0, x, h);
	}
	csp->xval = xval;
	csp->shown = 1;
	dval = cz_interp_value(wpanel->var, xval);

	/* draw new cursor */
	x = val2x(wp, csp->xval);
	gdk_draw_line(wp->win_drawing->window, csp->gdk_gc, x, 0, x, h);

	/* update value-label */
	sprintf(lbuf, "%.15s %.3f", wpanel->var->d.name, dval);
	gtk_label_set(GTK_LABEL(wpanel->lab_wavename), lbuf);

	/* update status label */
	lbuf[0] = 0;
	if(wp->cursor[0]->shown) {
		sprintf(abuf, "cursor1: %s", val2txt(wp->cursor[0]->xval));
		strcat(lbuf, abuf);
	}
	if(wp->cursor[1]->shown) {
		sprintf(abuf, " cursor2: %s", val2txt(wp->cursor[1]->xval));
		strcat(lbuf, abuf);
	}
	if(wp->cursor[0]->shown && wp->cursor[1]->shown) {
		sprintf(abuf, " delta: %s", val2txt(wp->cursor[1]->xval - wp->cursor[0]->xval));
		strcat(lbuf, abuf);
	}

	gtk_label_set(GTK_LABEL(win_status_label), lbuf);
	
	return 0;
}

static gint scroll_handler(GtkWidget *widget)
{
	GtkAdjustment *hsadj = GTK_ADJUSTMENT(widget);
	double owidth;
	owidth = wpanel->end_xval - wpanel->start_xval;

	wpanel->start_xval = hsadj->value;
	wpanel->end_xval = hsadj->value + owidth;

	draw_labels();
	draw_pixmap(wpanel->win_drawing, NULL);
	return 0;
}

/* For now: zooms are always done about the center.
 * 	adjust start and end X values for waveform graph, 
 *	adjust scrollbar percentage.
 */
static gint wv_zoom_in(GtkWidget *widget)
{
	double ocenter, owidth;
	ocenter = (wpanel->start_xval + wpanel->end_xval)/2;
	owidth = wpanel->end_xval - wpanel->start_xval;

	wpanel->start_xval = ocenter - owidth/4;
	wpanel->end_xval = ocenter + owidth/4;

	win_hsadj->value = wpanel->start_xval;
	win_hsadj->page_size = fabs(wpanel->end_xval - wpanel->start_xval);
	win_hsadj->page_increment = win_hsadj->page_size/2;

	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}

static gint wv_zoom_out(GtkWidget *widget)
{
	double ocenter, owidth;
	ocenter = (wpanel->start_xval + wpanel->end_xval)/2;
	owidth = wpanel->end_xval - wpanel->start_xval;

	wpanel->start_xval = ocenter - owidth;
	if(wpanel->start_xval < wpanel->min_xval)
		wpanel->start_xval = wpanel->min_xval;
	wpanel->end_xval = ocenter + owidth;
	if(wpanel->end_xval > wpanel->max_xval)
		wpanel->end_xval = wpanel->max_xval;

	win_hsadj->page_size = fabs(wpanel->end_xval - wpanel->start_xval);
	win_hsadj->page_increment = win_hsadj->page_size/2;
	win_hsadj->value = wpanel->start_xval;

/*	draw_labels();
	draw_pixmap(wpanel->win_drawing, NULL);
	*/
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "changed");
	gtk_signal_emit_by_name(GTK_OBJECT(win_hsadj), "value_changed");

	return 0;
}

static gint expose_handler(GtkWidget *widget, GdkEventExpose *event)
{
	int w = widget->allocation.width;
	int h = widget->allocation.height;
	

  /* On the first time through allocate a new GC and copy the window's
     black gc into it. Subsequently, free any previous pixmap, create
     a pixmap of window size and depth, and fill it with bg color. */
	if ( pixmap == NULL) {
		int i;
		colormap = gdk_window_get_colormap(widget->window);

		gdk_color_alloc(colormap, &wpanel->gdk_color);
		wpanel->gdk_gc = gdk_gc_new(widget->window);
		gdk_gc_set_foreground(wpanel->gdk_gc, &wpanel->gdk_color);

		if(bg_color_name) {
			gdk_color_alloc(colormap, &bg_gdk_color);
			bg_gdk_gc = gdk_gc_new(widget->window);
			gdk_gc_set_foreground(bg_gdk_gc, &bg_gdk_color);
		} else {
			bg_gdk_color = widget->style->bg[GTK_WIDGET_STATE(widget)];
			bg_gdk_gc = widget->style->bg_gc[GTK_WIDGET_STATE(widget)];
		}

		for(i = 0; i < 2; i++) {
			VBCursor *csp = wpanel->cursor[i];
			gdk_color_alloc(colormap, &csp->gdk_color);
 			csp->gdk_gc = gdk_gc_new(widget->window);
			if(!csp->gdk_gc) {
				fprintf(stderr, "couldn't allocate cursor %d gc\n", i);
				exit(2);
			}
			gdk_gc_set_foreground(csp->gdk_gc, &csp->gdk_color);
			/* FIX: this GDK_XOR gc doesn't work right unless
			   background color is explicitly set to "black",
			   but sometimes it happens to at least be visible */
			gdk_gc_set_background(csp->gdk_gc, &bg_gdk_color);
			gdk_gc_set_function(csp->gdk_gc, GDK_XOR);
		}
	} else {
		/* FIX: probably only need to free/new if size changed */
		gdk_pixmap_unref(pixmap);
	}

	pixmap = gdk_pixmap_new(widget->window, w, h, -1);
   
	/* since the scrollbar event also redraws everything, 
	 * let its handler do all the work by sending it the signal
	 * bonus: gets the scrollbar updated this event is due to a resize.
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
 */

static void 
draw_pixmap(GtkWidget *widget, GdkEventExpose *event)
{
	int w = widget->allocation.width;
	int h = widget->allocation.height;
	int x0, x1;
	int y0, y1;
	int i;
	double xstep;
	double xval;
	double yval;

	gdk_draw_rectangle(
		pixmap, bg_gdk_gc, TRUE, 0,0, w,h);

	xstep = (wpanel->end_xval - wpanel->start_xval)/w;

	x1 = 0;
	yval = cz_interp_value(wpanel->var, wpanel->start_xval);
	y1 = val2y(yval, wpanel->max_yval, wpanel->min_yval, h);

	for(i = 0, xval = wpanel->start_xval; i < w; i++, xval += xstep) {
		x0 = x1; y0 = y1;
		x1 = x0 + 1;
		yval = cz_interp_value(wpanel->var, xval);
		y1 = val2y(yval, wpanel->max_yval, wpanel->min_yval, h);
		gdk_draw_line(pixmap, wpanel->gdk_gc, x0,y0, x1,y1);
	}

	for(i = 0; i < 2; i++) {
		VBCursor *csp = wpanel->cursor[i];
		if(csp->shown) {
			x1 = val2x(wpanel, csp->xval);
			gdk_draw_line(pixmap, csp->gdk_gc, x1, 0, x1, h);
		}
	}

	if(event) {
	  /* Draw the exposed portions of the pixmap in its window. */
		gdk_draw_pixmap(
		  widget->window, widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		  pixmap,
		  event->area.x, event->area.y,
		  event->area.x, event->area.y,
		  event->area.width, event->area.height);
	} else {
	  /* Draw the whole thing. */
		gdk_draw_pixmap(
		  widget->window, widget->style->fg_gc[GTK_WIDGET_STATE(widget)],
		  pixmap,
		  0, 0, 
		  0, 0, 
		  w, h);
  
	}
}

/*
 * draw text labeling the waveform graph
 */
static void draw_labels(void)
{
	gtk_label_set(GTK_LABEL(win_xlabel_left), val2txt(wpanel->start_xval));
	gtk_label_set(GTK_LABEL(win_xlabel_right), val2txt(wpanel->end_xval));
}

/*
 * Construct window and initial widgets, then commence main loop.
 */
static void gtk_display(void)
{
	GtkWidget *window,  *box1, *hbox, *bbox, *btn; 
	/* Determine the minimum and nominal window sizes. */
	const int min_w=80, min_h=50, nom_w=600, nom_h=100, lab_w=80;

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
			    GTK_SIGNAL_FUNC(wv_zoom_in), NULL);
	gtk_widget_show (btn);
	
	btn = gtk_button_new_with_label ("Zoom Out");
	gtk_container_add (GTK_CONTAINER(bbox), btn);
	gtk_signal_connect (GTK_OBJECT (btn), "clicked",
			    GTK_SIGNAL_FUNC(wv_zoom_out), NULL);
	gtk_widget_show (btn);
	gtk_widget_show(bbox);

	/* label with cursor status */
	win_status_label = gtk_label_new(" ");
	gtk_box_pack_start(GTK_BOX(box1), win_status_label, FALSE, FALSE, 0);
	gtk_widget_show(win_status_label);

	/* table containing labels and waveforms */
	win_table = gtk_table_new(3,2,FALSE);
	gtk_widget_show(win_table);
	gtk_box_pack_start(GTK_BOX(box1), win_table, TRUE, TRUE, 5);

	/* area for y-axis labels and signal names */
	{
		char lbuf[128];
		wpanel->lvbox = gtk_vbox_new(FALSE, 0);
		gtk_widget_show(wpanel->lvbox);

/*	wpanel->lab_drawing = gtk_drawing_area_new();
	gtk_drawing_area_size(GTK_DRAWING_AREA(wpanel->lab_drawing), lab_w, nom_h);
	gtk_widget_show(wpanel->lab_drawing);
*/
		gtk_table_attach(GTK_TABLE(win_table), wpanel->lvbox, 
			 0, 1, 0, 1, 
			 GTK_FILL, GTK_EXPAND|GTK_FILL, 4, 0);

		sprintf(lbuf, "%.3f", wpanel->max_yval);
		wpanel->lab_max = gtk_label_new(lbuf);
		gtk_box_pack_start(GTK_BOX(wpanel->lvbox), wpanel->lab_max,
			 FALSE, FALSE, 0);
		gtk_widget_show(wpanel->lab_max);

		sprintf(lbuf, "%.3f", wpanel->min_yval);
		wpanel->lab_min = gtk_label_new(lbuf);
		gtk_box_pack_end(GTK_BOX(wpanel->lvbox), wpanel->lab_min,
			   FALSE, FALSE, 0);
		gtk_widget_show(wpanel->lab_min);

		sprintf(lbuf, "%.15s      ", wpanel->var->d.name);
		wpanel->lab_wavename = gtk_label_new(lbuf);
		gtk_box_pack_start(GTK_BOX(wpanel->lvbox), wpanel->lab_wavename,
				   FALSE, FALSE, 0);
		gtk_widget_set_name(wpanel->lab_wavename, "wavelabel");
		gtk_widget_show(wpanel->lab_wavename);

	}
	/* drawing area for waveform */
	wpanel->win_drawing = gtk_drawing_area_new();
	gtk_drawing_area_size(GTK_DRAWING_AREA(wpanel->win_drawing), nom_w, nom_h);
	gtk_widget_show(wpanel->win_drawing);
	gtk_table_attach(GTK_TABLE(win_table), wpanel->win_drawing, 
			 1, 2, 0, 1, 
			 GTK_EXPAND|GTK_FILL, GTK_EXPAND|GTK_FILL, 0, 0);

	gtk_signal_connect(
		GTK_OBJECT(wpanel->win_drawing), "expose_event", 
		(GtkSignalFunc)expose_handler, (gpointer)wpanel);
	gtk_signal_connect(
		GTK_OBJECT(wpanel->win_drawing), "button_release_event", 
		(GtkSignalFunc)click_handler, (gpointer)wpanel);
	gtk_widget_set_events(wpanel->win_drawing, 
			      GDK_EXPOSURE_MASK|GDK_BUTTON_RELEASE_MASK);
	
	/* horizontal box for X-axis labels */
	hbox = gtk_hbox_new(FALSE, 0);
	gtk_table_attach(GTK_TABLE(win_table), hbox,
			 1, 2, 1, 2,
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
		dwidth = wpanel->max_xval - wpanel->min_xval;
	win_hsadj = (GtkAdjustment *)
		gtk_adjustment_new(wpanel->start_xval, /* value */
				   wpanel->min_xval, /* lower */
				   wpanel->max_xval, /* upper */
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
		(GtkSignalFunc)scroll_handler, NULL);
	gtk_table_attach(GTK_TABLE(win_table), win_hsbar,
			 1, 2, 2, 3,
			 GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);
	gtk_widget_show(win_hsbar);

  
	/* Show the top-level window, set its minimum size, and enter the
	   Main event loop. */
	gtk_widget_show(box1);
	gtk_widget_show(window);
	gdk_window_set_hints(
		window->window, 0,0,  min_w, min_h, 0,0, GDK_HINT_MIN_SIZE);
	gtk_main();
}

/*
 * usage -- prints the standard switch info, then exits.
 */
static void usage(char *fmt, ...)
{
  va_list args;
  fflush(stdout);
  if (fmt)
    {
      va_start(args, fmt);
      fprintf(stderr, "%s: ", prog_name);
      vfprintf(stderr, fmt, args);
      fprintf(stderr, "\n");
      va_end(args);
    }
  fprintf(stderr, 
	  "%s: usage:\n"
	  "\t-h\n", prog_name);
  exit(EXIT_FAILURE);
}


void setup_colors(WavePanel *wp)
{
	int i;
/* TODO: figure out how to get these colors from styles in wv.gtkrc */
	if(!gdk_color_parse("blue", &wp->gdk_color)) {
		fprintf(stderr, "failed to parse fg color\n");
		exit(1);
	}
	
	wp->cursor[0]->color_name = "white";
	wp->cursor[1]->color_name = "yellow";
	for(i = 0; i < 2; i++) {
		if(!gdk_color_parse(wp->cursor[i]->color_name, 
				    &wp->cursor[i]->gdk_color)) {
			fprintf(stderr, "failed to parse cursor %d color\n", i);
			exit(1);
		}
	}

	if(bg_color_name) {
		if(!gdk_color_parse(bg_color_name, &bg_gdk_color)) {
			fprintf(stderr, "failed to parse bg color\n");
			exit(1);
		}
	}

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

	gtk_init(&argc, &argv);

	prog_name = argv[0];
	while ((c = getopt (argc, argv, "f:vx")) != EOF) {
		switch(c) {
		case 'f':
			field = atoi(optarg);
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

	/* manualy set up our single wave-panel
	* eventually the gui will allow (re)configuration of this setup
	*/
	wpanel = g_new0(WavePanel, 1);
	if(field >= df->ndv)
		field = 0;
	wpanel->var = df->dv[field];
	wpanel->start_xval = wpanel->var->iv->d.min;
	wpanel->end_xval = wpanel->var->iv->d.max;
	wpanel->min_xval = wpanel->var->iv->d.min;
	wpanel->max_xval = wpanel->var->iv->d.max;
	wpanel->min_yval = wpanel->var->d.min;
	wpanel->max_yval = wpanel->var->d.max;
	wpanel->cursor[0] = g_new0(VBCursor, 1);
	wpanel->cursor[1] = g_new0(VBCursor, 1);

	setup_colors(wpanel);
	gtk_display();

	return EXIT_SUCCESS;
}
