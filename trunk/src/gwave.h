
/*
 * declarations and definitions for gwave - waveform viewer
 * Steve Tell
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.1  1998/12/26 02:56:16  tell
 * Initial revision
 *
 * Revision 1.3  1998/09/30 21:50:41  tell
 * Add stuff for dragging of cursors and zoom-to-window,
 * and associated restructuring of zoom commands and mouse-button event handling
 *
 * Revision 1.2  1998/09/17 18:30:19  tell
 * Lots of changes.  multiple panels, multiple files, etc.
 *
 * Revision 1.1  1998/08/31 21:01:20  tell
 * Initial revision
 *
 */

#ifndef GWAVE_H
#define GWAVE_H

typedef struct _VBCursor VBCursor;
typedef struct _SelRange SelRange;
typedef struct _VisibleWave VisibleWave;
typedef struct _WavePanel WavePanel;
typedef struct _WaveTable WaveTable;
typedef struct _GWDataFile GWDataFile;
typedef struct _GWDnDData GWDnDData;
typedef enum _GWMouseState GWMouseState;

/*
 * state of mouse for drag operations
 */
enum _GWMouseState { M_NONE, M_CURSOR_DRAG,
		     M_SELRANGE_ARMED, M_SELRANGE_ACTIVE};

/* VBCursor - structure describing a vertical bar cursor */
struct _VBCursor {
	int shown;	/* vertical bar cursor */
	double xval;
	char *color_name;
	GdkColor gdk_color;
	GdkGC *gdk_gc;
};

typedef void (*SRFunc)(WavePanel *wp, int x1, int x2, gpointer data);

/* selecting a portion of the X axis */
struct _SelRange {
	int drawn;
	WavePanel *wp;
	GdkGC *gc;
	GdkColor gdk_color;
	int y;
	int x1, x2;
	SRFunc done_callback;
	gpointer done_data;
};

/* VisibleWave -- a waveform and anciliary stuff needed to show it
 *	in a panel.
 */
struct _VisibleWave {
	DVar *var;
	GWDataFile *gdf;
	int colorn;
	GdkGC *gc;
	GtkWidget *label;
	GtkWidget *button;
};

/*
 * WavePanel -- describes a single panel containing zero or more waveforms.
 */
struct _WavePanel {
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
	int width, height;
	int nextcolor;	/* color to use for next added waveform */
};

/*
 * WaveTable - structure describing
 *  all of the waveform-display panels and related elements
 */

struct _WaveTable {
	int npanels;
	WavePanel **panels;
	GtkWidget *vbox;	/* GtkVBox containing most elements
				   in main window. */
	GtkWidget *xlhbox;	/* GtkHBox containing x-axis labels */
	GtkWidget *table;
	VBCursor *cursor[2];
	SelRange *srange;
	double min_xval;	/* minimum and maximum data x values, */
	double max_xval;	/* over all panels */
	double start_xval;	/* starting drawn x-value (independent var) */
	double end_xval;	/* ending drawn x-value */
	int suppress_redraw;	/* don't re-draw if 1 */

	GtkWidget *popup_menu;
	WavePanel *popup_panel; /*panel cursor was in when panel-popup popped*/
	GWMouseState mstate;
	VBCursor *drag_cursor;
	int button_down;
};

/*
 * Structure to hold data for a single loaded waveform file.
 */
struct _GWDataFile {
	DataFile *df;
	GtkWidget *wlist_win;	/* window with scrolling variable list */
	GtkWidget *menu_item;	/* item in main window submenu */
	char *ftag;	/* short tag used to help identify which file is which */
};

/*
 * structure sent as drag-and-drop-data for selecting waveforms.
 */
struct _GWDnDData {
	DVar *dv;
};

/* globals defined in gwave.c */
extern char *prog_name;
extern GtkAdjustment *win_hsadj;
extern GtkWidget *win_main;
extern GtkWidget *win_hsbar;
extern GtkWidget *win_xlabel_left, *win_xlabel_right;
extern GtkWidget *win_status_label;
extern GtkWidget *var_list_submenu;
extern WaveTable *wtable;
extern const int NWColors;
extern int colors_initialized;
extern char *bg_color_name;
extern GdkColor bg_gdk_color;
extern GdkGC *bg_gdk_gc;
extern GdkColormap *win_colormap; /* colormap for main waveform window */

/* defined in cmd.c */
extern gint cmd_zoom_full(GtkWidget *widget);
extern gint cmd_zoom_in(GtkWidget *widget);
extern gint cmd_zoom_out(GtkWidget *widget);
extern gint cmd_zoom_cursors(GtkWidget *widget);
extern gint cmd_zoom_window(GtkWidget *widget);
extern gint cmd_delete_selected_waves(GtkWidget *widget);
extern void remove_wave_from_panel(WavePanel *wp, VisibleWave *vw);
extern void add_var_to_panel(WavePanel *wp, DVar *dv);
extern void wavepanel_update_data(WavePanel *wp);
extern void wavetable_update_data();

/* defined in draw.c */
extern void vw_wp_visit_draw(VisibleWave *vw, WavePanel *wp);
extern void draw_wavepanel(GtkWidget *widget, GdkEventExpose *event,
			   WavePanel *wp);
extern void draw_labels(void);
extern int val2y(double val, double top, double bot, int height);
extern double x2val(WavePanel *wp, int x);
extern int val2x(WavePanel *wp, double val);
extern char *val2txt(double val);
extern void alloc_colors(GtkWidget *widget);
extern void setup_colors(WaveTable *wtable);
extern void vw_wp_setup_gc(VisibleWave *vw, WavePanel *wp);

/* defined in event.c */
extern void draw_srange(SelRange *sr);
extern void select_x_range(SRFunc func, gpointer data);
extern gint button_press_handler(GtkWidget *widget, GdkEventButton *event, 
			  gpointer data);
extern gint button_release_handler(GtkWidget *widget, GdkEventButton *event, 
			  gpointer data);
extern gint motion_handler(GtkWidget *widget, GdkEventMotion *event, 
			  gpointer data);
extern gint scroll_handler(GtkWidget *widget);
extern gint expose_handler(GtkWidget *widget, GdkEventExpose *ev, 
			   WavePanel *wp);
extern void destroy_handler(GtkWidget *widget, gpointer data);
extern void wavepanel_dnd_drop (GtkWidget *button, GdkEvent *ev, gpointer d);

/* defined in pixmaps.c */
extern char *drag_no_xpm[];
extern char *wave_drag_ok_xpm[];

/* defined in wavelist.c */
void cmd_show_wave_list(GtkWidget *widget, GWDataFile *wdata);
extern int load_wave_file(char *name, char *type);
extern void get_fname_load_file(GtkWidget *w, gpointer d);
extern char *possible_drag_types[];
extern char *accepted_drop_types[];
extern GList *wdata_list;  /* List of GWDataFile *'s */

/* defined in wavewin.c */
extern void create_wdata_submenuitem(GWDataFile *wdata, GtkWidget *submenu);
extern void setup_waveform_window();
extern void vw_get_label_string(char *buf, int buflen, VisibleWave *vw);
extern void vw_wp_create_button(VisibleWave *vw, WavePanel *wp);
extern void wavewin_insert_panel(WavePanel *wp);
extern void wavewin_delete_panel(WavePanel *wp);
extern void cmd_popup_delete_panel(GtkWidget *w);
extern void cmd_popup_insert_panel(GtkWidget *w);
extern void cmd_append_panel(GtkWidget *w);

#endif