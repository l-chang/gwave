
/*
 * declarations and definitions for gwave - waveform viewer
 * Copyright 1998, 1999 Stephen G. Tell
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

#ifndef GWAVE_H
#define GWAVE_H

#ifndef SCWM_GUILE_H__
#include <scwm_guile.h>
#endif

#include <wavefile.h>

typedef struct _VBCursor VBCursor;
typedef struct _SelRange SelRange;
typedef struct _VisibleWave VisibleWave;
typedef struct _WavePanel WavePanel;
typedef struct _WaveTable WaveTable;
typedef struct _GWDataFile GWDataFile;
typedef struct _GWDnDData GWDnDData;
typedef enum _GWMouseState GWMouseState;
typedef struct _MeasureBtn MeasureBtn;


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


#define N_WTABLE_MBTNS 4

/* rows of the WaveTable's ftable */
#define WTABLE_FTR_TOOLBAR 0
#define WTABLE_FTR_XMHBOX  1
#define WTABLE_FTR_PTABLE  2
#define WTABLE_FTR_BHBOX  3
/*
 * WaveTable - structure describing
 *  all of the waveform-display panels and related elements
 */
struct _WaveTable {
	int npanels;
	WavePanel **panels;
	GtkWidget *window; // formerly window_main
	GtkWidget *menubar;
	GtkWidget *var_list_submenu;

	GtkWidget *ftable;	/* outer table framing most of the rest */

	GtkWidget *toolbar;
	GtkWidget *xmeasure_hbox;  /* hbox with cursor measurements */
	MeasureBtn *cursor_mbtn[N_WTABLE_MBTNS];  /* someday: make this dynamic */
	GtkWidget *vswindow;	/* GtkSrolledWindow to hold panel table */
	GtkWidget *table;	/* scrolled table containing all panels */

	GtkWidget *bot_vbox;
	GtkWidget *bot_hbox3;
	GtkWidget *xlhbox;	/* GtkHBox containing x-axis labels */
	GtkAdjustment *hsadj; // horizontal scrollbar
	GtkWidget *hsbar;
	GtkWidget *xlabel_left, *xlabel_right;
	GtkWidget *lab_xlogscale;

	VBCursor *cursor[2];
	SelRange *srange;
	double min_xval;	/* minimum and maximum data x values, */
	double max_xval;	/* over all panels */
	double start_xval;	/* starting drawn x-value (independent var) */
	double end_xval;	/* ending drawn x-value */
	int suppress_redraw;	/* don't re-draw if 1 */
	int logx;  	/* X axis scaling: 0=linear 1=log base 10 */

	GtkWidget *popup_menu;
	WavePanel *popup_panel; /*panel cursor was in when panel-popup popped*/
	GWMouseState mstate;
	VBCursor *drag_cursor;
	int button_down;
};

/*
 * structure sent as drag-and-drop-data for selecting waveforms.
 */
struct _GWDnDData {
	int magic;
	WaveVar *dv;
};

/* globals defined in gwave.c */
extern char *prog_name;
extern WaveTable *wtable;
extern const int NWColors;
extern int colors_initialized;
extern char *bg_color_name;
extern GdkColor bg_gdk_color;
extern GdkGC *bg_gdk_gc;
extern char *pg_color_name;
extern GdkColor pg_gdk_color;
extern GdkGC *pg_gdk_gc;
extern char *hl_color_name;
extern GdkColor hl_gdk_color;
extern GdkGC *hl_gdk_gc;
extern GdkColormap *win_colormap; /* colormap for main waveform window */
extern void create_about_window();
extern int v_flag;
extern SCM scm_gwave_debug;
#define gwave_debug SCM_NFALSEP(SCM_CDR(scm_gwave_debug))

/* defined in cmd.c */
extern gint cmd_zoom_full(GtkWidget *widget);
extern gint cmd_zoom_in(GtkWidget *widget);
extern gint cmd_zoom_out(GtkWidget *widget);
extern gint cmd_zoom_cursors(GtkWidget *widget);
extern gint cmd_zoom_window(GtkWidget *widget);
extern gint cmd_delete_selected_waves(GtkWidget *widget);
extern void remove_wfile_waves(GWDataFile *wdata);
extern void remove_wave_from_panel(WavePanel *wp, VisibleWave *vw);
extern SCM add_var_to_panel(WavePanel *wp, WaveVar *dv);
extern void wavepanel_update_data(WavePanel *wp);
extern void wavetable_update_data();

/* defined in draw.c */
extern void vw_wp_visit_draw(VisibleWave *vw, WavePanel *wp);
extern void draw_wavepanel(GtkWidget *widget, GdkEventExpose *event,
			   WavePanel *wp);
extern void draw_labels(WaveTable *wt);
extern double y2val(WavePanel *wp, int y);
extern int val2y(WavePanel *wp, double val);
extern double x2val(WavePanel *wp, int x, int log);
extern int val2x(WavePanel *wp, double val, int log);
extern char *val2txt(double val, int style);
extern void alloc_colors(GtkWidget *widget);
extern void setup_colors(WaveTable *wtable);
extern void vw_wp_setup_gc(VisibleWave *vw, WavePanel *wp);
extern SCM wtable_redraw_x();

/* defined in event.c */
extern void draw_srange(SelRange *sr);
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
extern void update_cursor(VBCursor *csp, double xval);

/* defined in pixmaps.c */
extern char *drag_no_xpm[];
extern char *wave_drag_ok_xpm[];

/* defined in wavelist.c */
void cmd_show_wave_list(GtkWidget *widget, GWDataFile *wdata);
extern GWDataFile *load_wave_file(char *name, char *type);
extern void get_fname_load_file(GtkWidget *w, gpointer d);
extern void reload_all_wave_files(GtkWidget *w);

extern char *possible_drag_types[];
extern char *accepted_drop_types[];
extern GList *wdata_list;  /* List of GWDataFile *'s */
extern GtkTooltips *get_gwave_tooltips();
extern SCM glist2scm(GList *list, SCM (*toscm)(void*));

#endif
