/*
 * wavewin.c, part of the gwave waveform viewer tool
 *
 * Functions in this file set up the main waveform window GUI.
 *
 * Copyright (C) 1998, 1999 Stephen G. Tell.
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
 * License along with this program; if not, write to the Free
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
#include <guile-gtk.h>

#include <config.h>
#include <scwm_guile.h>
#include <gwave.h>
#include <wavelist.h>

#define WAVEWIN_IMPLEMENTATION
#include <wavewin.h>

#define WAVEPANEL_MIN_WIDTH 400
#define WAVEPANEL_STD_HEIGHT 100
#define WAVEPANEL_JGE_HEIGHT 25

SCWM_HOOK(new_wavewin_hook,"new-wavewin-hook", 0);
  /** This hook is invoked with no arguments as the main waveform
window is created.
The main purpose of this hook is to create the menu and tool items
and add them to the menu/tool bars.
*/

SCWM_HOOK(new_wavepanel_hook,"new-wavepanel-hook", 1);
  /** This hook is invoked with one WavePanel argument when the
WavePanel is created.
The main purpose of this hook is to create the popup menu
and other event bindings for the wavepanel.
*/

SCM wavepanel_mouse_binding[6];

GtkWidget *win_main;
GtkWidget *win_main_menubar;
GtkWidget *win_main_toolbar;

WavePanel *last_drop_wavepanel;

/* generate VisibleWave button label string,
 * for both initial setup and updating.
 */
void
vw_get_label_string(char *buf, int buflen, VisibleWave *vw)
{
	GWDataFile *gdf;
	double xval, dval;
	int n, l;

	gdf = vw->gdf;
	g_assert(gdf != NULL);

	l = buflen - strlen(gdf->ftag) - 10;
	n = MIN(l, 15);
	xval = wtable->cursor[0]->xval;
	if(vw->var->wv_iv->wds->min <= xval 
	   && xval <= vw->var->wv_iv->wds->max) {

		dval = wv_interp_value(vw->var, xval);
		sprintf(buf, "%s: %.*s %s",
			gdf->ftag, l, vw->varname, val2txt(dval, 0));
	} else {
		/* should keep track of label state (name vs. name+val)
		 * and only re-do this if necessary */
		sprintf(buf, "%s: %.*s    ", gdf->ftag, l, vw->varname);
	}
}

/*
 * vw_wp_create_button -- called from g_list_foreach to 
 * create button and label widgets for one VisibleWave in a WavePanel
 */
void
vw_wp_create_button(VisibleWave *vw, WavePanel *wp)
{
	char lbuf[64];

	vw_get_label_string(lbuf, 64, vw);
	vw->label = gtk_label_new(lbuf);
	vw->button = gtk_toggle_button_new();
	gtk_container_add(GTK_CONTAINER(vw->button), vw->label);
	gtk_box_pack_start(GTK_BOX(wp->lvbox), vw->button,
			   FALSE, FALSE, 0);
	sprintf(lbuf, "wavecolor%d", vw->colorn);
	gtk_widget_set_name(vw->label, lbuf);
	gtk_widget_show(vw->label);
	gtk_widget_set_name(vw->button, "wavebutton");
	gtk_widget_show(vw->button);
}

/* create horizontal button box for top of main window */
GtkWidget *create_toolbar()
{
	GtkWidget *bbox, *btn;

	bbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_START);
	gtk_button_box_set_spacing(GTK_BUTTON_BOX(bbox), 5);
	gtk_widget_show(bbox);
	return bbox;
}

/* horizontal box for X-axis labels */
GtkWidget *create_xlabel_hbox()
{
	GtkWidget *hbox;
	hbox = gtk_hbox_new(FALSE, 0);
	win_xlabel_left = gtk_label_new("0");
	gtk_box_pack_start(GTK_BOX(hbox), win_xlabel_left, FALSE, FALSE, 0);
	gtk_widget_show(win_xlabel_left);

	win_xlabel_right = gtk_label_new("0");
	gtk_box_pack_end(GTK_BOX(hbox), win_xlabel_right, FALSE, FALSE, 0);
	gtk_widget_show(win_xlabel_right);

	gtk_widget_show(hbox);
	return hbox;
}

/* Allocate a new WavePanel and do basic setup */
WavePanel *
new_wave_panel()
{
	WavePanel *wp;
	wp = g_new0(WavePanel, 1);
	wp->valid = 1;

	SGT_NEWCELL_SMOB(wp->smob, WavePanel, wp);
	scm_protect_object(wp->smob);
	wp->outstanding_smob = 1;
	call1_hooks(new_wavepanel_hook, wp->smob);

	return wp;
}

/*
 * set or change type-related stuff for new or existing wavepanel.
 * ptype: panel type
 *    0 - original
 *    1 - short; smaller minimum size, doesn't show Y min/max labels.
 *		lets more panels fit for digital signals.
 */
void set_wave_panel_type(WavePanel *wp, int ptype)
{
	wp->ptype = ptype;
	if(ptype == 1) {
		gtk_widget_hide(wp->lab_min_hbox);
		gtk_widget_hide(wp->lab_max_hbox);
		wp->req_height = WAVEPANEL_JGE_HEIGHT;
	} else {
		gtk_widget_show(wp->lab_min_hbox);
		gtk_widget_show(wp->lab_max_hbox);
		wp->req_height = WAVEPANEL_STD_HEIGHT;
	}
	gtk_drawing_area_size(GTK_DRAWING_AREA(wp->drawing), 
			      wp->width ? wp->width : WAVEPANEL_MIN_WIDTH,
			      wp->req_height);
}

/*
 * Set up widgets for a newly-created WavePanel -
 *    construct lvbox and drawing area
 */ 
void setup_wave_panel(WavePanel *wp, int ptype)
{
	char lbuf[128];
	int min_h;

	if(v_flag) {
		fprintf(stderr, "setup_wave_panel ptype=%d min_h=%d\n", ptype, min_h);
	}
	wp->start_xval = wtable->start_xval;
	wp->end_xval = wtable->end_xval;

	/* y-axis labels and signal names, all in a vbox */
	wp->lvbox = gtk_vbox_new(FALSE, 0);
	gtk_widget_set_usize(wp->lvbox, 160, -1);
	gtk_widget_show(wp->lvbox);

	wp->lab_max_hbox = gtk_hbox_new(FALSE, 0);
	strcpy(lbuf, val2txt(wp->max_yval, 0));
	wp->lab_max = gtk_label_new(lbuf);
	gtk_box_pack_start(GTK_BOX(wp->lvbox), wp->lab_max_hbox,
			   FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(wp->lab_max_hbox), wp->lab_max,
			 FALSE, FALSE, 0);
	gtk_widget_show(wp->lab_max);

	wp->lab_min_hbox = gtk_hbox_new(FALSE, 0);
	strcpy(lbuf, val2txt(wp->min_yval, 0));
	wp->lab_min = gtk_label_new(lbuf);
	gtk_box_pack_end(GTK_BOX(wp->lvbox), wp->lab_min_hbox,
			 FALSE, FALSE, 0);
	gtk_box_pack_end(GTK_BOX(wp->lab_min_hbox), wp->lab_min,
			 FALSE, FALSE, 0);
	gtk_widget_show(wp->lab_min);

	g_list_foreach(wp->vwlist, (GFunc)vw_wp_create_button, wp);

	/* drawing area for waveform */
	wp->drawing = gtk_drawing_area_new();
	gtk_signal_connect(
		GTK_OBJECT(wp->drawing), "expose_event", 
		(GtkSignalFunc)expose_handler, (gpointer)wp);
	gtk_signal_connect(
		GTK_OBJECT(wp->drawing), "button_press_event", 
		(GtkSignalFunc)button_press_handler, (gpointer)wp);
	gtk_signal_connect(
		GTK_OBJECT(wp->drawing), "button_release_event", 
		(GtkSignalFunc)button_release_handler, (gpointer)wp);
	gtk_signal_connect(
		GTK_OBJECT(wp->drawing), "motion_notify_event", 
		(GtkSignalFunc)motion_handler, (gpointer)wp);

	set_wave_panel_type(wp, ptype);
	gtk_widget_show(wp->drawing);

	dnd_setup_target(wp->drawing, wp);

	gtk_widget_set_events(wp->drawing, 
			      GDK_EXPOSURE_MASK|GDK_BUTTON_RELEASE_MASK|
			      GDK_BUTTON_PRESS_MASK|
			      GDK_BUTTON1_MOTION_MASK|GDK_BUTTON2_MOTION_MASK);
}

/*
 * Delete a wavepanel structure and all data structures referenced from it.
 */
void destroy_wave_panel(WavePanel *wp)
{
	VisibleWave *vw;

	while((vw = g_list_nth_data(wp->vwlist, 0)) != NULL) {
		remove_wave_from_panel(wp, vw);
	}
	gtk_widget_destroy(wp->lvbox);
	gtk_widget_destroy(wp->drawing);
	gdk_pixmap_unref(wp->pixmap);
	wp->valid = 0;
	scm_unprotect_object(wp->smob);
	if(wp->outstanding_smob == 0)
		g_free(wp);
}

/* global wtable: GtkTable widget for the main window. */

SCWM_PROC(wtable_startval_xval, "wtable-start-xval", 0, 0, 0, ())
  /** return the X coordinate represented by the left edge of the
displayed portion of the waveforms */
#define FUNC_NAME s_wtable_start_xval
{
	return gh_double2scm(wtable->start_xval);
}
#undef FUNC_NAME

SCWM_PROC(wtable_end_xval, "wtable-end-xval", 0, 0, 0, ())
  /** return the X coordinate represented by the right edge of the
displayed portion of the waveforms */
#define FUNC_NAME s_wtable_end_xval
{
	return gh_double2scm(wtable->end_xval);
}
#undef FUNC_NAME

SCWM_PROC(wtable_min_xval, "wtable-min-xval", 0, 0, 0, ())
  /** return the X coordinate represented by the left edge of the
displayed portion of the waveforms */
#define FUNC_NAME s_wtable_min_xval
{
	return gh_double2scm(wtable->min_xval);
}
#undef FUNC_NAME

SCWM_PROC(wtable_max_xval, "wtable-max-xval", 0, 0, 0, ())
  /** return the X coordinate represented by the right edge of the
displayed portion of the waveforms */
#define FUNC_NAME s_wtable_max_xval
{
	return gh_double2scm(wtable->max_xval);
}
#undef FUNC_NAME

SCWM_PROC(wtable_vcursor, "wtable-vcursor", 1, 0, 0, (cur))
/** return the X coordinate where vertical bar cursor N is located
 */
#define FUNC_NAME s_wtable_vcursor
{
	int icno;
	VALIDATE_ARG_INT_RANGE_COPY(1, cur, 0, 2, icno);
	if(!wtable->cursor[icno]->shown)
		return SCM_BOOL_F;
	return gh_double2scm(wtable->cursor[icno]->xval);
}
#undef FUNC_NAME


/* build the GtkTable widget for the main window.
 * side effect:
 *	creates wtable->table widget and adds the other widgets
 *	to it. 
 *	wtable->xlhbox, win_hsbar, and the panel widgets must already
 *	be created.
 */
void
wavewin_build_table()
{
	int i;

	wtable->table = gtk_table_new(wtable->npanels+2,2,FALSE);
/*	gtk_table_set_row_spacings(GTK_TABLE(wtable->table), 2); */
	gtk_table_set_col_spacings(GTK_TABLE(wtable->table), 4);
	gtk_widget_show(wtable->table);
	gtk_box_pack_start(GTK_BOX(wtable->vbox), wtable->table, TRUE, TRUE, 5);
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];

		gtk_table_attach(GTK_TABLE(wtable->table), wp->lvbox, 
			 0, 1, i, i+1, 
			 GTK_FILL, GTK_EXPAND|GTK_FILL, 0, 1);

		gtk_table_attach(GTK_TABLE(wtable->table), wp->drawing, 
			 1, 2, i, i+1, 
			 GTK_EXPAND|GTK_FILL, GTK_EXPAND|GTK_FILL, 0, 1);

	}
	gtk_table_attach(GTK_TABLE(wtable->table), wtable->xlhbox,
			 1, 2, wtable->npanels, wtable->npanels+1,
			 GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);

	gtk_table_attach(GTK_TABLE(wtable->table), win_hsbar,
			 1, 2, wtable->npanels+1, wtable->npanels+2,
			 GTK_EXPAND|GTK_FILL, GTK_FILL, 0, 0);

#ifndef GTK_V12
	/* can't set up dnd_drop on wavepanel drawing areas until it
	 * is connected to a window and realized so that it has
	 * an X-window.  At least I think that's the deal. */
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		gtk_widget_dnd_drop_set (wp->lvbox, TRUE,
					 accepted_drop_types, 1, FALSE);
	
		gtk_widget_dnd_drop_set (wp->drawing, TRUE,
					 accepted_drop_types, 1, FALSE);
	}
#endif
}

/*
 * delete waveform window's GtkTable Widget.
 * arranges so that the child widgets stay around so a new table
 * can be built with more or fewer panels.
 */
void
wavewin_destroy_table()
{
	int i;
	/* bump refcount on table's children so they don't get cleaned up */
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		gtk_widget_ref(wp->lvbox);
		gtk_container_remove(GTK_CONTAINER(wtable->table), wp->lvbox);
		gtk_widget_ref(wp->drawing);
		gtk_container_remove(GTK_CONTAINER(wtable->table),wp->drawing);
	}
	gtk_widget_ref(wtable->xlhbox);
	gtk_container_remove(GTK_CONTAINER(wtable->table), wtable->xlhbox);
	gtk_widget_ref(win_hsbar);
	gtk_container_remove(GTK_CONTAINER(wtable->table), win_hsbar);

	gtk_widget_destroy(wtable->table);
	wtable->table = NULL;
}       

/* remove the extra references to wtable's child widgets
 * that we had to make while rebuilding the table
 */
void
wavewin_finish_table_rebuild()
{
	int i;
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];

		gtk_widget_unref(wp->lvbox);
		gtk_widget_unref(wp->drawing);
	}
	gtk_widget_unref(wtable->xlhbox);
	gtk_widget_unref(win_hsbar);
}

SCWM_PROC(get_wavewin, "get-wavewin", 0, 0, 0, ())
  /** return the GtkWindow object for the main waveform window 
*/
#define FUNC_NAME s_get_wavewin
{
	return sgtk_wrap_gtkobj(GTK_OBJECT(win_main));
}
#undef FUNC_NAME

SCWM_PROC(get_wavewin_toolbar, "get-wavewin-toolbar", 0, 0, 0, ())
  /** return the GtkHBox object for horizontal box to contain
function buttons or icons in the main waveform window 
*/
#define FUNC_NAME s_get_wavewin_toolbar
{
	return sgtk_wrap_gtkobj(GTK_OBJECT(win_main_toolbar));
}
#undef FUNC_NAME

SCWM_PROC(get_wavewin_menubar, "get-wavewin-menubar", 0, 0, 0, ())
  /** return the GtkMenuBar object for menubar in the main waveform window 
*/
#define FUNC_NAME s_get_wavewin_menubar
{
	return sgtk_wrap_gtkobj(GTK_OBJECT(win_main_menubar));
}
#undef FUNC_NAME

/*
 * Construct main window and its widgets
 */
void setup_waveform_window(void)
{
	int i;
	GtkWidget *box0, *bbox;
	/* some size information. */
	const int min_w=80, min_h=50;

	/* Create a top-level window. Set the title and establish delete and
	   destroy event handlers. */
	win_main = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_name(win_main, prog_name);
	gtk_signal_connect(
		GTK_OBJECT(win_main), "destroy",
		GTK_SIGNAL_FUNC(destroy_handler), NULL);
	gtk_signal_connect(
		GTK_OBJECT(win_main), "delete_event",
		GTK_SIGNAL_FUNC(destroy_handler), NULL);

	/* create the vertical box, and add it to the window */
	box0 = gtk_vbox_new(FALSE, 0);
	gtk_container_add (GTK_CONTAINER (win_main), box0);
	gtk_widget_show(box0);

	win_main_menubar = gtk_menu_bar_new();
	gtk_widget_show(win_main_menubar);
	/* create_gwave_menu(); */
	gtk_box_pack_start(GTK_BOX(box0), win_main_menubar, FALSE, TRUE, 0);

	wtable->vbox = gtk_vbox_new(FALSE, 0);
	gtk_container_border_width (GTK_CONTAINER (wtable->vbox), 5);
	gtk_container_add (GTK_CONTAINER (box0), wtable->vbox);
	win_main_toolbar = create_toolbar();
	gtk_box_pack_start(GTK_BOX(wtable->vbox),
			   win_main_toolbar, FALSE, FALSE, 0);

	/* label with cursor status */
	win_status_label = gtk_label_new(" ");
	gtk_box_pack_start(GTK_BOX(wtable->vbox), win_status_label, FALSE, FALSE, 0);
	gtk_widget_show(win_status_label);

	/* set up WavePanels */
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		setup_wave_panel(wp, 0);
	}

	/* horizontal box for X-axis labels */
	wtable->xlhbox = create_xlabel_hbox();

	/* scrollbar */
	{
		double dwidth;
		dwidth = wtable->max_xval - wtable->min_xval;
	win_hsadj = (GtkAdjustment *)
		gtk_adjustment_new(wtable->start_xval, /* value */
				   wtable->min_xval, /* lower */
				   wtable->max_xval, /* upper */
				   dwidth/100,	/* step increment = 1% */
				   dwidth/2, 	/* page increment = 50% */
				   dwidth	/* page_size */
			);
	}
	win_hsbar = gtk_hscrollbar_new(GTK_ADJUSTMENT(win_hsadj));
	gtk_range_set_update_policy (GTK_RANGE (win_hsbar), 
			       GTK_UPDATE_CONTINUOUS);
	gtk_signal_connect(
		GTK_OBJECT(win_hsadj), "value_changed", 
		(GtkSignalFunc)scroll_handler, (gpointer)wtable);
	gtk_widget_show(win_hsbar);

	/* assemble wavepanels, label, and scrollbar into the table */
	wavewin_build_table();

	/* have to call hooks to build menu before doing 
	   gtk-widget-show on main window */
	call0_hooks(new_wavewin_hook);

	/* Show the top-level window, set its minimum size */
	gtk_widget_show(wtable->vbox);
	gtk_widget_show(win_main);
	gdk_window_set_hints(win_main->window, 0,0,  min_w, min_h, 0,0,
			     GDK_HINT_MIN_SIZE);
	wtable->button_down = -1;

}

/*
 * Delete and rebuild the GtkTable for the waveform window.
 * prototype for adding/deleting panels
 */
void
wavewin_rebuild_table()
{
	wavewin_destroy_table();
	/* change # of panels or just rearrange wtable->panels array here */
	wavewin_build_table();
	wavewin_finish_table_rebuild();
}

/*
 * Create new WavePanel before the specified panel, 
 * or at the end if no panel specified.
 */
void
wavewin_insert_panel(WavePanel *ppos, int ptype)
{
	int p, n;
	WavePanel **owp;
	int found = 0;

	wavewin_destroy_table();

	owp = wtable->panels;
	wtable->npanels++;
	wtable->panels = g_new0(WavePanel*, wtable->npanels);

	for(p = 0, n = 0; p < wtable->npanels - 1 ; p++) {
		if(ppos == owp[p]) {
			wtable->panels[n] = new_wave_panel();
			setup_wave_panel(wtable->panels[n], ptype);

			/* protect new widgets from unref needed on old widgets
			 * in finish_table_rebuild */
			gtk_widget_ref(wtable->panels[n]->lvbox);
			gtk_widget_ref(wtable->panels[n]->drawing);
			found = 1;
			n++;
		}
		wtable->panels[n++] = owp[p];
	}
	if(!found) { /* add at end */
		wtable->panels[n] = new_wave_panel();
		setup_wave_panel(wtable->panels[n], ptype);

		gtk_widget_ref(wtable->panels[n]->lvbox);
		gtk_widget_ref(wtable->panels[n]->drawing);
	}
	g_free(owp);
	wavewin_build_table();
	wavewin_finish_table_rebuild();
}



/*
 * Delete the specified WavePanel.
 */
void
wavewin_delete_panel(WavePanel *dwp)
{
	int i, p;
	WavePanel **nwp;
	if(wtable->npanels == 1) {
		fprintf(stderr, "cmd_delete_panel: can't delete last panel\n");
		return;
	}

	if(dwp == last_drop_wavepanel)
		last_drop_wavepanel = NULL;
	wavewin_destroy_table();

	nwp = g_new0(WavePanel*, wtable->npanels - 1);
	for(p = 0, i = 0; i < wtable->npanels; i++) {
		if(wtable->panels[i] == dwp) {
			destroy_wave_panel(wtable->panels[i]);
			dwp = NULL;
			wtable->panels[i] = NULL;
		} else {
			nwp[p++] = wtable->panels[i];
		}
	}
	if(dwp) {
		fprintf(stderr, "cmd_delete_panel: specified panel not found\n");
		/* some memory may have leaked */
	}
	g_free(wtable->panels);
	wtable->npanels--;
	wtable->panels = nwp;

	wavewin_build_table();
	wavewin_finish_table_rebuild();
}

SCWM_PROC(wtable_insert_panel_x, "wtable-insert-panel!", 2, 0, 0, 
	   (SCM wp, SCM type))
/** Add a new panel of type TYPE to the waveform display after panel WP, or
 at the end if WP is #f */
#define FUNC_NAME s_wtable_insert_panel_x
{
	WavePanel *cwp;
	int ptype;
	VALIDATE_ARG_WavePanel_COPY_USE_NULL(1,wp,cwp);
	VALIDATE_ARG_INT_RANGE_COPY(2,type,0,1,ptype);
	if(v_flag)
		fprintf(stderr, "wtable_insert_panel(0x%x)\n", cwp);
	wavewin_insert_panel(cwp, ptype);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(wtable_delete_panel_x, "wtable-delete-panel!", 1, 0, 0, (SCM wp))
/** Delete panel WP from the waveform display */
#define FUNC_NAME s_wtable_delete_panel_x
{
	WavePanel *cwp;
	VALIDATE_ARG_WavePanel_COPY(1,wp,cwp);
	if(v_flag)
		fprintf(stderr, "wtable_delete_panel(0x%x)\n", cwp);
	wavewin_delete_panel(cwp);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCWM_PROC(wavepanel_x2val, "wavepanel-x2val", 2, 0, 0,
	  (SCM wavepanel, SCM xpixel))
/** Given an XPIXEL coordinate in WAVEPANEL, 
 return the value of the independent variable at that position
 in the waveform.
*/
#define FUNC_NAME s_wavepanel_x2val
{
	WavePanel *wp;
	int x;
	double val;
	VALIDATE_ARG_WavePanel_COPY_USE_NULL(1,wavepanel,wp);
	VALIDATE_ARG_INT_COPY(2,xpixel,x);
	val = x2val(wp,x);
	return gh_double2scm(val);
}
#undef FUNC_NAME

SCWM_PROC(wavepanel_type, "wavepanel-type", 1, 0, 0,
	  (SCM wavepanel))
/** return the type of a WAVEPANEL, as an integer.
*/
#define FUNC_NAME s_wavepanel_type
{
	WavePanel *wp;
	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);
	return gh_int2scm(wp->ptype);
}
#undef FUNC_NAME

SCWM_PROC(set_wavepanel_type_x, "set-wavepanel-type!", 2, 0, 0,
	  (SCM wavepanel, SCM type))
/** change the type of WAVEPANEL to TYPE.
*/
#define FUNC_NAME s_set_wavepanel_type_x
{
	WavePanel *wp;
	int ptype;
	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);
	VALIDATE_ARG_INT_RANGE_COPY(2,type,0,1,ptype);
	set_wave_panel_type(wp, ptype);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/* 
 * this binding stuff is really quite a hack; we should build a common
 * mechanism for mouse and keyboard bindings, together with modifiers.
 * But the scheme interface here isn't too far from what that would implement,
 * and this simple thing lets us get the rest of the old menus into guile.
 */
SCWM_PROC(wavepanel_bind_mouse, "wavepanel-bind-mouse", 2, 0, 0,
           (SCM button, SCM proc))
     /** binds a mouse BUTTON to the procedure PROC in all wavepanels. 
      *  PROC is called with 1 argument, the wavepanel that the mouse was in
      */
#define FUNC_NAME s_wavepanel_bind_mouse
{
	int bnum;
	VALIDATE_ARG_INT_RANGE_COPY(1, button, 1, 5, bnum);
	VALIDATE_ARG_PROC(2, proc);

/* TODO: find right way to initialize and test to remove old binding.
  if(gh_procedure_p(wavepanel_mouse_binding[bnum])) {
                scm_unprotect_object(wavepanel_mouse_binding[bnum]);
        }
*/
	if (!UNSET_SCM(proc)) {
		scm_protect_object(proc);
		wavepanel_mouse_binding[bnum] = proc;
	}
		
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*****************************************************************************
 * Standard stuff for the WavePanel SMOB */

scm_sizet
free_WavePanel(SCM obj)
{
	WavePanel *wp = WavePanel(obj);
	wp->outstanding_smob = 0;

	if(wp->valid == 0) { /* if C has already invalidated, free it up */
		if(v_flag)
			fprintf(stderr, "free WavePanel 0x%x in gc\n", wp);
		g_free(wp);
		return sizeof(WavePanel);
	}
	else
		return 0;
}

SCM
mark_WavePanel(SCM obj)
{
	return SCM_BOOL_F;
}

int 
print_WavePanel(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
	scm_puts("#<WavePanel ", port);
	scm_intprint((long)WavePanel(obj), 16, port);
  	if(!WavePanel(obj)->valid)
		scm_puts("invalid", port);
	scm_putc('>', port);
	return 1;
}

SCWM_PROC(WavePanel_p, "WavePanel?", 1, 0, 0,
           (SCM obj))
     /** Returns #t if OBJ is a gwave data file object, otherwise #f. */
#define FUNC_NAME s_WavePanel_p
{
	return SCM_BOOL_FromBool(WavePanel_P(obj));
}
#undef FUNC_NAME

/*****************************************************************************
 * Standard stuff for the VisibleWave SMOB */

scm_sizet
free_VisibleWave(SCM obj)
{
	VisibleWave *wp = VisibleWave(obj);
	wp->outstanding_smob = 0;

	if(wp->valid == 0) { /* if C has already invalidated, free it up */
		if(v_flag)
			fprintf(stderr, "free VisibleWave 0x%x in gc\n", wp);
		g_free(wp);
		return sizeof(VisibleWave);
	}
	else
		return 0;
}

SCM
mark_VisibleWave(SCM obj)
{
	return SCM_BOOL_F;
}

int 
print_VisibleWave(SCM obj, SCM port, scm_print_state *ARG_IGNORE(pstate))
{
	scm_puts("#<VisibleWave ", port);
	scm_intprint((long)VisibleWave(obj), 16, port);
  	if(!VisibleWave(obj)->valid)
		scm_puts("invalid", port);
	scm_putc('>', port);
	return 1;
}

SCWM_PROC(VisibleWave_p, "VisibleWave?", 1, 0, 0,
           (SCM obj))
     /** Returns #t if OBJ is a gwave data file object, otherwise #f. */
#define FUNC_NAME s_VisibleWave_p
{
	return SCM_BOOL_FromBool(VisibleWave_P(obj));
}
#undef FUNC_NAME


/*********************************************************************** 
 * guile initialization 
 */

MAKE_SMOBFUNS(WavePanel);
MAKE_SMOBFUNS(VisibleWave);

void init_wavewin()
{
        REGISTER_SCWMSMOBFUNS(WavePanel);
        REGISTER_SCWMSMOBFUNS(VisibleWave);

#ifndef SCM_MAGIC_SNARFER
#include "wavewin.x"
#endif
}
