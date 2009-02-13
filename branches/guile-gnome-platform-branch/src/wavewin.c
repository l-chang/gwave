/*
 * wavewin.c, part of the gwave waveform viewer tool
 *
 * Functions in this file set up the main waveform window GUI.
 *
 * Copyright (C) 1998, 1999, 2000 Stephen G. Tell.
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
#include <guile-gnome-gobject/gobject.h>

#include <config.h>
#include <scwm_guile.h>
#include <gwave.h>
#include <wavelist.h>
#include <measurebtn.h>

#define WAVEWIN_IMPLEMENTATION
#include <wavewin.h>

#define WAVEPANEL_MIN_WIDTH 400
#define WAVEPANEL_MIN_HEIGHT 20
#define WAVEPANEL_MAX_REQHEIGHT 400

XSCM_HOOK(new_wavewin_hook,"new-wavewin-hook", 0, (),
"This hook is invoked with no arguments when the main waveform"
"window is first created."
"The main purpose of this hook is to allow creation of the"
"contents of the menubar and toolbar.");

/* create horizontal button box for top of main window */
GtkWidget *create_toolbar()
{
	GtkWidget *bbox;

	bbox = gtk_hbutton_box_new();
	gtk_button_box_set_layout(GTK_BUTTON_BOX(bbox), GTK_BUTTONBOX_START);
	gtk_button_box_set_spacing(GTK_BUTTON_BOX(bbox), 5);
	gtk_widget_show(bbox);
	return bbox;
}

/* horizontal box for X-axis labels */
GtkWidget *create_xlabel_hbox(WaveTable *wt)
{
	GtkWidget *hbox;
	hbox = gtk_hbox_new(FALSE, 0);
	wt->xlabel_left = gtk_label_new("0");
	gtk_box_pack_start(GTK_BOX(hbox), wt->xlabel_left, FALSE, FALSE, 0);
	gtk_widget_show(wt->xlabel_left);

	wt->xlabel_right = gtk_label_new("0");
	gtk_box_pack_end(GTK_BOX(hbox), wt->xlabel_right, FALSE, FALSE, 0);
	gtk_widget_show(wt->xlabel_right);

	wt->lab_xlogscale = gtk_label_new("LogX");
	gtk_box_pack_end(GTK_BOX(hbox), wt->lab_xlogscale, TRUE, FALSE, 0);

	gtk_widget_show(hbox);
	return hbox;
}

/* global wtable: GtkTable widget for the main window. */

SCM_DEFINE(wtable_start_xval, "wtable-start-xval", 0, 0, 0, (),
"Return the X coordinate represented by the left edge of the"
"displayed portion of the waveforms")
#define FUNC_NAME s_wtable_start_xval
{
	return scm_make_real(wtable->start_xval);
}
#undef FUNC_NAME

SCM_DEFINE(wtable_end_xval, "wtable-end-xval", 0, 0, 0, (),
"Return the X coordinate represented by the right edge of the"
"displayed portion of the waveforms")
#define FUNC_NAME s_wtable_end_xval
{
	return scm_make_real(wtable->end_xval);
}
#undef FUNC_NAME

SCM_DEFINE(wtable_min_xval, "wtable-min-xval", 0, 0, 0, (),
	   "return the minimum X coordinate of any displayed waveform")
#define FUNC_NAME s_wtable_min_xval
{
	return scm_make_real(wtable->min_xval);
}
#undef FUNC_NAME

SCM_DEFINE(wtable_max_xval, "wtable-max-xval", 0, 0, 0, (),
	   "return the maximum X coordinate of any displayed waveform")
#define FUNC_NAME s_wtable_max_xval
{
	return scm_make_real(wtable->max_xval);
}
#undef FUNC_NAME

SCM_DEFINE(wtable_vcursor, "wtable-vcursor", 1, 0, 0, (SCM cur),
	   "return the x coordinate where vertical bar cursor CUR is located")
#define FUNC_NAME s_wtable_vcursor
{
	int icno;
	VALIDATE_ARG_INT_RANGE_COPY(1, cur, 0, 2, icno);
	if(!wtable->cursor[icno]->shown)
		return SCM_BOOL_F;
	return scm_make_real(wtable->cursor[icno]->xval);
}
#undef FUNC_NAME

SCM_DEFINE(set_wtable_vcursor_x, "set-wtable-vcursor!", 2, 0, 0, 
	   (SCM cur, SCM x),
	   "Position vertical bar cursor number CUR at X")
#define FUNC_NAME s_set_wtable_vcursor_x
{
	int icno;
	double xval;
	VBCursor *csp;

	VALIDATE_ARG_INT_RANGE_COPY(1, cur, 0, 2, icno);
	csp = wtable->cursor[icno];
	VALIDATE_ARG_DBL_COPY(2, x, xval);
	if(xval < wtable->min_xval)
		xval = wtable->min_xval;
	if(xval > wtable->max_xval)
		xval = wtable->max_xval;

	update_cursor(csp, xval);

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

/*
 * Gtk+ signal handler called when main window scrolled panel table chanes size
 *	try to make the horizontal scrollbar the same width as the drawing
 *	area parts of the panel
 */
void
wavewin_ptable_size_handler(GtkWidget *w,
			       GtkRequisition *req, gpointer d)
{
	WaveTable *wt = (WaveTable *)d;
	int pwidth, boxwidth;

	pwidth = wt->panels[0]->drawing->allocation.width;
	boxwidth = wt->bot_vbox->requisition.width;

	if(gwave_debug) 
		printf("wavewin_ptable_size_handler: pwidth=%d boxwidth=%d\n",
		       pwidth, boxwidth);

	if(pwidth > 20 && pwidth != boxwidth)
		gtk_widget_set_usize(wt->bot_vbox, pwidth, -1);
}

/*
 * Gtk+ signal handler called when main window's scrollbar changes size.
 *	try to make the empty glue window the same size, to make everything line up.

 */
void
wavewin_ptablevsbar_sh_handler(GtkWidget *w,
			       gpointer d)
{
	WaveTable *wt = (WaveTable *)d;
	int pwidth, boxwidth;
	int vis;


	pwidth = w->allocation.width + 3;
	vis = GTK_WIDGET_VISIBLE(w);
	boxwidth = wt->bot_hbox3->allocation.width;

	if(gwave_debug) 
		printf("wavewin_ptablevsbar_sh_handler: pwidth=%d vis=%d boxwidth=%d\n",
		       pwidth, vis, boxwidth);

	if(vis) {
		if(!GTK_WIDGET_VISIBLE(wt->bot_hbox3))
			gtk_widget_show(wt->bot_hbox3);
		if(pwidth > 8 && pwidth != boxwidth)
			   gtk_widget_set_usize(wt->bot_hbox3, pwidth, -1);
	} else {
		if(GTK_WIDGET_VISIBLE(wt->bot_hbox3))
			gtk_widget_hide(wt->bot_hbox3);
	}
}

/* this is a hack and should go away: try to rejigger the bottom of
 * the window so things line up right.
 * I don't fully understand the gtk size-management stuff, but experimentaly
 * this seems to work.
 */
void 
wavewin_bot_fixup()
{
	WaveTable *wt = wtable;

	if(gwave_debug) printf("wavewin_bot_fixup()\n");

	if(wt) {
		wavewin_ptable_size_handler(NULL, NULL, (gpointer)wt);
		wavewin_ptablevsbar_sh_handler(GTK_SCROLLED_WINDOW(wt->vswindow)->vscrollbar,  (gpointer)wt);

		/* I don't know why this works, but without the next three
		   lines, things are still wrong after a window resize */
		gtk_main_iteration_do(0);
		wavewin_ptable_size_handler(NULL, NULL, (gpointer)wt);
		wavewin_ptablevsbar_sh_handler(GTK_SCROLLED_WINDOW(wt->vswindow)->vscrollbar,  (gpointer)wt);
	}
}


/* build the GtkTable widget for the main window.
 * side effect:
 *	creates wtable->table widget and adds the other widgets
 *	to it. 
 */
void
wavewin_build_table(WaveTable *wt)
{
	int i;

	wt->table = gtk_table_new(wtable->npanels,2,FALSE);
/*	gtk_table_set_row_spacings(GTK_TABLE(wtable->table), 2); */
	gtk_table_set_col_spacings(GTK_TABLE(wt->table), 4);
	gtk_widget_show(wt->table);
	for(i = 0; i < wt->npanels; i++) {
		WavePanel *wp = wt->panels[i];

		gtk_table_attach(GTK_TABLE(wt->table), wp->lmvbox, 
			 0, 1, i, i+1, 
			 GTK_FILL, GTK_EXPAND|GTK_FILL, 0, 1);

		gtk_table_attach(GTK_TABLE(wt->table), wp->drawing, 
			 1, 2, i, i+1, 
			 GTK_EXPAND|GTK_FILL, GTK_EXPAND|GTK_FILL, 0, 1);

	}

	gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(wt->vswindow),
					      wt->table);
	gtk_container_set_focus_vadjustment(
		GTK_CONTAINER (wt->table),
		gtk_scrolled_window_get_vadjustment(GTK_SCROLLED_WINDOW(wt->vswindow)));

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
		gtk_widget_ref(wp->lmvbox);
		gtk_container_remove(GTK_CONTAINER(wtable->table), wp->lmvbox);
		gtk_widget_ref(wp->drawing);
		gtk_container_remove(GTK_CONTAINER(wtable->table),wp->drawing);
	}
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

		gtk_widget_unref(wp->lmvbox);
		gtk_widget_unref(wp->drawing);
	}
}

SCM_DEFINE(get_wavewin, "get-wavewin", 0, 0, 0, (),
	   "Return the GtkWindow object for the main waveform window.")
#define FUNC_NAME s_get_wavewin
{
	return scm_c_gtype_instance_to_scm(GTK_OBJECT(wtable->window));
}
#undef FUNC_NAME

SCM_DEFINE(get_wavewin_toolbar, "get-wavewin-toolbar", 0, 0, 0, (),
"Return the GtkHBox object for horizontal box to contain"
"function buttons or icons in the main waveform window")
#define FUNC_NAME s_get_wavewin_toolbar
{
	return scm_c_gtype_instance_to_scm(GTK_OBJECT(wtable->toolbar));
}
#undef FUNC_NAME

SCM_DEFINE(get_wavewin_menubar, "get-wavewin-menubar", 0, 0, 0, (),
	   "return the GtkMenuBar object for menubar in the main waveform window")
#define FUNC_NAME s_get_wavewin_menubar
{
	return scm_c_gtype_instance_to_scm(GTK_OBJECT(wtable->menubar));
}
#undef FUNC_NAME


/* construct row of x-axis measurement boxes
 */
static GtkWidget *build_xmeasure_hbox(WaveTable *wt)
{
	GtkWidget *xmhbox = gtk_hbox_new(FALSE, 0);

	wt->cursor_mbtn[3] = measure_button_new(NULL, MBF_RECIPCURDIFF);
	gtk_box_pack_end(GTK_BOX(xmhbox),
			 wt->cursor_mbtn[3]->button,  FALSE, FALSE, 0);
 
	wt->cursor_mbtn[2] = measure_button_new(NULL, MBF_CURSORDIFF);
	gtk_box_pack_end(GTK_BOX(xmhbox),
			   wt->cursor_mbtn[2]->button,  FALSE, FALSE, 0);

	wt->cursor_mbtn[1] = measure_button_new(NULL, MBF_CURSOR1);
	gtk_box_pack_end(GTK_BOX(xmhbox),
			   wt->cursor_mbtn[1]->button,  FALSE, FALSE, 0);

	wt->cursor_mbtn[0] = measure_button_new(NULL, MBF_CURSOR0);
	gtk_box_pack_end(GTK_BOX(xmhbox),
			   wt->cursor_mbtn[0]->button, FALSE, FALSE, 0);

	return xmhbox;
}

/*
 * Construct main window and its widgets
 */
void setup_waveform_window(void)
{
	int i;
	GtkWidget *box0;
	GtkWidget *hbox1, *hbox2;
	GtkWidget *w;

	/* some size information. */
	const int min_w=450, min_h=220;

	/* Create a top-level window. Set the title and establish delete and
	   destroy event handlers. */
	wtable->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_widget_set_name(wtable->window, prog_name);
	gtk_signal_connect(
		GTK_OBJECT(wtable->window), "destroy",
		GTK_SIGNAL_FUNC(destroy_handler), NULL);
	gtk_signal_connect(
		GTK_OBJECT(wtable->window), "delete_event",
		GTK_SIGNAL_FUNC(destroy_handler), NULL);

	/* create the vertical box, and add it to the window */
	box0 = gtk_vbox_new(FALSE, 0);
	gtk_container_add(GTK_CONTAINER(wtable->window), box0);
	gtk_widget_show(box0);

	wtable->menubar = gtk_menu_bar_new();
	gtk_widget_show(wtable->menubar);
	gtk_box_pack_start(GTK_BOX(box0), wtable->menubar, FALSE, TRUE, 0);

	/* table that structures the rest of the window */
	wtable->ftable = gtk_table_new(4, 1, FALSE);
	gtk_container_border_width (GTK_CONTAINER (wtable->ftable), 5);
	gtk_container_add (GTK_CONTAINER (box0), wtable->ftable);

	wtable->toolbar = create_toolbar();
	gtk_table_attach(GTK_TABLE(wtable->ftable), wtable->toolbar, 
			 0, 1, WTABLE_FTR_TOOLBAR, WTABLE_FTR_TOOLBAR+1,
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	/* x measurement bar:  the X-cursor-related measurebuttons */
	wtable->xmeasure_hbox = build_xmeasure_hbox(wtable);
	gtk_widget_show(wtable->xmeasure_hbox);
	gtk_table_attach(GTK_TABLE(wtable->ftable), wtable->xmeasure_hbox, 
			 0, 1, WTABLE_FTR_XMHBOX, WTABLE_FTR_XMHBOX+1,
			 GTK_EXPAND|GTK_FILL, 0, 0, 1);


	// scrolled window with vertical scrollbar for panel-table
	wtable->vswindow = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(wtable->vswindow),
                                  GTK_POLICY_NEVER, 
                                  GTK_POLICY_AUTOMATIC);
	gtk_scrolled_window_set_placement(GTK_SCROLLED_WINDOW(wtable->vswindow), GTK_CORNER_TOP_LEFT);
	GTK_WIDGET_UNSET_FLAGS (GTK_SCROLLED_WINDOW(wtable->vswindow)->vscrollbar, GTK_CAN_FOCUS);
	gtk_widget_show(wtable->vswindow);
//	gtk_widget_set_usize(wtable->ftable, -1, min_h);

	gtk_signal_connect(GTK_OBJECT(wtable->vswindow), "size-allocate", 
			   GTK_SIGNAL_FUNC(wavewin_ptable_size_handler),
			   (gpointer)wtable);

	gtk_table_attach(GTK_TABLE(wtable->ftable), wtable->vswindow,
			 0, 1, WTABLE_FTR_PTABLE, WTABLE_FTR_PTABLE+1,
			 GTK_EXPAND|GTK_FILL,GTK_EXPAND|GTK_FILL, 0, 0);

	/* set up initial WavePanels - remnant to be removed someday */
	for(i = 0; i < wtable->npanels; i++) {
		WavePanel *wp = wtable->panels[i];
		setup_wave_panel(wp, 0, 0);
	}

	/* all the stuff at the bottom */
	hbox1 = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(hbox1);
	gtk_table_attach(GTK_TABLE(wtable->ftable), hbox1, 0, 1, 
			 WTABLE_FTR_BHBOX, WTABLE_FTR_BHBOX+1,
			 GTK_EXPAND|GTK_FILL, 0, 0, 0);

	hbox2 = gtk_hbox_new(FALSE, 0);
	gtk_widget_show(hbox2);
	gtk_box_pack_start(GTK_BOX(hbox1), hbox2, TRUE, TRUE, 0);

	wtable->bot_vbox = gtk_vbox_new(FALSE, 0);
	gtk_widget_show(wtable->bot_vbox);
	gtk_box_pack_start(GTK_BOX(hbox1), wtable->bot_vbox, FALSE, FALSE, 0);

	wtable->bot_hbox3 = gtk_hbox_new(FALSE, 0);
	if(gwave_debug) {
		w = gtk_label_new("@");
		gtk_widget_show(w);
		gtk_box_pack_start(GTK_BOX(wtable->bot_hbox3), w, FALSE, FALSE, 0);
	}
	gtk_widget_show(wtable->bot_hbox3);
	gtk_box_pack_start(GTK_BOX(hbox1), wtable->bot_hbox3, FALSE, FALSE, 0);

	gtk_signal_connect(GTK_OBJECT(GTK_SCROLLED_WINDOW(wtable->vswindow)->vscrollbar),
			   "show", 
			   GTK_SIGNAL_FUNC(wavewin_ptablevsbar_sh_handler), (gpointer)wtable);
	gtk_signal_connect(GTK_OBJECT(GTK_SCROLLED_WINDOW(wtable->vswindow)->vscrollbar),
			   "hide", 
			   GTK_SIGNAL_FUNC(wavewin_ptablevsbar_sh_handler), (gpointer)wtable);

		/* horizontal box for X-axis labels */
	wtable->xlhbox = create_xlabel_hbox(wtable);
	gtk_box_pack_start(GTK_BOX(wtable->bot_vbox), wtable->xlhbox,
			   FALSE, FALSE, 0);

		/* horizontal scrollbar */
	wtable->hsadj = (GtkAdjustment *)
 		gtk_adjustment_new(0.0, /* value */
 				   0.0, /* lower */
 				   1.0, /* upper */
 				   1.0/100.,	/* step increment = 1% */
 				   1.0/2., 	/* page increment = 50% */
 				   1.0		/* page_size */
			);
	wtable->hsbar = gtk_hscrollbar_new(GTK_ADJUSTMENT(wtable->hsadj));
	gtk_range_set_update_policy (GTK_RANGE (wtable->hsbar), 
			       GTK_UPDATE_CONTINUOUS);
	gtk_signal_connect(
		GTK_OBJECT(wtable->hsadj), "value_changed", 
		(GtkSignalFunc)scroll_handler, (gpointer)wtable);
	gtk_widget_show(wtable->hsbar);
	gtk_box_pack_start(GTK_BOX(wtable->bot_vbox), wtable->hsbar,
			   FALSE, FALSE, 0);

	/* assemble wavepanels into the table */
	wavewin_build_table(wtable);

	/* have to call hooks to build menu before doing 
	   gtk-widget-show on main window */
	call0_hooks(new_wavewin_hook);

	/* Show the top-level window, set its minimum size */
	gtk_widget_show(wtable->ftable);
	gtk_widget_show(wtable->window);
	gtk_window_set_default_size(GTK_WINDOW(wtable->window),
				    min_w, min_h*3/2);
	gdk_window_set_hints(wtable->window->window, 0,0,  min_w, min_h, 0,0,
			     GDK_HINT_MIN_SIZE);
	wtable->button_down = -1;

	gtk_signal_connect(
		GTK_OBJECT(wtable->window), 
//		"configure-event", 
		"size-allocate", 
		(GtkSignalFunc)wavewin_bot_fixup, (gpointer)wtable);
}

/*
 * Create new WavePanel before the specified panel, 
 * or at the end if no panel specified.
 */
void
wavewin_insert_panel(WavePanel *ppos, int minheight, int showlabels)
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
			setup_wave_panel(wtable->panels[n], minheight, showlabels);

			/* protect new widgets from unref needed on old widgets
			 * in finish_table_rebuild */
			gtk_widget_ref(wtable->panels[n]->lmvbox);
			gtk_widget_ref(wtable->panels[n]->drawing);
			found = 1;
			n++;
		}
		wtable->panels[n++] = owp[p];
	}
	if(!found) { /* add at end */
		wtable->panels[n] = new_wave_panel();
		setup_wave_panel(wtable->panels[n], minheight, showlabels);

		gtk_widget_ref(wtable->panels[n]->lmvbox);
		gtk_widget_ref(wtable->panels[n]->drawing);
	}
	g_free(owp);
	wavewin_build_table(wtable);
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

	wavewin_build_table(wtable);
	wavewin_finish_table_rebuild();
}

WavePanel *
first_selected_wavepanel()
{
	int i;
	for(i = wtable->npanels-1; i >= 0; i--) {
		WavePanel *wp = wtable->panels[i];
		if(wp->selected)
			return wp;
	}
	return NULL;
}

SCM_DEFINE(wtable_insert_panel_x, "wtable-insert-panel!", 2, 1, 0, 
	   (SCM wp, SCM minheight, SCM showlabels),
"Add a new panel after the existing panel WP, or"
"at the end if WP is #f."
"The new panel has minimum height MINHEIGHT and has visible y-labels"
"unless SHOWLABELS is #f")
#define FUNC_NAME s_wtable_insert_panel_x
{
	WavePanel *cwp;
	int iheight, ishow; 
	VALIDATE_ARG_WavePanel_COPY_USE_NULL(1,wp,cwp);
	VALIDATE_ARG_INT_RANGE_COPY(2,minheight,
				    WAVEPANEL_MIN_HEIGHT,
				    WAVEPANEL_MAX_REQHEIGHT, iheight);
	VALIDATE_ARG_BOOL_COPY_USE_T(3,showlabels,ishow);
	if(v_flag)
		fprintf(stderr, "wtable_insert_panel(0x%x)\n", cwp);
	wavewin_insert_panel(cwp, iheight, ishow);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(wtable_delete_panel_x, "wtable-delete-panel!", 1, 0, 0, (SCM wp),
"Delete panel WP from the waveform display")
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

SCM_DEFINE(wtable_set_xlogscale_x, "wtable-set-xlogscale!", 1, 0, 0,
	   (SCM xlogscale),
	   "Set scaling for all X axes; logarithmic if XLOGSCALE is #t, else linear")
#define FUNC_NAME s_wtable_set_xlogscale_x
{
	int logx;
	VALIDATE_ARG_BOOL_COPY(1,xlogscale,logx);

	if(wtable->logx != logx) {
		wtable->logx = logx;
		if(logx) {
			gtk_widget_show(wtable->lab_xlogscale);
		} else {
			gtk_widget_hide(wtable->lab_xlogscale);
		}
		wtable_redraw_x();
	}

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(wtable_xlogscale_p, "wtable-xlogscale?", 0, 0, 0, (),
	   "If the X axis is set to Logarithmic scaling, return #t.")
#define FUNC_NAME s_wtable_xlogscale_p
{
	if(wtable->logx)
		return SCM_BOOL_T;
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE(wtable_wavepanels, "wtable-wavepanels", 0, 0, 0,
	   (),
	   "Return list of WavePanels that are currently displayed")
#define FUNC_NAME s_wtable_wavepanels
{
	int i;
	SCM answer;
	answer = SCM_EOL;
	for(i = wtable->npanels-1; i >= 0; i--) {
		WavePanel *wp = wtable->panels[i];
		answer = scm_cons(wp->smob, answer); 
	}
	return answer;
}
#undef FUNC_NAME

SCM_DEFINE(set_wtable_measure_x, "set-wtable-measure!", 2, 0, 0, 
	   (SCM n, SCM func),
 "Change the global measurement box numbered N (0 through 3)"
"to display the result of the measurement function FUNC")
#define FUNC_NAME s_set_wtable_measure_x
{
	int mno;
	int mfunc;
	VALIDATE_ARG_INT_RANGE_COPY(1, n, 0, N_WTABLE_MBTNS-1, mno);
	VALIDATE_ARG_INT_RANGE_COPY(2, func, 0, MBF_MAX_FUNC, mfunc);

	mbtn_set_func(wtable->cursor_mbtn[mno], mfunc);
	mbtn_update(wtable->cursor_mbtn[mno], NULL);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE(wtable_bottom_fixup, "wtable-bottom-fixup", 0, 0, 0,
	   (),
	   "Attempt to fix up sizing/positioning of bottom of window")
#define FUNC_NAME s_wtable_bottom_fixup
{
	wavewin_bot_fixup(wtable);

	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



SCM_DEFINE(set_wtable_tooltips_x, "set-wtable-tooltips!", 1, 0, 0, 
	   (SCM tt),
 "Use tooltips group TT when adding pop-up tool tips to"
 "widgets within the wave table")
#define FUNC_NAME s_set_wtable_tooltips_x
{
	GtkTooltips *gtt;

	SCM_VALIDATE_GOBJECT_COPY(1, tt, gtt);

	if(!GTK_IS_TOOLTIPS(gtt)) {
		printf("set_wtable_tooltips!: not a tooltips %lx\n",  gtt);
	}
	wtable->ttips = gtt;
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*********************************************************************** 
 * guile initialization 
 */

void init_wavewin()
{
#ifndef SCM_MAGIC_SNARF_INITS
#include "wavewin.x"
#endif
}
