/*
 * wavepanel.c, part of the gwave waveform viewer tool
 *
 * Functions in this file handle wave panels.
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
#include <guile-gtk.h>

#include <config.h>
#include <scwm_guile.h>
#include <gwave.h>
#include <wavelist.h>
#include <wavewin.h>
#include <measurebtn.h>

#define WAVEPANEL_MIN_WIDTH 400
#define WAVEPANEL_MIN_HEIGHT 20
#define WAVEPANEL_MAX_REQHEIGHT 400

XSCM_HOOK(new_wavepanel_hook,"new-wavepanel-hook", 1, (SCM wp),
"This hook is invoked with one WavePanel argument, WP, when the
WavePanel is first created and added to the waveform window.
The main purpose of this hook is to allow creation of 
popup menus and other event bindings for the wavepanel.");

SCM wavepanel_mouse_binding[6];
WavePanel *last_drop_wavepanel;

/* generate VisibleWave button label string,
 * for both initial setup and updating.
 */
void
vw_get_label_string(char *buf, int buflen, VisibleWave *vw)
{
	GWDataFile *gdf;
	int n, l;

	gdf = vw->gdf;
	g_assert(gdf != NULL);

	l = buflen - strlen(gdf->ftag) - 10;
	n = MIN(l, 15);
	sprintf(buf, "%s: %.*s", gdf->ftag, n, vw->varname);
}

/*
 * vw_wp_create_button -- create button, label, and measurement
 * widgets for a new VisibleWave just added to a WavePanel
 */
void
vw_wp_create_button(VisibleWave *vw, WavePanel *wp)
{
	char lbuf[64];
	int newrow;

	vw_get_label_string(lbuf, 64, vw);
	vw->label = gtk_label_new(lbuf);
	vw->button = gtk_toggle_button_new();
	gtk_container_add(GTK_CONTAINER(vw->button), vw->label);

	/* create new row #1 of the label/measurement table,
	   and add this stuff there.  row 0 reserved for Y-axis label. */
	newrow = 1;
	gtk_table_insert_row(wp->lmtable, newrow);

	gtk_table_attach(GTK_TABLE(wp->lmtable), vw->button, 
			 0, 1, newrow, newrow+1,
			 GTK_EXPAND|GTK_FILL,
			 0,
			 0, 0 );

	sprintf(lbuf, "wavecolor%d", vw->colorn);
	gtk_widget_set_name(vw->label, lbuf);
	gtk_widget_show(vw->label);
	gtk_widget_set_name(vw->button, "wavebutton");
	gtk_widget_show(vw->button);

	/* create measurement buttons */
	vw->mbtn[0] = measure_button_new(vw->var, MBF_VARC0);
	vw->mbtn[1] = measure_button_new(vw->var, MBF_VARC1);

	gtk_widget_set_usize(vw->mbtn[0]->button, 60, -1);
	gtk_widget_set_usize(vw->mbtn[1]->button, 60, -1);

	gtk_table_attach(GTK_TABLE(wp->lmtable), vw->mbtn[0]->button,
			 1, 2, newrow, newrow+1,
			 GTK_FILL,
			 0,
			 0, 0 );

	gtk_table_attach(GTK_TABLE(wp->lmtable), vw->mbtn[1]->button,
			 2, 3, newrow, newrow+1,
			 GTK_FILL,
			 0,
			 0, 0 );

	mbtn_update(vw->mbtn[0], NULL);
	mbtn_update(vw->mbtn[1], NULL);
}

void
draw_wavepanel_labels(WavePanel *wp)
{
	if(wp->lab_min) {
		gtk_label_set(GTK_LABEL(wp->lab_min), val2txt(wp->start_yval,0));
	}
	if(wp->lab_max) {
		gtk_label_set(GTK_LABEL(wp->lab_max), val2txt(wp->end_yval,0));
	}
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
 * Construct label/measurement area on the left side of a WavePanel.
 * After revision, this will consist of a table containing 3 columns.
 * The top row (row 0) has a single hbox spanning 3 colums.  
 * It may be invisible, and conatains the LogY indicator and maximum Y value
 * label.
 * The bottom row (row N-1) has a single hbox spanning 3 columns,
 * with the minimum Y value label.
 * rows 1 through N-2 contain three columns, for the label and up to two 
 * measurements.
 * 
 */
void setup_wavepanel_lmtable(WavePanel *wp, int showlabels)
{
	char lbuf[128];
	GtkWidget *vbox;

	wp->lmtable = gtk_table_new(2, 3, FALSE);
	gtk_widget_set_usize(wp->lmtable, 220, -1);
	gtk_widget_show(wp->lmtable);

	wp->lab_max_hbox = gtk_hbox_new(FALSE, 0);
	gtk_table_attach(GTK_TABLE(wp->lmtable), wp->lab_max_hbox,
			 0, 3, 0, 1,
			 GTK_EXPAND|GTK_FILL,
			 0,
			 0, 0 );

	strcpy(lbuf, val2txt(wp->end_yval, 0));
	wp->lab_max = gtk_label_new(lbuf);
	gtk_box_pack_end(GTK_BOX(wp->lab_max_hbox), wp->lab_max,
			 FALSE, FALSE, 0);
	gtk_widget_show(wp->lab_max);

	wp->lab_logscale = gtk_label_new("LogY");
	gtk_box_pack_start(GTK_BOX(wp->lab_max_hbox), wp->lab_logscale,
			 FALSE, FALSE, 0);

	strcpy(lbuf, val2txt(wp->start_yval, 0));
	wp->lab_min = gtk_label_new(lbuf);
	gtk_widget_show(wp->lab_min);
	wp->lab_min_hbox = gtk_hbox_new(FALSE, 0);
	gtk_box_pack_end(GTK_BOX(wp->lab_min_hbox), wp->lab_min,
			 FALSE, FALSE, 0);
	vbox = gtk_vbox_new(FALSE, 0);
	gtk_widget_show(vbox);
	gtk_box_pack_end(GTK_BOX(vbox), wp->lab_min_hbox, FALSE, FALSE, 0);

	gtk_table_attach(GTK_TABLE(wp->lmtable), vbox,
			 0, 3, 1, 2,
			 GTK_EXPAND|GTK_FILL,
			 GTK_EXPAND|GTK_FILL,
			 0, 0 );
	if(showlabels) {
		gtk_widget_show(wp->lab_min_hbox);
		gtk_widget_show(wp->lab_max_hbox);
	}

}

/*
 * Set up widgets for a newly-created WavePanel -
 *    construct label and drawing areas
 */ 
void setup_wave_panel(WavePanel *wp, int minheight, int showlabels)
{
	if(v_flag) {
		fprintf(stderr, "setup_wave_panel minheight%d showlabels=%d\n",
			minheight, showlabels);
	}
	wp->start_xval = wtable->start_xval;
	wp->end_xval = wtable->end_xval;

	setup_wavepanel_lmtable(wp, showlabels);

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

	if(minheight < WAVEPANEL_MIN_HEIGHT)
		wp->req_height = WAVEPANEL_MIN_HEIGHT;
	else if(minheight > WAVEPANEL_MAX_REQHEIGHT)
		wp->req_height = WAVEPANEL_MAX_REQHEIGHT;
	else
		wp->req_height = minheight;

	gtk_drawing_area_size(GTK_DRAWING_AREA(wp->drawing), 
			      wp->width ? wp->width : WAVEPANEL_MIN_WIDTH,
			      wp->req_height);
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
	gtk_widget_destroy(wp->lmtable);
	gtk_widget_destroy(wp->drawing);
	gdk_pixmap_unref(wp->pixmap);
	wp->valid = 0;
	scm_unprotect_object(wp->smob);
	if(wp->outstanding_smob == 0)
		g_free(wp);
}

XSCM_DEFINE(wavepanel_y_zoom_x, "wavepanel-y-zoom!", 3, 0, 0, 
	   (SCM wavepanel, SCM miny, SCM maxy),
	   "zoom/rescale WAVEPANEL so that the y axis displays from MINY to MAXY")
#define FUNC_NAME s_wavepanel_y_zoom_x
{
	WavePanel *wp;
	double dmin, dmax;
	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);
	if(miny == SCM_BOOL_F) {
		wp->man_yzoom = 0;
		wp->start_yval = wp->min_yval;
		wp->end_yval = wp->max_yval;
	} else {
		VALIDATE_ARG_DBL_COPY(1, miny, dmin);
		VALIDATE_ARG_DBL_COPY(2, maxy, dmax);
		wp->man_yzoom = 1;
		if(dmin < dmax) {
			wp->start_yval = dmin;
			wp->end_yval = dmax;
		} else {
			wp->start_yval = dmax;
			wp->end_yval = dmin;
		}
	}
	draw_wavepanel(wp->drawing, NULL, wp);
	draw_wavepanel_labels(wp);
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

XSCM_DEFINE(wavepanel_y_manual_p, "wavepanel-y-manual?", 1, 0, 0,
	   (SCM wavepanel),
"If WAVEPANEL's y extents have been zoomed manually, return #t.
Otherwise, return #f to indicate automatic y-zoom to show the minimum
and maximum values of all dependent variables")
#define FUNC_NAME s_wavepanel_y_manual_p
{
	WavePanel *wp;
	int logy;
	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);

	if(wp->man_yzoom)
		return SCM_BOOL_T;
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME


XSCM_DEFINE(set_wavepanel_ylabels_visible_x, "set-wavepanel-ylabels-visible!", 2, 0, 0,
	   (SCM wavepanel, SCM show),
"If SHOW is #t, make the Y-axis labels on the left side of WAVEPANEL
visible.  If show is #f, hide the labels.  Hiding the labels allows
shrinking WAVEPANEL's height a little further.  This is useful when you have
a lot of panels, for example with digital circuits.")
#define FUNC_NAME s_set_wavepanel_ylabels_visible_x
{
	WavePanel *wp;
	int ishow;
	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);
	VALIDATE_ARG_BOOL_COPY(2, show, ishow);

	if(ishow) {
		gtk_widget_show(wp->lab_min_hbox);
		gtk_widget_show(wp->lab_max_hbox);
	} else {
		gtk_widget_hide(wp->lab_min_hbox);
		gtk_widget_hide(wp->lab_max_hbox);
	}
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

XSCM_DEFINE(set_wavepanel_ylogscale_x, "set-wavepanel-ylogscale!", 2, 0, 0,
	   (SCM wavepanel, SCM logscale),
"If LOGSCALE is #t, The Y-axis of WAVEPANEL is set to have
Logarithmic scaling.  Otherwise, scaling is linear.")
#define FUNC_NAME s_set_wavepanel_ylogscale_x
{
	WavePanel *wp;
	int logy;
	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);
	VALIDATE_ARG_BOOL_COPY(2, logscale, logy);
	
	if(wp->logy != logy) {
		wp->logy = logy;
		if(logy)
			gtk_widget_show(wp->lab_logscale);
		else
			gtk_widget_hide(wp->lab_logscale);
		draw_wavepanel(wp->drawing, NULL, wp);
	}
	return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

XSCM_DEFINE(wavepanel_ylogscale_p, "wavepanel-ylogscale?", 1, 0, 0,
	   (SCM wavepanel),
	   "If WAVEPANEL is set to Logarithmic scaling, return #t.")
#define FUNC_NAME s_wavepanel_ylogscale_p
{
	WavePanel *wp;
	int logy;
	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);

	if(wp->logy)
		return SCM_BOOL_T;
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM  /* Helper for wavepanel_visiblewaves */
wavepanel_to_scm(WavePanel *wp)
{
	return wp->smob;
}

XSCM_DEFINE(wavepanel_visiblewaves, "wavepanel-visiblewaves", 1, 0, 0,
	   (SCM wavepanel),
	   "Return a list of the VisibleWaves contained in WAVEPANEL.")
#define FUNC_NAME s_wavepanel_visiblewaves
{
	WavePanel *wp;

	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);

	return glist2scm(wp->vwlist, wavepanel_to_scm);
}
#undef FUNC_NAME


/* 
 * this binding stuff is really quite a hack; we should build a common
 * mechanism for mouse and keyboard bindings, together with modifiers.
 * But the scheme interface here isn't too far from what that would implement,
 * and this simple thing lets us get the rest of the old menus into guile.
 */
XSCM_DEFINE(wavepanel_bind_mouse, "wavepanel-bind-mouse", 2, 0, 0,
           (SCM button, SCM proc),
"binds a mouse BUTTON to the procedure PROC in all wavepanels. 
PROC is called with 1 argument, the wavepanel that the mouse was in.")
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

XSCM_DEFINE(wavepanel_x2val, "wavepanel-x2val", 2, 0, 0,
	   (SCM wavepanel, SCM xpixel),
"Given an XPIXEL coordinate in WAVEPANEL, 
return the value of the independent variable at that position
in the waveform.")
#define FUNC_NAME s_wavepanel_x2val
{
	WavePanel *wp;
	int x;
	double val;
	VALIDATE_ARG_WavePanel_COPY_USE_NULL(1,wavepanel,wp);
	VALIDATE_ARG_INT_COPY(2,xpixel,x);
	val = x2val(wp, x, wtable->logx);
	return gh_double2scm(val);
}
#undef FUNC_NAME

XSCM_DEFINE(wavepanel_y2val, "wavepanel-y2val", 2, 0, 0,
	   (SCM wavepanel, SCM ypixel),
"Given a YPIXEL screen-space coordinate in WAVEPANEL, 
return the value that the dependent variable would have
at that position.")
#define FUNC_NAME s_wavepanel_y2val
{
	WavePanel *wp;
	int y;
	double val;
	VALIDATE_ARG_WavePanel_COPY_USE_NULL(1,wavepanel,wp);
	VALIDATE_ARG_INT_COPY(2,ypixel,y);
	val = y2val(wp, y);
	return gh_double2scm(val);
}
#undef FUNC_NAME

XSCM_DEFINE(wavepanel_disp_rect, "wavepanel-disp-rect", 1, 0, 0,
	   (SCM wavepanel),
"Return a list containing coordinates of the space displayed 
currently displayed by the current zoom setting of WAVEPANEL.  
The list contains four elements, startX, startY, endX, endY")
#define FUNC_NAME s_wavepanel_disp_rect
{
	WavePanel *wp;
	SCM answer = SCM_EOL;
	VALIDATE_ARG_WavePanel_COPY_USE_NULL(1,wavepanel,wp);
	answer = scm_cons( gh_double2scm(wp->end_yval), answer);
	answer = scm_cons( gh_double2scm(wp->end_xval), answer);
	answer = scm_cons( gh_double2scm(wp->start_yval), answer);
	answer = scm_cons( gh_double2scm(wp->start_xval), answer);
	return answer;
}
#undef FUNC_NAME

XSCM_DEFINE(wavepanel_max_rect, "wavepanel-max-rect", 1, 0, 0,
	   (SCM wavepanel),
"Return a list containing coordinates of the bounding box of all waveforms
displayed in WAVEPANEL.
The list contains four elements, minX, minY, maxX, maxY")
#define FUNC_NAME s_wavepanel_max_rect
{
	WavePanel *wp;
	SCM answer = SCM_EOL;
	VALIDATE_ARG_WavePanel_COPY_USE_NULL(1,wavepanel,wp);
	answer = scm_cons( gh_double2scm(wp->max_yval), answer);
	answer = scm_cons( gh_double2scm(wp->max_xval), answer);
	answer = scm_cons( gh_double2scm(wp->min_yval), answer);
	answer = scm_cons( gh_double2scm(wp->min_xval), answer);
	return answer;
}
#undef FUNC_NAME

XSCM_DEFINE(set_wavepanel_minheight_x, "set-wavepanel-minheight!", 2, 0, 0,
	   (SCM wavepanel, SCM height),
"Set the minimum height of WAVEPANEL to HEIGHT pixels.  Adding multiple
VisibleWaves to the wavepanel can cause the actual height to increase
beyond this minimum, but it will never be smaller.")
#define FUNC_NAME s_set_wavepanel_minheight_x
{
	WavePanel *wp;
	int min_height;
	VALIDATE_ARG_WavePanel_COPY(1,wavepanel,wp);
	VALIDATE_ARG_INT_RANGE_COPY(2,height,WAVEPANEL_MIN_HEIGHT,400,min_height);
	wp->req_height = min_height;
	gtk_drawing_area_size(GTK_DRAWING_AREA(wp->drawing), 
			      wp->width ? wp->width : WAVEPANEL_MIN_WIDTH,
			      wp->req_height);

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

XSCM_DEFINE(WavePanel_p, "WavePanel?", 1, 0, 0,
           (SCM obj),
	   "Returns #t if OBJ is a gwave data file object, otherwise #f.")
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

XSCM_DEFINE(VisibleWave_p, "VisibleWave?", 1, 0, 0,
           (SCM obj),
	   "Returns #t if OBJ is a gwave data file object, otherwise #f.")
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

void init_wavepanel()
{
        REGISTER_SCWMSMOBFUNS(WavePanel);
        REGISTER_SCWMSMOBFUNS(VisibleWave);

#ifndef XSCM_MAGIC_SNARF_INITS
#include "wavepanel.x"
#endif
}
