/*
 * measurebtn.c, part of the gwave waveform viewer tool
 *
 * Functions in this file handle "measurement buttons" - gui objects where
 * measurement results are displayed.
 *
 * Copyright (C) 1998, 1999, 2000, 2001 Stephen G. Tell.
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
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <gtk/gtk.h>
#include <guile-gtk.h>

#include <config.h>
#include <gwave.h>
#include <measurebtn.h>

GList *all_measure_buttons;

/* Allocate a new Measure Button and do basic setup */
MeasureBtn *
measure_button_new(WaveVar *wv, int mfunc)
{
	int need_var;
	char *tip = NULL;
	MeasureBtn *mbtn;
	GtkTooltips *gw_tooltips;

	mbtn = g_new0(MeasureBtn, 1);
	mbtn->measurefunc = mfunc;
	mbtn->var = wv;
	
	mbtn->label = gtk_label_new("");
	switch (mbtn->measurefunc) {
	case MBF_CURSOR0:
		gtk_widget_set_name(mbtn->label, "cursor0color");
		tip = "measure: cursor 0";
		need_var = 0;
		break;
		
	case MBF_CURSOR1:
		gtk_widget_set_name(mbtn->label, "cursor1color");
		tip = "measure: cursor 1";
		need_var = 0;
		break;
		
	case MBF_CURSORDIFF:
		gtk_widget_set_name(mbtn->label, "cursorDcolor");
		tip = "measure: cursor1 - cursor0";
		need_var = 0;
		break;
		
	case MBF_VARC0:
		gtk_widget_set_name(mbtn->label, "cursor0color");
		tip = "measure: variable(cursor0)";
		need_var = 1;
		break;
		
	case MBF_VARC1:
		gtk_widget_set_name(mbtn->label, "cursor1color");
		tip = "measure: variable(cursor0)";
		need_var = 1;
		break;

	case MBF_VARDIFF:
		gtk_widget_set_name(mbtn->label, "cursorDcolor");
		tip = "measure: variable(cursor1) - variable(cursor0)";
		need_var = 1;
		break;

	default:
		gtk_widget_set_name(mbtn->label, "cursor0color");
		need_var = 0;
		break;
		
	}
	if(need_var && !wv)
		g_error("new_MeasureBtn: measure function %d requires a WaveVar", mfunc);

	gtk_widget_show(mbtn->label);

	mbtn->button = gtk_button_new();
	gtk_widget_set_name(mbtn->button, "wavebutton");
	gtk_container_add(GTK_CONTAINER(mbtn->button), mbtn->label);

	all_measure_buttons = g_list_prepend(all_measure_buttons, mbtn);


	if(tip) {
		gw_tooltips = get_gwave_tooltips();
		gtk_tooltips_set_tip(GTK_TOOLTIPS(gw_tooltips), 
				     mbtn->button, tip, "");
	}

	return mbtn;
}

void
mbtn_delete(MeasureBtn *mbtn)
{
	gtk_widget_destroy(mbtn->button);  /* kills the child label also */
	all_measure_buttons = g_list_remove(all_measure_buttons, mbtn);

	g_free(mbtn);
}

void
mbtn_hide(MeasureBtn *mbtn)
{
	gtk_widget_hide(mbtn->button);
}

void
mbtn_show(MeasureBtn *mbtn)
{
	gtk_widget_show(mbtn->button);
}

void
mbtn_update(MeasureBtn *mbtn, gpointer *d)
{
	double csr_val, c2_val;
	double mvalue;
	int valid;
	
	valid = 0;
	switch (mbtn->measurefunc) {
	case MBF_CURSOR0:
		mvalue = wtable->cursor[0]->xval;
		valid = wtable->cursor[0]->shown;
		break;
		
	case MBF_CURSOR1:
		mvalue = wtable->cursor[1]->xval;
		valid = wtable->cursor[1]->shown;
		break;
		
	case MBF_CURSORDIFF:
		mvalue = wtable->cursor[1]->xval - 
			wtable->cursor[0]->xval;
		valid = wtable->cursor[0]->shown
			&& wtable->cursor[1]->shown;
		break;
		
	case MBF_VARC0:
		csr_val = wtable->cursor[0]->xval;
		valid = wtable->cursor[0]->shown 
			&& (mbtn->var->wv_iv->wds->min <= csr_val)
			&& (csr_val <= mbtn->var->wv_iv->wds->max);
		if(valid) 
			mvalue = wv_interp_value(mbtn->var, csr_val);
		break;
		
	case MBF_VARC1:
		csr_val = wtable->cursor[1]->xval;
		valid = wtable->cursor[1]->shown 
			&& (mbtn->var->wv_iv->wds->min <= csr_val)
			&& (csr_val <= mbtn->var->wv_iv->wds->max);
		if(valid) 
			mvalue = wv_interp_value(mbtn->var, csr_val);
		break;

	case MBF_VARDIFF:
		csr_val = wtable->cursor[0]->xval;
		c2_val = wtable->cursor[1]->xval;
		valid = wtable->cursor[0]->shown 
			&& (mbtn->var->wv_iv->wds->min <= csr_val)
			&& (csr_val <= mbtn->var->wv_iv->wds->max)
			&&   wtable->cursor[1]->shown 
			&& (mbtn->var->wv_iv->wds->min <= c2_val)
			&& (c2_val <= mbtn->var->wv_iv->wds->max);
		if(valid)
			mvalue = wv_interp_value(mbtn->var, c2_val) - 
				wv_interp_value(mbtn->var, csr_val);
		break;

	default:
		mvalue = 0.0;
		break;
		
	}

	/* automaticly hide/unhide as value becomes valid/invalid.
	 * we might not always want this behavior; add flag if not.
	 */

	if(valid) {
		if(!GTK_WIDGET_VISIBLE(mbtn->button))
			gtk_widget_show(mbtn->button);
		gtk_label_set(GTK_LABEL(mbtn->label), val2txt(mvalue, 0));
	} else {
		if(GTK_WIDGET_VISIBLE(mbtn->button))
			gtk_widget_hide(mbtn->button);
	}

}

void
mbtn_update_all()
{
	g_list_foreach(all_measure_buttons, (GFunc)mbtn_update, NULL);
}
