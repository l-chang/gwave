/* measurebtn.h - 
 * declarations and definitions for the "measurebutton" functions
 * of the gwave waveform viewer
 *
 * Copyright 1998, 1999, 2000, 2001 Stephen G. Tell
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

/*
 * A measurebutton is simply a button containing a label that displays 
 * some kind of measurement result.   
 * The measurement function defines how that measurement is performed.  
 * Currently, measurement functions can involve up to a single waveform 
 * variable (WaveVar) and the vertical-bar cursor values.
 * 
 * We define a number of useful built-in measurement functions, and soon hope 
 * to allow user-defined measurement functions.
 */

struct _MeasureBtn {
	int measurefunc;
	WaveVar *var;		// note: might be NULL
	
	GtkWidget *button;
	GtkWidget *label;
};

#define MBF_NONE	0
#define MBF_CURSOR0	1
#define MBF_CURSOR1	2
#define MBF_CURSORDIFF	3
#define MBF_VARC0	4
#define MBF_VARC1	5
#define MBF_VARDIFF	6
#define MBF_RECIPCURDIFF	7
#define MBF_MAX_FUNC MBF_RECIPCURDIFF

extern MeasureBtn *measure_button_new(WaveVar *wv, int mfunc);
extern void mbtn_set_func(MeasureBtn *mbtn, int mfunc);
extern void mbtn_delete(MeasureBtn *mbtn);
extern void mbtn_hide(MeasureBtn *mbtn);
extern void mbtn_show(MeasureBtn *mbtn);
extern void mbtn_update(MeasureBtn *mbtn, gpointer *d);
extern void mbtn_update_var(MeasureBtn *mbtn, WaveVar *wv);
extern void mbtn_update_all();
