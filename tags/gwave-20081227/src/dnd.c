/*
 * dnd.c - all of the stuff for the drag-and-drop interaction that gwave does.
 *
 * Copyright (C) 1998, 1999 Stephen G. Tell
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
 *
 * This used to include compatibility code for Gtk+ 1.0 and 1.2.
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

#include <config.h>
#include <gwave.h>
#include <wavelist.h>

#include "gtkmisc.h"


#define GWAVE_PRIVATE_DND_MAGIC	0xf00bbaad

enum {
  TARGET_STRING = 0,
  TARGET_DVAR = 1,
  TARGET_ROOTWIN = 2,
  TARGET_URL = 3
};

static GtkTargetEntry target_table[] = {
  { "x-gwave/dvar", GTK_TARGET_SAME_APP, TARGET_DVAR },
  { "STRING",     0, TARGET_STRING },
  { "text/plain", 0, TARGET_STRING },
  { "application/x-rootwin-drop", 0, TARGET_ROOTWIN }
};
static guint n_targets = sizeof(target_table) / sizeof(target_table[0]);

static GdkPixmap *drag_icon_pixmap;
static GdkPixmap *drag_mask_pixmap;

/*
 * set up for drag & drop.
 */
void
dnd_init(GtkWidget *window)
{
	static GtkWidget *drag_icon = NULL;
	static GtkWidget *drop_icon = NULL;

	if(!drag_icon_pixmap)
		drag_icon_pixmap = gdk_pixmap_colormap_create_from_xpm_d (NULL,
			  gtk_widget_get_colormap(GTK_WIDGET(window)),
			  &drag_mask_pixmap,
		          NULL, wave_drag_ok_xpm);
}

/****************************************************************************
 *
 * event handler for a drop onto the target, in our case
 * when variable gets dropped on a wavepanel
 */

void
dnd_target_event(GtkWidget          *widget,
		 GdkDragContext     *context,
		 gint                x,
		 gint                y,
		 GtkSelectionData   *data,
		 guint               info,
		 guint               time,
		 gpointer d)
{
	WavePanel *wp = (WavePanel *)d; 
	
	GWDnDData dd;
/*	g_print("dnd_target_event info=%d length=%d\n", info, data->length); */

	switch(info) {
	case TARGET_DVAR:
		dd = *(GWDnDData *)data->data;
		if(data->length == sizeof(GWDnDData) 
		 && dd.magic == GWAVE_PRIVATE_DND_MAGIC) {
/*			g_print("dnd_target_event recieved dv=0x%lx\n", dd.dv); */
			add_var_to_panel(wp, dd.dv);
		} else {
			g_print("dnd_target_event bad magic number %lx\n", 
				dd.magic);
		}
		gtk_drag_finish (context, TRUE, FALSE, time);
		break;
	case TARGET_STRING:
		/* eventually we may be able to do somthing useful with 
		 * a dropped string or other drop data types.
		 * For example dropping between multiple
		 * running gwaves, or dropping between a simulator 
		 * and gwave. 
		 */
		g_print("dnd_target_event: received \"%s\"\n",
			(gchar *)data->data);
		gtk_drag_finish (context, TRUE, FALSE, time);
	default:
		gtk_drag_finish (context, FALSE, FALSE, time);
	}
}


/*
 * Set up a widget as a drop target
 */
void
dnd_setup_target(GtkWidget *w, gpointer *d)
{
	gtk_drag_dest_set (w,
			   GTK_DEST_DEFAULT_DROP | GTK_DEST_DEFAULT_MOTION,
			   target_table, n_targets - 1,/*no rootwin*/
			   GDK_ACTION_COPY | GDK_ACTION_MOVE);
	
	gtk_signal_connect (GTK_OBJECT (w), 
			    "drag_data_received",
			    GTK_SIGNAL_FUNC(dnd_target_event), d);
}


/*****************************************************************************
 *									     
 * Prepare drag-and-drop data at the source, so the
 * underlying stuff can magicly transfer it to the target.
 */

void
dnd_source_data_get(GtkWidget          *widget,
		       GdkDragContext     *context,
		       GtkSelectionData   *selection_data,
		       guint               info,
		       guint               time,
		    gpointer d)
{
	WaveVar *dv = (WaveVar *)d;
	GWDnDData dd;
	GWDataFile *gdf;
	char buf[1024];

/*	g_print("dnd_source_data_get: type=%d\n", info); */

	switch(info) {
	case TARGET_ROOTWIN:
		/* maybe this could eventually mean somthing? */
/* 		g_print("wavevar %s dropped on the rootwin\n",
		   dv->wv_name);*/
		break;
	case TARGET_STRING:
		gdf = wvar_gwdatafile(dv);
		sprintf(buf, "%s;%s", 
			gdf->wf->wf_filename,
			dv->wv_name);
		
		gtk_selection_data_set (selection_data,
					selection_data->target,
					8, buf, strlen(buf));
		break;
	case TARGET_DVAR:
		dd.magic = GWAVE_PRIVATE_DND_MAGIC;
		dd.dv = dv;
/*		g_print("source_data_get: dv=%lx\n", dv); */
		gtk_selection_data_set (selection_data,
					selection_data->target,
					8, (gpointer)&dd, sizeof(GWDnDData));
		break;
	default:
		g_print("unknown target type\n");
	}
}


void
dnd_source_data_delete(GtkWidget          *widget,
			  GdkDragContext     *context,
			  gpointer            data)
{
	g_print("dnd_source_data_delete() called\n");
}


/*
 * Set up a widget as a drag source
 */
void
dnd_setup_source(GtkWindow *window, GtkWidget *w, WaveVar *dv)
{

	gtk_drag_source_set (w, GDK_BUTTON1_MASK,
			     target_table, n_targets, 
			     GDK_ACTION_COPY | GDK_ACTION_MOVE);
	gtk_drag_source_set_icon (w, 
				  gtk_widget_get_colormap(GTK_WIDGET(window)),
				  drag_icon_pixmap, drag_mask_pixmap);
		
	gtk_signal_connect (GTK_OBJECT(w), "drag_data_get",
			    GTK_SIGNAL_FUNC(dnd_source_data_get), 
			    (gpointer) dv);

	gtk_signal_connect (GTK_OBJECT(w), "drag_data_delete",
			    GTK_SIGNAL_FUNC (dnd_source_data_delete), NULL);

}

