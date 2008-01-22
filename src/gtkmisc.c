/* gtkmisc.c - 
 * misc. generic helper routines for Gtk+, that aren't at all specfic to
 * this particular application. - Steve Tell
 * May have been borrowed from elsewhere.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <gtk/gtk.h>
#include <guile-gnome-gobject/gobject.h>
#include <scwm_guile.h>

/* A couple of routines to make it easier to build menus.
 * These are by Steve Tell.
 *
 *
 * Create a (sub)menu and the item that activates it.
 * Returns GtkMenu widget pointer.
 */
GtkWidget *
create_menu(char *label, GtkWidget *parent)
{
	GtkWidget *item, *menu;
	
	if(label)
		item = gtk_menu_item_new_with_label(label);
	else
		item = gtk_menu_item_new();

	menu = gtk_menu_new();
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), menu);

	if(parent) {
		if(GTK_IS_MENU_BAR(parent))
			gtk_menu_bar_append (GTK_MENU_BAR (parent), item);
		else if(GTK_IS_MENU(parent))
			gtk_menu_append(GTK_MENU(parent), item);
	}
	gtk_widget_show(item);
	return menu;
}

/*
 * helper function for making menu items.  Returns menu item widget pointer,
 * but it can often be ignored, since the item is already added to the parent.
 */
GtkWidget *
create_menuitem(char *label, GtkWidget *parent, GtkSignalFunc action, 
		gpointer p)
{
	GtkWidget *item;
	if(label)
		item = gtk_menu_item_new_with_label(label);
	else
		item = gtk_menu_item_new();

	if(action)
		gtk_signal_connect (GTK_OBJECT (item), "activate", action, p);
	if(parent)
		gtk_menu_append (GTK_MENU(parent), item);
	gtk_widget_show (item);
	return item;
}


/* these routines were borrowed from testgtk.c:
 *	shape_pressed
 *	shape_released
 *	shape_motion
 *	shape_create_icon
 *
 * I created shape_create_icon_d by making a trivial change to a copy
 * of shape_create_icon, and hearby disclaim all rights to that 
 * derivative work. - sgt.
 *
 *
 * Original copyright message from testgtk.c follows:
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 */


/*
 * Shaped Windows
 */
static GdkWindow *root_win = NULL;

typedef struct _cursoroffset {gint x,y;} CursorOffset;

static void
shape_pressed (GtkWidget *widget, GdkEventButton *event)
{
  CursorOffset *p;

  /* ignore double and triple click */
  if (event->type != GDK_BUTTON_PRESS)
    return;

  p = gtk_object_get_user_data (GTK_OBJECT(widget));
  p->x = (int) event->x;
  p->y = (int) event->y;

  gtk_grab_add (widget);
  gdk_pointer_grab (widget->window, TRUE,
		    GDK_BUTTON_RELEASE_MASK |
		    GDK_BUTTON_MOTION_MASK |
		    GDK_POINTER_MOTION_HINT_MASK,
		    NULL, NULL, 0);
}


static void
shape_released (GtkWidget *widget)
{
  gtk_grab_remove (widget);
  gdk_pointer_ungrab (0);
}

static void
shape_motion (GtkWidget      *widget, 
	      GdkEventMotion *event)
{
  gint xp, yp;
  CursorOffset * p;
  GdkModifierType mask;

  p = gtk_object_get_user_data (GTK_OBJECT (widget));

  /*
   * Can't use event->x / event->y here 
   * because I need absolute coordinates.
   */
  gdk_window_get_pointer (root_win, &xp, &yp, &mask);
  gtk_widget_set_uposition (widget, xp  - p->x, yp  - p->y);
}

GtkWidget *
shape_create_icon (char     *xpm_file,
		   gint      x,
		   gint      y,
		   gint      px,
		   gint      py,
		   gint      window_type)
{
  GtkWidget *window;
  GtkWidget *pixmap;
  GtkWidget *fixed;
  CursorOffset* icon_pos;
  GdkGC* gc;
  GdkBitmap *gdk_pixmap_mask;
  GdkPixmap *gdk_pixmap;
  GtkStyle *style;

  style = gtk_widget_get_default_style ();
  gc = style->black_gc;	

  /*
   * GDK_WINDOW_TOPLEVEL works also, giving you a title border
   */
  window = gtk_window_new (window_type);
  
  fixed = gtk_fixed_new ();
  gtk_widget_set_usize (fixed, 100,100);
  gtk_container_add (GTK_CONTAINER (window), fixed);
  gtk_widget_show (fixed);
  
  gtk_widget_set_events (window, 
			 gtk_widget_get_events (window) |
			 GDK_BUTTON_MOTION_MASK |
			 GDK_POINTER_MOTION_HINT_MASK |
			 GDK_BUTTON_PRESS_MASK);

  gtk_widget_realize (window);
  gdk_pixmap = gdk_pixmap_create_from_xpm (window->window, &gdk_pixmap_mask, 
					   &style->bg[GTK_STATE_NORMAL],
					   xpm_file);

  pixmap = gtk_pixmap_new (gdk_pixmap, gdk_pixmap_mask);
  gtk_fixed_put (GTK_FIXED (fixed), pixmap, px,py);
  gtk_widget_show (pixmap);
  
  gtk_widget_shape_combine_mask (window, gdk_pixmap_mask, px,py);


  gtk_signal_connect (GTK_OBJECT (window), "button_press_event",
		      GTK_SIGNAL_FUNC (shape_pressed),NULL);
  gtk_signal_connect (GTK_OBJECT (window), "button_release_event",
		      GTK_SIGNAL_FUNC (shape_released),NULL);
  gtk_signal_connect (GTK_OBJECT (window), "motion_notify_event",
		      GTK_SIGNAL_FUNC (shape_motion),NULL);

  icon_pos = g_new (CursorOffset, 1);
  gtk_object_set_user_data(GTK_OBJECT(window), icon_pos);

  gtk_widget_set_uposition (window, x, y);
  gtk_widget_show (window);
  
  return window;
}


GtkWidget *
shape_create_icon_d (char     **xpm_data,
		     gint      x,
		     gint      y,
		     gint      px,
		     gint      py,
		     gint      window_type)
{
  GtkWidget *window;
  GtkWidget *pixmap;
  GtkWidget *fixed;
  CursorOffset* icon_pos;
  GdkGC* gc;
  GdkBitmap *gdk_pixmap_mask;
  GdkPixmap *gdk_pixmap;
  GtkStyle *style;

  style = gtk_widget_get_default_style ();
  gc = style->black_gc;	

  /*
   * GDK_WINDOW_TOPLEVEL works also, giving you a title border
   */
  window = gtk_window_new (window_type);
  
  fixed = gtk_fixed_new ();
  gtk_widget_set_usize (fixed, 100,100);
  gtk_container_add (GTK_CONTAINER (window), fixed);
  gtk_widget_show (fixed);
  
  gtk_widget_set_events (window, 
			 gtk_widget_get_events (window) |
			 GDK_BUTTON_MOTION_MASK |
			 GDK_POINTER_MOTION_HINT_MASK |
			 GDK_BUTTON_PRESS_MASK);

  gtk_widget_realize (window);
  gdk_pixmap = gdk_pixmap_create_from_xpm_d (window->window, &gdk_pixmap_mask, 
					   &style->bg[GTK_STATE_NORMAL],
					   xpm_data);

  pixmap = gtk_pixmap_new (gdk_pixmap, gdk_pixmap_mask);
  gtk_fixed_put (GTK_FIXED (fixed), pixmap, px,py);
  gtk_widget_show (pixmap);
  
  gtk_widget_shape_combine_mask (window, gdk_pixmap_mask, px,py);


  gtk_signal_connect (GTK_OBJECT (window), "button_press_event",
		      GTK_SIGNAL_FUNC (shape_pressed),NULL);
  gtk_signal_connect (GTK_OBJECT (window), "button_release_event",
		      GTK_SIGNAL_FUNC (shape_released),NULL);
  gtk_signal_connect (GTK_OBJECT (window), "motion_notify_event",
		      GTK_SIGNAL_FUNC (shape_motion),NULL);

  icon_pos = g_new (CursorOffset, 1);
  gtk_object_set_user_data(GTK_OBJECT(window), icon_pos);

  gtk_widget_set_uposition (window, x, y);
  gtk_widget_show (window);
  
  return window;
}

SCM_DEFINE(gtk_tooltips_enabled_p, "gtk-tooltips-enabled?", 1, 0, 0, (SCM tt),
 "Return #t if the GtkTooltips object TT is enabled, otherwise"
"return #f.  See gtk-tooltips-enable in the guile-gtk documentation,"
"or gtk_tooltips_enable in the Gtk+ documentation for GtkTooltips.")
#define FUNC_NAME s_gtk_tooltips_enabled_p
{
	GObject *gobj;
	GtkTooltips *gtktt;


//	VALIDATE_ARG_GTK_COPY(1, tt, GTK_TYPE_TOOLTIPS, gtktt);
	SCM_VALIDATE_GOBJECT_COPY(1, tt, gobj);
	gtktt = GTK_TOOLTIPS(gobj);

	if(gtktt->enabled)
		return SCM_BOOL_T;
	else
		return SCM_BOOL_F;
}
#undef FUNC_NAME


/* guile initialization */
void init_gtkmisc()
{
#ifndef XSCM_MAGIC_SNARF_INITS
#include "gtkmisc.x"
#endif
}
