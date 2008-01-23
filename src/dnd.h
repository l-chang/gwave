/*
 * dnd.h - definitions for drag-and-drop convenience functions
 *
 * Copyright (C) 2007 Stephen G. Tell
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

#ifndef DND_H
#define DND_H

extern void dnd_init(GtkWidget *window);
extern void dnd_setup_target(GtkWidget *w, gpointer *d);
extern void dnd_setup_source(GtkWindow *window, GtkWidget *w, WaveVar *dv);

#endif

