/*
 * prototypes for routines in gtkmisc.c
 *
 * $Log: not supported by cvs2svn $
 */

/* create icon-window from xpm file */ 
extern GtkWidget *shape_create_icon (char     *xpm_file,
				     gint      x,
				     gint      y,
				     gint      px,
				     gint      py,
				     gint      window_type);

/* create icon-window from xpm data in char array */ 
extern GtkWidget *shape_create_icon_d (char     **xpm_data,
				       gint      x,
				       gint      y,
				       gint      px,
				       gint      py,
				       gint      window_type);
