/*
 * prototypes for routines in gtkmisc.c
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.1  1998/08/31 21:00:57  tell
 * Initial revision
 *
 */

extern GtkWidget *create_menu(char *label, GtkWidget *parent);
extern GtkWidget *create_menuitem(char *label, GtkWidget *parent,
				  GtkSignalFunc action, gpointer p);

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
