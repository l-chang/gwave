/*
 * GtkTable_indel.h - definitions of GtkTable convenience functions
 */

#ifndef GTK_TABLE_INDEL_H
#define GTK_TABLE_INDEL_H

extern void gtk_table_move_row(GtkTable *table, int from, int to);
extern void gtk_table_insert_row(GtkWidget *widget, int row);
extern void gtk_table_delete_row(GtkWidget *widget, int row);
extern void gtk_table_rotate_rows(GtkWidget *widget, int from, int to);
extern int gtk_table_get_child_row(GtkWidget *widget, GtkWidget *child);

#endif
