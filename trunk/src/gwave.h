
/*
 * declarations and definitions for gwave - waveform viewer
 * Steve Tell
 *
 * $Log: not supported by cvs2svn $
 */

#ifndef GWAVE_H
#define GWAVE_H

/*
 * Structure to hold all of the waveform data, displayed or not.
 * Little more than a placeholder, it will get fleshed out when we move
 * to support having multiple data files loaded at once.
 */
typedef struct {
	DataFile *df;
} WData;

extern WData *waveData;

/* defined in wavelist.c */
extern void cmd_show_wave_list(GtkWidget *widget);
extern char *possible_drag_types[];
extern char *accepted_drop_types[];

/* defined in pixmaps.c */
extern char *drag_no_xpm[];
extern char *wave_drag_ok_xpm[];

#endif
