/*
 * wavelist.h - part of gwave
 * Declarations related to waveform data storage.
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
 * You should have received a copy of the GNU General Public
 * License along with this software; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef WAVELIST_H
#define WAVELIST_H

#ifndef SCWM_GUILE_H__
#include <scwm_guile.h>
#endif

#undef EXTERN
#undef EXTERN_SET
#ifdef WAVELIST_IMPLEMENTATION
#define EXTERN
#define EXTERN_SET(x,y) x = y
#else
#define EXTERN extern
#define EXTERN_SET(x,y) extern x
#endif

/***********************************************************************
 * Stuff to wrap GwDataFile as a SMOB 
 */

EXTERN long scm_tc16_scwm_GWDataFile;

#define GWDataFile_P(X) (SCM_NIMP(X) && SCM_SMOB_PREDICATE(scm_tc16_scwm_GWDataFile, X))
#define GWDataFile(X)  ((GWDataFile *)SCM_SMOB_DATA(X))
#define SAFE_GWDataFile(X)  (GWDataFile_P((X))? GWDataFile((X)) : NULL)

#define VALIDATE_ARG_GWDataFile(pos,scm) \
  do { \
  if (!GWDataFile_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_GWDataFile_COPY(pos,scm,cvar) \
  do { \
  if (!GWDataFile_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  else cvar = GWDataFile(scm); \
  } while (0)

/* "handles" to actual WaveVars.  We can 
 * invalidate these when freeing the WaveFile,
 * even if un-gc'ed WaveVar smobs still want to point to
 * them. 
 * Pointers to WaveVarH are stored:
 *	in the udata pointer of WaveVar, if one has been alocated
 *	in the wvhl list in the GWDataFile
 *	in possibly many guile smobs.
*/
struct _WaveVarH { 
	WaveVar *wv;
	GWDataFile *df;
	SCM smob;
};
typedef struct _WaveVarH WaveVarH;

/*
 * Structure to hold data for a single loaded waveform file.
 */
struct _GWDataFile {
	WaveFile *wf;
	GtkWidget *wlist_win;	/* window with scrolling variable list */
	GtkWidget *wlist_box; 	/* scrolled box containing DnD variable items */
	GtkWidget *wlist_menubar; /* menubar in variable list window */
	GtkWidget *menu_item;	/* item in main window submenu */
	char *ftag;	/* short tag used to help identify which file is which */
	SCM smob;
	int outstanding_smob;	/* if the guile world has a pointer, defer freeing. */
	int ndv;
	GSList *wvhl;
};

/* given a wavevar, how to get back to a gwdatafile... follow pointers
 * to wtable to WaveFile to GWDataFile */
#define wvar_gwdatafile(WV) ((GWDataFile*)(WV)->wtable->wf->udata)


/***********************************************************************
 * Stuff to wrap WaveVar as a SMOB 
 */

EXTERN long scm_tc16_scwm_WaveVar;

#define WaveVarH_P(X) (SCM_NIMP(X) && SCM_SMOB_PREDICATE(scm_tc16_scwm_WaveVar, X))
#define WaveVarH(X)  ((WaveVarH *)SCM_SMOB_DATA(X))
#define SAFE_WaveVarH(X)  (WaveVarH_P((X))? WaveVarH((X)) : NULL)
#define SAFE_WaveVar(X)  (WaveVarH_P((X)) ? WaveVarH((X))->wv : NULL)

#define VALIDATE_ARG_WaveVarH(pos,scm) \
  do { \
  if (!WaveVarH_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  } while (0)

#define VALIDATE_ARG_WaveVar_COPY(pos,scm,cvar) \
  do { \
  if (!WaveVarH_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  else cvar = (WaveVarH(scm)->wv) ? WaveVarH(scm)->wv : NULL; \
  } while (0)

#define VALIDATE_ARG_WaveVarH_COPY(pos,scm,cvar) \
  do { \
  if (!WaveVarH_P(scm)) scm_wrong_type_arg(FUNC_NAME,pos,scm); \
  else cvar = WaveVarH(scm); \
  } while (0)


#endif /* WAVELIST_H */

