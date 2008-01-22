/* $Id: guile-compat.c,v 1.3 2002/03/28 06:35:21 sgt Exp $ */
/*
 * Copyright (C) 1997-1999, Maciej Stachowiak and Greg J. Badros
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.GPL.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA
 *
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <guile/gh.h>

#include "guile-compat.h"

#ifdef __cplusplus
extern "C" {
#endif

#define DBG -1

void  scwm_msg(int , const char *id, const char *msg,...);

#undef USE_STACKJMPBUF


SCM make_output_strport(char *fname)
{
  return scm_mkstrport(SCM_INUM0, scm_make_string(SCM_INUM0, 
						  SCM_UNDEFINED),
		       SCM_OPN | SCM_WRTNG,
		       fname);
}

#ifdef __cplusplus
}
#endif


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

