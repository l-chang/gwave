/* $Id: guile-compat.c,v 1.3 2002/03/28 06:35:21 sgt Exp $ */
/*
 * Original Copyright (C) 1997-1999, Maciej Stachowiak and Greg J. Badros
 * Additions copyright 2008 Stephen G. Tell.
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
#include <string.h>
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


/* variant of guile-1.8's scm_to_locale_string that never throws an error.
 * Instead of throwing an exception, NULL is returned if the arg is not a
 * scheme string.
 *
 * The result is always nul terminated, and the length is always available,
 * although a the lenp pointer may be passed as NULL, if the caller wants to
 * ignore the length and assume that the string contains no inner \0s.
 */
char *safe_scm_to_stringn (SCM str, size_t *lenp)
{
	char *res;
	size_t len;

	if (!scm_is_string (str)) {
		if(lenp)
			*lenp = 0;
		return NULL;
	}
	len = scm_i_string_length (str);
	res = scm_malloc (len + 1);
	memcpy (res, scm_i_string_chars (str), len);
	res[len] = '\0';   //unconditionaly null terminate
	
	if(lenp)
		*lenp = len;
	
	scm_remember_upto_here_1 (str);
	return res;
}




#ifdef __cplusplus
}
#endif
