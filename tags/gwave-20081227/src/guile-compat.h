/* $Id: guile-compat.h,v 1.6 2008-01-19 19:28:50 sgt Exp $ */
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

#ifndef GUILE_COMPAT_H
#define GUILE_COMPAT_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define DEREF_LAST_STACK scm_fluid_ref(SCM_VARIABLE_REF (scm_the_last_stack_fluid_var))

/*SCM 
scm_internal_cwdr_no_unwind (scm_catch_body_t body, void *body_data,
			     scm_catch_handler_t handler, void *handler_data,
			     SCM_STACKITEM *stack_start);
*/

SCM make_output_strport(char *fname);
char *safe_scm_to_stringn (SCM str, size_t *lenp);

#endif /* GUILE_COMPAT_H */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

