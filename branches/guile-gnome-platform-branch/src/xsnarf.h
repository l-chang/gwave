/* $Id: xsnarf.h,v 1.3 2005/10/21 03:06:42 sgt Exp $
 * xsnarf.h
 * 
 * an init-function and document snarfing system for guile, but indepenendent
 * of guile version.  
 * This implementation is most like the one from guile-1.4, but will also
 * work in guile-1.5/1.6
 * 
 */


#ifndef XSNARF_H__
#define XSNARF_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/*#define SCM__INIT_HOOK(var, name, nargs) do { var = scm_permanent_object(scm_c_define(name, scm_make_hook(SCM_MAKINUM(nargs)))); } while (0)
 */
#define SCM__INIT_HOOK(var, name, nargs) do { var = scm_create_hook(name, nargs);} while(0)


#ifndef SCM_FUNC_CAST_ARBITRARY_ARGS
#define SCM_FUNC_CAST_ARBITRARY_ARGS SCM (*)()
#endif


#define XSCM_HOOK(var, name, nargs, arglist, docstring) \
SCM_SNARF_HERE(static SCM var)\
SCM_SNARF_DOCS(hook, name, name, arglist, nargs, 0, 0, docstring) \
SCM_SNARF_INIT( SCM__INIT_HOOK(var, name, nargs) )

#define XSCM_GLOBAL_HOOK(var, name, nargs, arglist, docstring) \
SCM_SNARF_HERE(SCM var)\
SCM_SNARF_DOCS(hook, name, name, arglist, nargs, 0, 0, docstring) \
SCM_SNARF_INIT( SCM__INIT_HOOK(var, name, nargs) )

#define XSCM_CONCEPT(name, docstring)\
	SCM_SNARF_DOCS(concept, name, name, 0, 0, 0, 0, docstring)



#endif /* GUILE_EXT_H__ */

