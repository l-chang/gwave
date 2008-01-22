/* $Id: xsnarf.h,v 1.3 2005-10-21 03:06:42 sgt Exp $
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

/* three ways of making a hook, taken from SCWM's compatibility layer */
#ifndef HAVE_SCM_MAKE_HOOK
#define SCM__INIT_HOOK(var, name, nargs) do { var = scm_sysintern(name, SCM_EOL); } while (0)
#else
#ifdef HAVE_SCM_CREATE_HOOK
#define SCM__INIT_HOOK(var, name, nargs) do { var = scm_create_hook(name,nargs); } while (0)
#else
#define SCM__INIT_HOOK(var, name, nargs) do { var = scm_make_named_hook(name,nargs); } while (0)
#endif
#endif

#ifdef XSCM_MAGIC_SNARF_INITS
# define XSCM_SNARF_HERE(X)
# define XSCM_SNARF_INIT(X) ^^ X
# define XSCM_SNARF_DOCS(TYPE, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)
#else
# ifdef XSCM_MAGIC_SNARF_DOCS
#  define XSCM_SNARF_HERE(X)
#  define XSCM_SNARF_INIT(X)
#  define XSCM_SNARF_DOCS(TYPE, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING) \
^^ ( \
(fname FNAME) \
(type TYPE ) \
(location __FILE__ __LINE__ ) \
(arglist #ARGLIST ) \
(argsig REQ OPT VAR ) \
(doc DOCSTRING))
# else
#  define XSCM_SNARF_HERE(X) X
#  define XSCM_SNARF_INIT(X)
#  define XSCM_SNARF_DOCS(TYPE, FNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)
# endif
#endif

#ifndef SCM_FUNC_CAST_ARBITRARY_ARGS
#define SCM_FUNC_CAST_ARBITRARY_ARGS SCM (*)()
#endif

#define XSCM_DEFINE(FNAME, PRIMNAME, REQ, OPT, VAR, ARGLIST, DOCSTRING) \
XSCM_SNARF_HERE(\
static const char s_ ## FNAME [] = PRIMNAME; \
SCM FNAME ARGLIST\
)\
XSCM_SNARF_INIT(\
scm_c_define_gsubr (s_ ## FNAME, REQ, OPT, VAR, \
                (SCM_FUNC_CAST_ARBITRARY_ARGS) FNAME); \
)\
XSCM_SNARF_DOCS(primitive, PRIMNAME, ARGLIST, REQ, OPT, VAR, DOCSTRING)


#define XSCM_HOOK(var, name, nargs, arglist, docstring) \
XSCM_SNARF_HERE(static SCM var)\
XSCM_SNARF_DOCS(hook, name, arglist, nargs, 0, 0, docstring) \
XSCM_SNARF_INIT( SCM__INIT_HOOK(var, name, nargs) )

#define XSCM_GLOBAL_HOOK(var, name, nargs, arglist, docstring) \
XSCM_SNARF_HERE(SCM var)\
XSCM_SNARF_DOCS(hook, name, arglist, nargs, 0, 0, docstring) \
XSCM_SNARF_INIT( SCM__INIT_HOOK(var, name, nargs) )

#define XSCM_CONCEPT(name, docstring)\
XSCM_SNARF_DOCS(concept, name, 0, 0, 0, 0, docstring)



#define XSCM_VCELL(c_name, scheme_name, docstring) \
XSCM_SNARF_HERE(static SCM c_name) \
XSCM_SNARF_INIT(c_name = scm_permanent_object (scm_intern0 (scheme_name)); SCM_SETCDR (c_name, SCM_BOOL_F)); \
XSCM_SNARF_DOCS(vcell, scheme_name, 0, 0, 0, 0, docstring)

#define XSCM_GLOBAL_VCELL(c_name, scheme_name, docstring) \
XSCM_SNARF_HERE(SCM c_name) \
XSCM_SNARF_INIT(c_name = scm_permanent_object (scm_intern0 (scheme_name)); SCM_SETCDR (c_name, SCM_BOOL_F)); \
XSCM_SNARF_DOCS(vcell, scheme_name, 0, 0, 0, 0, docstring)

#define XSCM_VCELL_INIT(c_name, scheme_name, init_val, docstring) \
XSCM_SNARF_HERE(static SCM c_name) \
XSCM_SNARF_INIT(c_name = scm_permanent_object (scm_intern0 (scheme_name)); SCM_SETCDR (c_name, init_val));\
XSCM_SNARF_DOCS(vcell, scheme_name, 0, 0, 0, 0, docstring)

#define XSCM_GLOBAL_VCELL_INIT(c_name, scheme_name, init_val, docstring) \
XSCM_SNARF_HERE(SCM c_name) \
XSCM_SNARF_INIT(c_name = scm_permanent_object (scm_intern0 (scheme_name)); SCM_SETCDR (c_name, init_val));\
XSCM_SNARF_DOCS(vcell, scheme_name, 0, 0, 0, 0, docstring)

#endif /* GUILE_EXT_H__ */

