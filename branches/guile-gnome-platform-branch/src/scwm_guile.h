/* $Id
 * scwm_guile.h
 * 
 * Guile-interface help inspired by SCWM.
 * 
 * Based on scwm.h:
 * Copyright (C) 1997-1999, Maciej Stachowiak and Greg J. Badros
 */


#ifndef SCWM_GUILE_H__
#define SCWM_GUILE_H__

#include "arg_unused.h"
#include <guile/gh.h>
#include "validate.h"
#include <xsnarf.h>

#ifndef False
#define False 0
#endif
#ifndef True
#define True (!False)
#endif


#define SCWM_MAKE_HOOK(args) scm_permanent_object(scm_make_hook(scm_long2num(args)))

SCM scwm_handle_error (void *handler_data, SCM tag, SCM throw_args);
SCM scwm_safe_apply_message_only (SCM proc, SCM args);

/* Individual callbacks. */

SCM scwm_safe_apply (SCM proc, SCM args);
SCM scwm_safe_call0 (SCM thunk);
SCM scwm_safe_call1 (SCM proc, SCM arg);
SCM scwm_safe_call2 (SCM proc, SCM arg1, SCM arg2);

SCM safe_load (SCM fname);
SCM scwm_safe_load (char *filename);
SCM scwm_safe_eval_str (char *string);

/* Hooks. */

SCM call0_hooks (SCM hook);
SCM call1_hooks (SCM hook_type, SCM arg);
SCM call2_hooks (SCM hook_type, SCM arg1, SCM arg2);
SCM call3_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3);
SCM call4_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3, SCM arg4);
SCM call5_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5);
SCM call6_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6);
SCM call7_hooks (SCM hook_type, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7);

SCM scwm_run_hook(SCM hook, SCM args);
SCM scwm_run_hook_message_only(SCM hook, SCM args);

SCM scm_empty_hook_p(SCM hook);


/* new-style SMOBs -- this gives smobs names, too */
#define MAKE_SMOBFUNS(T) /* nothing */
#define REGISTER_SCWMSMOBFUNS(T) \
 do { \
    scm_tc16_scwm_ ## T = scm_make_smob_type_mfpe( #T, 0, &(mark_ ##T), &(free_ ## T), &(print_ ## T), NULL); \
  } while (0)


#ifndef SCWM_EXTRACT_COMMENTS
/* do not define this macro if we are extracting comments since
   the macro name is used as a lexical cue to the extractor */

/* SCWM_VAR_INIT, SCWM_VAR still require a variable declaration */

#define SCWM_VAR_INIT(cvar, name, val) \
  do { pscm_ ## cvar = SCM_CDRLOC( \
      scm_sysintern(name, val) ); } while (0)

#define SCWM_VAR(cvar, name) \
  do { pscm_ ## cvar = SCM_CDRLOC( \
      scm_sysintern0(name) ); } while (0)


/* GJB:FIXME:: Note that cvar is ignored for now */
#define SCWM_VAR_READ_ONLY(cvar, name,val) \
  do { scm_sysintern(name,val); \
     } while (0)


#endif /* !SCWM_EXTRACT_COMMENTS */


/* Check if the scm variable is undefined or #f -- these cases
   correspond to places where we want to use a default value
   either because the args were omitted, or #f was used to skip
   the argument to get to an argument that the client wanted to 
   specify.
   Intentionally not named SCM_UNSET, since that would imply
   it's part of guile */
#define UNSET_SCM(x) (((x) == SCM_UNDEFINED) || ((x) == SCM_BOOL_F))

#define GC_MARK_SCM_IF_SET(scm) do { if (scm && !UNSET_SCM((scm))) \
     { scm_gc_mark((scm)); } } while (0)

#define SCWM_NEWCELL_SMOB(ANSWER,ID,PSMOB) \
   do { \
     SCM_NEWCELL((ANSWER)); \
     SCM_SETCDR((ANSWER),(SCM) (PSMOB)); \
     SCM_SETCAR((ANSWER),(ID)); \
   } while (0)

/* a variation that hides the tc16 part */
#define SGT_NEWCELL_SMOB(ANSWER,ID,PSMOB) \
   do { \
     SCM_NEWCELL((ANSWER)); \
     SCM_SETCAR((ANSWER),(scm_tc16_scwm_ ## ID)); \
     SCM_SETCDR((ANSWER),(SCM) (PSMOB)); \
   } while (0)

#define DEREF_IF_SYMBOL(x) do { if (gh_symbol_p((x))) { \
                                   (x) = scm_symbol_binding(SCM_BOOL_F,(x)); \
                                } } while (0)

#define DYNAMIC_PROCEDURE_P(x) (gh_procedure_p((x)) || \
				(gh_symbol_p((x)) && \
				 gh_procedure_p(scm_symbol_binding(SCM_BOOL_F,(x)))))

#define PROCEDURE_OR_SYMBOL_P(x) (gh_procedure_p((x)) || gh_symbol_p((x)))

#define RESTP_SCM 1


#define scwm_ptr2scm(p) gh_long2scm((long)(p))

#define SCM_BOOL_FromBool(x) ((x)? SCM_BOOL_T: SCM_BOOL_F)



#define PackedBool(x) unsigned short x:1

#endif /*  SCWM_GUILE_H__ */

/* Global variables */

/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

