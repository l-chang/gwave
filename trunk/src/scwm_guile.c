/*
 * scwm_guile.c
 * Copyright (C) 1999 Steve Tell
 *
 * based heavily on callbacks.c from SCWM:
 * Copyright (C) 1997-1999 Maciej Stachowiak and Greg J. Badros
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

#include <sys/time.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h>

#include <guile/gh.h>
#include <libguile.h>
#include <libguile/fluids.h>

#include "scwm_guile.h"
#include "guile-compat.h"

#ifdef USE_DMALLOC
#include "dmalloc.h"
#endif

extern char *prog_name;

SCM_HOOK(error_hook, "error-hook", 5, (SCM a, SCM b, SCM c, SCM d, SCM e),
"Called on all kinds of errors and exceptions.
Whenever an error or other uncaught throw occurs on any callback,
whether a hook, a mouse binding, a key binding, a menu entry, a file
being processed, or anything else, error-hook will be invoked. Each
procedure in the hook will be called with the throw arguments; these
will generally include information about the nature of the error.");

struct scwm_body_apply_data {
  SCM proc;
  SCM args;
};


static SCM
scwm_body_apply (void *body_data)
{
  struct scwm_body_apply_data *ad = (struct scwm_body_apply_data *) body_data;
  return gh_apply(ad->proc, ad->args);
}

/* Use scm_internal_cwdr to establish a new dynamic root - this causes
   all throws to be caught and prevents continuations from exiting the
   dynamic scope of the callback. This is needed to prevent callbacks
   from disrupting scwm's flow control, which would likely cause a
   crash. Use scm_internal_stack_catch to save the stack so we can
   display a backtrace. scm_internal_stack_cwdr is the combination of
   both. Note that the current implementation causes three(!) distinct
   catch-like constructs to be used; this may have negative, perhaps
   even significantly so, performance implications. */

struct cwssdr_data
{
  SCM tag;
  scm_catch_body_t body;
  void *data;
  scm_catch_handler_t handler;
};

static SCM
cwssdr_body (void *data)
{
  struct cwssdr_data *d = (struct cwssdr_data *) data;
  return scm_internal_stack_catch (d->tag, d->body, d->data, d->handler, 
				  NULL);
}

SCM
scm_internal_stack_cwdr (scm_catch_body_t body,
			 void *body_data,
			 scm_catch_handler_t handler,
			 void *handler_data,
			 SCM_STACKITEM *stack_item)
{
  struct cwssdr_data d;
  d.tag = SCM_BOOL_T;
  d.body = body;
  d.data = body_data;
  d.handler = handler;
  return scm_internal_cwdr_no_unwind (cwssdr_body, &d, handler, handler_data, 
			    stack_item);
}



SCM
scwm_safe_apply (SCM proc, SCM args)
{
  SCM_STACKITEM stack_item;
  struct scwm_body_apply_data apply_data;

  apply_data.proc = proc;
  apply_data.args = args;

  return scm_internal_stack_cwdr(scwm_body_apply, &apply_data,
				 scwm_handle_error, prog_name,
				 &stack_item);
}


SCM
scwm_safe_apply_message_only (SCM proc, SCM args)
{
  SCM_STACKITEM stack_item;
  struct scwm_body_apply_data apply_data;

  apply_data.proc = proc;
  apply_data.args = args;

  return scm_internal_cwdr_no_unwind(scwm_body_apply, &apply_data,
			   scm_handle_by_message_noexit, prog_name,
			   &stack_item);
}


SCM
scwm_safe_call0 (SCM thunk)
{
  return scwm_safe_apply (thunk, SCM_EOL);
}


SCM
scwm_safe_call1 (SCM proc, SCM arg)
{
  /* This means w must cons (albeit only once) on each callback of
     size one - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg, SCM_UNDEFINED));
}


SCM
scwm_safe_call2 (SCM proc, SCM arg1, SCM arg2)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, SCM_UNDEFINED));
}

SCM
scwm_safe_call3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, SCM_UNDEFINED));
}

SCM
scwm_safe_call4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, arg4, SCM_UNDEFINED));
}

SCM
scwm_safe_call5 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, arg4, arg5, SCM_UNDEFINED));
}

SCM
scwm_safe_call6 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, arg4, arg5, arg6, SCM_UNDEFINED));
}

SCM
scwm_safe_call7 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7)
{
  /* This means w must cons (albeit only once) on each callback of
     size two - seems lame. */
  return scwm_safe_apply (proc, gh_list(arg1, arg2, arg3, arg4, arg5, arg6, arg7, SCM_UNDEFINED));
}


/* Hooks. */

/* FIXDOC: We need a way to cross-reference concepts in docs. */

SCM_CONCEPT("Hooks",
"Hooks are used throughout gwave to provide a convenient mechanism for
user callbacks on particular events. Fundamentally, a hook is just a
variable that contains a list of procedures that are called in order
when the relevant event occurs. However, several convenience macros
are provided for manipulating hooks; see `add-hook!', `remove-hook!',
`reset-hook!', and `run-hook'.");

static SCM run_hook_proc;

__inline__ SCM scwm_run_hook(SCM hook, SCM args)
{
  return scwm_safe_apply(run_hook_proc, gh_cons(hook,args));
}

__inline__ SCM scwm_run_hook_message_only(SCM hook, SCM args)
{
  return scwm_safe_apply_message_only(run_hook_proc, gh_cons(hook,args));
}


__inline__ SCM call0_hooks(SCM hook)
{
  return scwm_run_hook(hook,SCM_EOL);
}

__inline__ SCM call1_hooks(SCM hook, SCM arg1)
{
  return scwm_run_hook(hook,gh_list(arg1,SCM_UNDEFINED));
}

__inline__ SCM call2_hooks(SCM hook, SCM arg1, SCM arg2)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,SCM_UNDEFINED));
}

__inline__ SCM call3_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,SCM_UNDEFINED));
}

__inline__ SCM call4_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,arg4,SCM_UNDEFINED));
}

__inline__ SCM call5_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,arg4,arg5,SCM_UNDEFINED));
}

__inline__ SCM call6_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,arg4,arg5,arg6,SCM_UNDEFINED));
}

__inline__ SCM call7_hooks(SCM hook, SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7)
{
  return scwm_run_hook(hook,gh_list(arg1,arg2,arg3,arg4,arg5,arg6,arg7,SCM_UNDEFINED));
}

SCM
scm_empty_hook_p(SCM hook)
{
  return gh_bool2scm(!gh_pair_p(gh_cddr(hook)));
}


/* Slightly tricky - we want to catch errors per expression, but only
   establish a new dynamic root per load operation, as it's perfectly
   OK for a file to invoke a continuation created by a different
   expression in the file as far as scwm is concerned. So we set a
   dynamic root for the whole load operation, but also catch on each
   eval. */

static SCM
scwm_body_eval_x (void *body_data)
{
  SCM expr = *(SCM *) body_data;
#ifdef HAVE_SCM_EVAL_X_MODULE
  return scm_eval_x (expr, scm_current_module() );
#else
  return scm_eval_x (expr);
#endif
}

__inline__ static SCM 
scwm_catching_eval_x (SCM expr) {
  return scm_internal_stack_catch (SCM_BOOL_T, scwm_body_eval_x, &expr,
			  scwm_handle_error, prog_name);
}

static int clnsProcessingHook = 5;

__inline__ static SCM 
scwm_catching_load_from_port (SCM port)
{
  SCM expr;
  SCM answer = SCM_UNSPECIFIED;
  int i = 0;

  while (!SCM_EOF_OBJECT_P(expr = scm_read (port))) {  
    answer = scwm_catching_eval_x (expr);
  }
  scm_close_port (port);

  return answer;
}

static SCM
scwm_body_load (void *body_data)
{
  SCM filename = *(SCM *) body_data;
  SCM port = scm_open_file (filename, gh_str02scm("r"));
  return scwm_catching_load_from_port (port);
}

static SCM
scwm_body_eval_str (void *body_data)
{
  char *string = (char *) body_data;
  SCM port = scm_mkstrport (SCM_MAKINUM (0), gh_str02scm(string), 
			    SCM_OPN | SCM_RDNG, "scwm_safe_eval_str");
  return scwm_catching_load_from_port (port);
}


SCM 
scwm_handle_error (void *ARG_IGNORE(data), SCM tag, SCM throw_args)
{
#if 0 /* GJB:FIXME:: */
  SCM port = scm_mkstrport(SCM_INUM0, 
			   scm_make_string(SCM_MAKINUM(200), SCM_UNDEFINED),
			   SCM_OPN | SCM_WRTNG,
			   "error-handler");
#else
  SCM port = scm_def_errp;
#endif

  /* GJB:FIXME:MS: is this a guile compatibility test that can be dropped
     now?  */
  if (scm_ilength (throw_args) >= 3)
    {
      SCM fl;
#ifdef HAVE_SCM_THE_LAST_STACK_FLUID_VAR
      fl = SCM_VARIABLE_REF (scm_the_last_stack_fluid_var);
#else
      fl = gh_cdr(scm_the_last_stack_fluid);
#endif
      /* GJB:FIXME:MS: This is a horrible hack,
         but DEREF_LAST_STACK macro was throwing a wrong type 
         argument at weird times, and I'm trying to avoid
         a crash when I demo to RMS tomorrow, hence this
         ugly hack --04/27/99 gjb */
      if (SCM_NIMP (fl) && SCM_FLUIDP (fl)) {
        SCM stack = DEREF_LAST_STACK;
        SCM subr = gh_car (throw_args);
        SCM message = SCM_CADR (throw_args);
        SCM args = SCM_CADDR (throw_args);
        
        scm_newline(port);
        scm_display_backtrace (stack, port, SCM_UNDEFINED, SCM_UNDEFINED);
        scm_newline(port);
        scm_display_error (stack, port, subr, message, args, SCM_EOL);
      } else {
/*        scwm_msg(ERR,"scwm_handle_error","scm_the_last_stack_fluid not holding a fluid!"); */
      }
    }
  else
    {
      scm_puts ("uncaught throw to ", port);
      scm_prin1 (tag, port, 0);
      scm_puts (": ", port);
      scm_prin1 (throw_args, port, 1);
      scm_putc ('\n', port);
      exit (2);
    }

  /* GJB:FIXME:MS: can the scheme code display a backtrace without the
     stack argument? */
/*  DBUG((scwm_msg(DBG,"scwm_handle_error","length(throw_args) = %d", gh_length(throw_args));)) */
  return scwm_run_hook_message_only(error_hook, gh_cons(tag, throw_args));
}


SCM_DEFINE(safe_load, "safe-load", 1, 0, 0,
           (SCM fname),
"Load file FNAME while trapping and displaying errors.
Each individual top-level-expression is evaluated separately and all
errors are trapped and displayed.  You should use this procedure if
you need to make sure most of a file loads, even if it may contain
errors.")
#define FUNC_NAME s_safe_load
{
  SCM_STACKITEM stack_item;
  VALIDATE_ARG_STR(1,fname);
  return scm_internal_cwdr_no_unwind(scwm_body_load, &fname,
				     scm_handle_by_message_noexit, prog_name, 
				     &stack_item);
}
#undef FUNC_NAME

SCM scwm_safe_load (char *filename)
{
  return safe_load(gh_str02scm(filename));
}

SCM scwm_safe_eval_str (char *string)
{
  SCM_STACKITEM stack_item;
  return scm_internal_cwdr_no_unwind(scwm_body_eval_str, string,
				     scm_handle_by_message_noexit, prog_name, 
				     &stack_item);
}

void init_scwm_guile()
{
  run_hook_proc = gh_lookup("run-hook");

#ifndef SCM_MAGIC_SNARFER
#include "scwm_guile.x"
#endif
}


/* Local Variables: */
/* tab-width: 8 */
/* c-basic-offset: 2 */
/* End: */
/* vim:ts=8:sw=2:sta 
 */

