/* acconfig.h --- documentation for local preprocessor symbols defined
 *  by configure.  
 *  Use autoheader to process acconfig.h along with configure.in to generate
 *  config.h.in
 *  Steve Tell <tell@cs.unc.edu>
 */

/* Name of package.  */
#undef PACKAGE

/* Version of package.  */
#undef VERSION

/* do we have posix regex routines? */
#undef HAVE_POSIX_REGEXP

/* do we have Gtk+? */
#undef HAVE_GTK

/* does the Gtk+ we found have the new drag-and-drop stuff?
 * do we need to use gtk_scrolled_window_add_with_viewport()?
 */
#undef GTK_V12

/* If present, the pathname of graph, from GNU plotutils. 
 */
#undef PROG_GRAPH

/* Define this if your libguile has a scm_eval_string that is safe against
   re-entry by continuations. This should be true of snapshots newer than
   970928.  */
#undef HAVE_SAFE_SCM_EVAL_STRING

/* Define this if your libguile exports scm_puts, meaning that
   scm_gen_puts should no longer be used. This should be true of
   snapshots newer than 971014.  */
#undef HAVE_SCM_PUTS

/* Define this if your libguile has gh_vector_ref instead of gh_vref,
   meaning that gh_vref should no longer be used. This should be
   true of snapshots newer than 971012.  */
#undef HAVE_GH_VECTOR_REF

/* Define this if your libguile has gh_vector_set_x instead of gh_vset,
   meaning that gh_vset should no longer be used. This should be
   true of snapshots newer than 971020.  */
#undef HAVE_GH_VECTOR_SET_X

/* Define this if your libguile has readline support. This should be
   true of snapshots newer than 971023.  */
#undef HAVE_SCM_READLINE

/* Define this if your libguile has gh_length and not
   gh_list_length. This should be true of snapshots newer than 970915.  */
#undef HAVE_GH_LENGTH

/* Define this if your libguile has scm_parse_path.  */
#undef HAVE_SCM_PARSE_PATH

/* Define this if your libguile has scm_internal_select.  */
#undef HAVE_SCM_INTERNAL_SELECT

/* Define this if your libguile has scm_the_last_stack_fluid instead
   of scm_the_last_stack_var.  */
#undef HAVE_SCM_THE_LAST_STACK_FLUID

/* Define this if your libguile has scm_internal_cwdr.  */
#undef HAVE_SCM_INTERNAL_CWDR

/* Define this if your libguile has scm_internal_stack_catch.  */
#undef HAVE_SCM_INTERNAL_STACK_CATCH

/* Define this if your libguile has scm_internal_parse_path,
   which should be used instead of scm_parse_path from C.  */
#undef HAVE_SCM_INTERNAL_PARSE_PATH

/* Define this if your libguile has a scm_make_vector, which needs
   three arguments. This should be true only of older versions. */
#undef HAVE_SCM_MAKE_VECTOR_3_ARGS

/* Define this if your libguile has scm_load_startup_files,
   which means the hack to get boot-9.scm to be loaded is unnecessary
   and even dangerous. */
#undef HAVE_SCM_LOAD_STARTUP_FILES

/* Define this if your libguile has scm_make_hook, indicating
   C-level support for hooks. */
#undef HAVE_SCM_MAKE_HOOK

/* Define this if your libguile has scm_strport_to_string
   (added sometime after guile-1.3) */
#undef HAVE_SCM_STRPORT_TO_STRING
