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

