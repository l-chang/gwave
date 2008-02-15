/* $Id: xgserver.c,v 1.1 2000/11/06 06:42:20 sgt Exp $
 * xgserver.c
 * Copyright 2000 Steve Tell
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
 * 
 * This file is based on fragments of scwm.c and events.c from
 *  the SCWM window manager, by  Greg J. Badros and Maciej Stachowiak 
 *
 * extracting only the scwmexec protocol, generalizing it, and adapting
 * it to gdk was done by Steve Tell.
 */

#define XGNAME "GWAVE"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <assert.h>

#include <guile-gnome-gobject/gobject.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

static GdkFilterReturn xg_gdk_filter(GdkXEvent *xevent,
			      GdkEvent *event,
			      gpointer data);
static void xg_cleanup();
extern char *remote_guile_eval(char *req, size_t *reslen,
			       char **outp, size_t *outlenp,
			       char **errp, size_t *errlenp);

static void xg_handle_exec();


/**CONCEPT: SCWMEXEC Protocol 
  Scwm supports a protocol for other programs to send commands to the
window manager. Programs send ordinary configuration language
expressions and are returned a string representation of the return
value, and the output and error output generated, if any.

  For more information on how to make use of this protocol, see the
documentation for the scwmexec and scwmrepl programs, the scwm.el
emacs interaction mode, the libscwmexec library, and the details of
the SCWMEXEC protocol (as documented in
<filename>doc/scwmexec.proto</filename>).
FIXDOC: Link to file!
*/

/* w_for_scwmexec_response is the window that is used by scwmexec
   protocol -- shutdown.c's Done function uses this too in case
   scwmexec executes a quit, or causes a segfault (which cases
   the HandleScwmExec function to not complete as it should) */
static Window w_for_exec_response;  


static Atom XA_XGEXEC_LISTENER;
static Atom XA_XGEXEC_REQWIN;
static Atom XA_XGEXEC_REQUEST;
static Atom XA_XGEXEC_REPLY;
static Atom XA_XGEXEC_NOTIFY;
static Atom XA_XGEXEC_OUTPUT;
static Atom XA_XGEXEC_ERROR;

static Display *Dpy;
static Window Root;

/*
Reset the scwmexec protocol.\n\
This procedure removes the \"XA_SCWMEXEC_REQUEST\" property on the\n\
root window.  It should not be necessary but may be useful in case\n\
your X server goes awry (and otherwise you would have to restart your\n\
X server).  Use if scwmexec or scwmrepl are not returning (e.g.,\n\
if your Emacs hangs when you try evaluating a scwm expression).")
*/
void
xg_reset_protocol()
{
	XDeleteProperty(Dpy, Root, XA_XGEXEC_REQUEST);
}

void
xg_init(Display *display)
{
	XWindowAttributes xwa;
	int screen;
	GdkWindow *gdk_rootwindow;
	long old_event_mask;

	if(display == NULL)
		Dpy = GDK_DISPLAY();
	else
		Dpy = display;
	
	screen = XDefaultScreen(Dpy);
	Root = XRootWindow(Dpy, screen);

	XA_XGEXEC_LISTENER=XInternAtom(Dpy, XGNAME "EXEC_LISTENER", False);
	XA_XGEXEC_REQWIN=XInternAtom(Dpy, XGNAME "EXEC_REQWIN", False);
	XA_XGEXEC_REQUEST=XInternAtom(Dpy, XGNAME "EXEC_REQUEST", False);
	XA_XGEXEC_REPLY=XInternAtom(Dpy,  XGNAME "EXEC_REPLY", False);
	XA_XGEXEC_NOTIFY=XInternAtom(Dpy, XGNAME "EXEC_NOTIFY", False);
	XA_XGEXEC_OUTPUT=XInternAtom(Dpy, XGNAME "EXEC_OUTPUT", False);
	XA_XGEXEC_ERROR=XInternAtom(Dpy, XGNAME "EXEC_ERROR", False);

/* if the XA_XGEXEC_REQWIN window is already set at 
   startup, the first scwm-exec protocol request will cause
   lots of X errors */
	XDeleteProperty(Dpy,Root,XA_XGEXEC_REQWIN);

	g_atexit(xg_cleanup);

	/* Initialize scwmexec response window (used in shutdown.c's Done,
	   as well as by HandleScwmExec) */
	w_for_exec_response = None;
	/* Announce support for scwmexec protocol. */
	XChangeProperty(Dpy, Root, 
			XA_XGEXEC_LISTENER, XA_STRING,
			8, PropModeReplace, (unsigned char *) XGNAME "exec", 
			4+strlen(XGNAME) );

/*	printf("xg_init root=%d LISTENER atom=%d\n", Root, XA_XGEXEC_LISTENER);
 */


	XGetWindowAttributes (gdk_display, Root, &xwa);
	old_event_mask = xwa.your_event_mask;
	XSelectInput (Dpy, Root, old_event_mask | PropertyChangeMask);

	/* connect to gdk's XEvent handler using its event-filter mechanism */
	gdk_rootwindow = gdk_window_lookup(Root);
	gdk_window_add_filter(gdk_rootwindow, xg_gdk_filter, NULL);
}

/* gdk event filter to hook into low-level gdk event handling 
 */
static GdkFilterReturn 
xg_gdk_filter(GdkXEvent *xevent, GdkEvent *event, gpointer data)
{
	XEvent *ev = (XEvent *)xevent;
/*	printf("in xg_gdk_filter data=%x type=%d\n", data, ev->type); */
	
	if(ev->type == PropertyNotify
	   && ev->xproperty.atom == XA_XGEXEC_REQWIN) {
		xg_handle_exec();
		return GDK_FILTER_REMOVE;
	} else
		return GDK_FILTER_CONTINUE;
}

static void
xg_cleanup()
{
	XDeleteProperty(Dpy, Root, XA_XGEXEC_LISTENER);

	if (None != w_for_exec_response) {
		/* give a response to libscwmexec in case 
		   we were in the middle of
		   an xgexec when we quit or segfaulted */
		XChangeProperty(Dpy, w_for_exec_response,
				XA_XGEXEC_OUTPUT, XA_STRING,
				8, PropModeReplace, "", 0);
		XChangeProperty(Dpy, w_for_exec_response,
				XA_XGEXEC_ERROR, XA_STRING,
				8, PropModeReplace, "", 0);
		XChangeProperty(Dpy, w_for_exec_response,
				XA_XGEXEC_REPLY, XA_STRING,
				8, PropModeReplace, "", 0);
	}
}

/* implement the X-property-driven remote-exec protocol.
 * gets called on PropertyNotify events.
 */
static void
xg_handle_exec()
{
	Window w;
	Window *pw;
	Atom type_ret;
	int form_ret;
	unsigned char *ret, *output, *error;
	size_t reslen, outlen, errlen;

	unsigned long nitems;
	unsigned long bytes_after;
	unsigned char *req;
	unsigned long last_offset=0;
	unsigned long saved_bytes_after=0;
  
	/* The XGEXEC_REQWIN property is treated as a queue of window IDs
	   from which the request will be read. There may be more than one
	   (or fewer than one in some cases) by the time we get here. We
	   will loop and keep reading until we have snarfed the whole
	   property, to make sure we can safely delete it. 

	   See also the doc/scwmexec.proto file for a high-level 
	   description of this protocol.
	*/
	do {
		/* Read a single request window from the queue. */
		if (XGetWindowProperty(Dpy, Root, XA_XGEXEC_REQWIN,
				       last_offset, 1, True, AnyPropertyType, 
				       &type_ret, &form_ret, &nitems, &bytes_after,
				       (unsigned char **) &pw)==Success && pw != NULL) {
			/* This is the window we want to look at: */
			w = *pw;
			XFree(pw);
			/* Increment the offset at which to read within the property. It
			   will not get deleted until we read the very last bytes at the
			   end. */
			last_offset += nitems * (form_ret/8);
			/* Save an indication of whether we need to read more or not. */
			saved_bytes_after=bytes_after;
      
/*      DBUG((DBG,FUNC_NAME,"Trying to get request from %ld",w)); */
      
			/* Get and delete its XGEXEC_REQUEST property. We do
			   XGetWindowProperty twice, once to get the length, and again
			   to read the whole length's worth. */
			if (XGetWindowProperty(Dpy, w,
					       XA_XGEXEC_REQUEST,
					       0, 0, False, XA_STRING, 
					       &type_ret, &form_ret, &nitems, &bytes_after,
					       &req)==Success && 
			    XGetWindowProperty(Dpy, w,
					       XA_XGEXEC_REQUEST,
					       0, (bytes_after / 4) +
					       (bytes_after % 4 ? 1 : 0), True, XA_STRING, 
					       &type_ret, &form_ret, &nitems, &bytes_after,
					       &req)==Success) {

				/* before we eval the request, record the window to respond
				   in a global, so Done can respond if necessary (in case
				   the eval-d expression calls `quit' or seg faults, etc.) 
				   TODO: implement this scwm behavior here.
				*/
				w_for_exec_response = w;

				ret = remote_guile_eval(req,  &reslen,
							(char **)&output, &outlen,
							(char **)&error, &errlen);
				XFree(req); 

				/* Set the output, error and reply properties appropriately. */
				XChangeProperty(Dpy, w_for_exec_response,
						XA_XGEXEC_OUTPUT, XA_STRING,
						8, PropModeReplace, output,
						(long)outlen);
				XChangeProperty(Dpy, w_for_exec_response,
						XA_XGEXEC_ERROR, XA_STRING,
						8, PropModeReplace, error,
						(long)errlen);
				XChangeProperty(Dpy, w_for_exec_response,
						XA_XGEXEC_REPLY, XA_STRING,
						8, PropModeReplace, ret, 
						(long)reslen);
          
				/* Since we successfully reset the reply properties,
				   shutdown.c's Done no longer needs to, so reset
				   the global */
				w_for_exec_response = None;
        
				free(ret);
				free(output);
				free(error);
			} else {
				fprintf(stderr, "Cannot get XA_%sEXEC_REQUEST atom from window %ld",
					XGNAME, w_for_exec_response);
			}
		} else {
			/* XGetWindowProperty returned False */
/*      DBUG((WARN,FUNC_NAME,"Done with last window in list of scwmexec requests"));*/
			saved_bytes_after = 0;
			last_offset = 0;
		}
	} while (saved_bytes_after != 0);
	/* Repeat until we get a saved_bytes_after of 0 on reading XGEXEC_REQWIN,
	   indicating that we read it all and it was deleted. It may well have
	   been re-created before we exit, but that doesn't matter because we'll
	   get a PropertyNotify and re-enter, but the offset to use will correctly
	   be 0. */

	return;
}


