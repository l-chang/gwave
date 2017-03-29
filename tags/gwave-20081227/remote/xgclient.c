/* $Id: xgclient.c,v 1.1 2000-11-06 06:39:09 sgt Exp $
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

#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xgexec.h"

#define XGNAME "GWAVE"

static Atom XA_XGEXEC_LISTENER;
static Atom XA_XGEXEC_REQUEST;
static Atom XA_XGEXEC_REQWIN;
static Atom XA_XGEXEC_REPLY;
static Atom XA_XGEXEC_NOTIFY;
static Atom XA_XGEXEC_OUTPUT;
static Atom XA_XGEXEC_ERROR;

Window xgexec_init(Display *dpy) 
{
	Window root;
	Atom type_ret;
	int form_ret;
	unsigned long nitems;
	unsigned long bytes_after;
	unsigned char *prop;

	/* intern our atoms */
	XA_XGEXEC_LISTENER=XInternAtom(dpy, XGNAME"EXEC_LISTENER", False);
	XA_XGEXEC_REQWIN=XInternAtom(dpy,   XGNAME"EXEC_REQWIN", False);
	XA_XGEXEC_REQUEST=XInternAtom(dpy,  XGNAME"EXEC_REQUEST", False);
	XA_XGEXEC_REPLY=XInternAtom(dpy,    XGNAME"EXEC_REPLY", False);
	XA_XGEXEC_NOTIFY=XInternAtom(dpy,   XGNAME"EXEC_NOTIFY", False);
	XA_XGEXEC_OUTPUT=XInternAtom(dpy,   XGNAME"EXEC_OUTPUT", False);
	XA_XGEXEC_ERROR=XInternAtom(dpy,    XGNAME"EXEC_ERROR", False);

	root=DefaultRootWindow(dpy);

	nitems=1;
	if (XGetWindowProperty(dpy, root, XA_XGEXEC_LISTENER,
			       0,1, False, AnyPropertyType, 
			       &type_ret, &form_ret, &nitems, &bytes_after,
			       &prop) != Success || prop == NULL || nitems==0) {
		if (prop!=NULL) {
			XFree(prop);
		}
		return (None);
	}

	XFree(prop);
	return(XCreateSimpleWindow(dpy, root, 3, 4, 2, 2, 1, 0, 0));
}

char *xgexec_exec(Display *dpy, Window w, char *req)
{
	char *result, *out, *err;
	result = xgexec_exec_full(dpy, w, req, &out, &err);
	XFree (out);
	XFree (err);
	return result;
}

static Bool FPropertyNotifyOnWindow(Display *dpy, XEvent *ev, Window *w)
{
	if (ev->type==PropertyNotify && ev->xproperty.window== *w) {
		return True;
	} else {
		return False;
	}
}

typedef Bool (*PredicateFn)();

char *xgexec_exec_full(Display *dpy, Window w, char *req,
		       char **output, char **error)
{
	Atom type_ret;
	int form_ret;
	unsigned long nitems;
	unsigned long bytes_after;
	unsigned char *prop;
	Window root=DefaultRootWindow(dpy);
	XEvent ev;
	int got_reply = 0;
	int got_output = 0; 
	int got_error = 0;

	/* X event handling - wait for XA_XGEXEC_REPLY on w 
	   This needs to be before the ChangeProperty, otherwise
	   there is a race condition. --09/15/98 gjb*/
	XSelectInput(dpy,w,PropertyChangeMask);

	XChangeProperty(dpy, w, XA_XGEXEC_REQUEST, XA_STRING,
			8, PropModeReplace, req, strlen(req)+1);

	XChangeProperty(dpy, root, XA_XGEXEC_REQWIN, 1,
			32, PropModeAppend, (unsigned char *) &w, 1);

	do {
		XIfEvent (dpy, &ev, (PredicateFn) FPropertyNotifyOnWindow, (XPointer) &w);
		if (ev.xproperty.state == PropertyNewValue) {
			if (ev.xproperty.atom == XA_XGEXEC_REPLY) {
				got_reply = 1;
			} else if (ev.xproperty.atom == XA_XGEXEC_OUTPUT) {
				got_output = 1;
			} else if (ev.xproperty.atom == XA_XGEXEC_ERROR) {
				got_error = 1;
			}
		}
#ifdef DEBUG_REPLIES
		fprintf(stderr, "Got {reply,output,error} = {%d,%d,%d}\n",
			got_reply, got_output, got_error);
#endif
	} while (!got_reply || !got_output || !got_error);

	*error=NULL;
	*output=NULL;

	/* FIXMS: Grabbing these in delete mode loses massively for some
	   reason.  Need to find out why. The properties really should be
	   deleted.*/
	XGetWindowProperty(dpy, w, XA_XGEXEC_OUTPUT,
			   0,0, False, AnyPropertyType, 
			   &type_ret, &form_ret, &nitems, &bytes_after,
			   (unsigned char **) output);
	XGetWindowProperty(dpy, w, XA_XGEXEC_OUTPUT,
			   0, (bytes_after / 4) + ((bytes_after % 4) ? 1 : 0), 
			   True, type_ret, 
			   &type_ret, &form_ret, &nitems, &bytes_after,
			   (unsigned char **) output);

	XGetWindowProperty(dpy, w, XA_XGEXEC_ERROR,
			   0,0, False, AnyPropertyType, 
			   &type_ret, &form_ret, &nitems, &bytes_after,
			   (unsigned char **) error);
	XGetWindowProperty(dpy, w, XA_XGEXEC_ERROR,
			   0, (bytes_after / 4) + ((bytes_after % 4) ? 1 : 0), 
			   True, type_ret, 
			   &type_ret, &form_ret, &nitems, &bytes_after,
			   (unsigned char **) error);

	XGetWindowProperty(dpy, w, XA_XGEXEC_REPLY,
			   0,0, False, AnyPropertyType, 
			   &type_ret, &form_ret, &nitems, &bytes_after,
			   &prop);
	XGetWindowProperty(dpy, w, XA_XGEXEC_REPLY,
			   0, (bytes_after / 4) + ((bytes_after % 4) ? 1 : 0), 
			   True, type_ret, 
			   &type_ret, &form_ret, &nitems, &bytes_after,
			   &prop);

	return (char *) prop;
}
