#ifndef XGEXEC_H
#define XGEXEC_H

#include <X11/Xatom.h>
#include <X11/X.h>
#include <X11/Xlib.h>

Window xgexec_init(Display *dpy);
char *xgexec_exec(Display *dpy, Window w, char *req);
char *xgexec_exec_full(Display *dpy, Window w, char *req,
			 char **output, char **error);

#endif /* XGEXEC_H */
