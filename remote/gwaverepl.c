/*
 * gwaverepl - read-evaluate-print loop connected to running gwave process.
 * by Steve Tell
 * November 6 10, 2000
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
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <glib.h>
#include "xgexec.h"

Display *display;
char *dispName;
Window w;

extern char *split_at(char **to_split, int i);


int
init_display()
{
	if (!(dispName = getenv("DISPLAY")))
		return 0;
	if (!(display = XOpenDisplay(dispName)))
		return 0;
	return 1;
}


void die(char *str)
{
	fputs(str,stderr);
	exit(1);
}

main(int argc, char **argv)
{
	extern char *optarg;
	extern int optind;
	int ch, fd;
	char *msg;
	char rbuf[32768];
	int port;
	int len;
	char *result, *out, *err;

	int splitpoint;
	char *expr;
	int done = 0;
	char *gather = g_new(char, 1);


	if (argc != 1)
		die("Usage: gwaverepl\n");
	if (!init_display())
		die("Could not connect to gwave server. Check your DISPLAY environment variable.\n");

	w=xgexec_init(display);

	if (w==None)
		die("Unable to establish gwave-exec connection.\n");

#ifdef HAVE_READLINE
	init_readline();
#endif
	while (!done) {
		if ((splitpoint = check_balance(gather))) {
			char *result, *error, *output;
			expr = split_at(&gather,splitpoint);

			result = xgexec_exec_full(display, w, expr, &output, &error);
			fputs (output, stdout);
			if (strlen(error)!=0) {
				fputs(error, stderr);
			} else {
				fputs(result, stdout);
			}
			putchar('\n');

			if (result) { g_free(result); result = NULL; }
			if (output) { g_free(output); output = NULL; }
			if (error) { g_free(error); error = NULL; }
			free(expr);
			expr = NULL;
		} else {
			done = !appending_fgets(&gather);
		}
	}
	
	XCloseDisplay (display);

	exit(0);
}

