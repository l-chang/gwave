/*
 * print.c, part of the gwave waveform viewer tool
 *
 * Functions in this file implement exporting waveforms for printing
 * or incorporation into other documents.
 *
 * Copyright (C) 1998, 1999 Stephen G. Tell
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include <ctype.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include <gtk/gtk.h>
#include <config.h>
#include "gwave.h"

static char *tmpdir = "/tmp";
#ifdef PROG_GRAPH
static char *graph_prog = PROG_GRAPH;
#else
/* probably won't work, but it will compile...*/
static char *graph_prog = "graph";
#endif

struct visit_export_data {
	WavePanel *wp;
	FILE *fp;
};


static void
vw_wp_visit_export_visible(gpointer p, gpointer d)
{
	VisibleWave *vw = (VisibleWave *)p;
	WavePanel *wp = ((struct visit_export_data *)d)->wp;
	FILE *fp = ((struct visit_export_data *)d)->fp;
	WaveVar *dv, *iv;
	int starti, endi, i;
	double x,y;

	dv = vw->var;
	iv = dv->wv_iv;
	starti = wf_find_point(iv, wp->start_xval);
	endi = wf_find_point(iv, wp->end_xval);

	for(i = starti; i <= endi; i++) {
		x = wds_get_point(&iv->wds[0], i);
		y = wds_get_point(&dv->wds[0], i);
		fprintf(fp, "%g %g\n", x, y);
	}
	fputc('\n', fp);
}


/*
 * write a file containing the visible data in a single wavepanel
 */
int
export_graph_data(char *file, WavePanel *wp)
{
	FILE *fp;
	struct visit_export_data ved;

	fp = fopen(file, "w");
	if(!fp) {
		perror(file);
		return -1;
	}
	ved.wp = wp;
	ved.fp = fp;
	g_list_foreach(wp->vwlist, vw_wp_visit_export_visible, (gpointer)&ved);

	fclose(fp);
}



/*
 * print/export a representation of what is shown on the screen.
 * Only a minimum of options are available so far.
 */
void
cmd_print(char *file, char *format, int color, int landscape)
{
	char **graph_argv;
	int graph_argc = 0;

	int ngraphs = 0;
	char **dfnames;
	char buf1[64], buf2[64], buf3[64], buf4[64];
	int outfd;
	int pid;
	int status;
	int i;
	WavePanel *wp;
	int rc;

	dfnames = g_new(char*, wtable->npanels);
	
	for(i = 0; i < wtable->npanels; i++) {
		wp = wtable->panels[i];
		if(wp == NULL)
			continue; /* deleted panel */
		if(wp->vwlist == NULL)
			continue; /* no waves? skip panel */
		dfnames[ngraphs] = g_new(char,256);
		sprintf(dfnames[ngraphs], "%s/gwplot.%d", tmpdir, ngraphs);
		
		export_graph_data(dfnames[ngraphs], wp);

		ngraphs++;
	}

	graph_argv = g_new(char*, 30 + 5*ngraphs);
	graph_argv[graph_argc++] = graph_prog;
	graph_argv[graph_argc++] = "-T";
	graph_argv[graph_argc++] = format;
	graph_argv[graph_argc++] = "--input-format";
	graph_argv[graph_argc++] = "a";
	graph_argv[graph_argc++] = "--width-of-plot";
	graph_argv[graph_argc++] = "0.9";

	graph_argv[graph_argc++] = "--height-of-plot";
	sprintf(buf1, "%.3f", (0.9/ngraphs) - 0.05);
	graph_argv[graph_argc++] = buf1;

	graph_argv[graph_argc++] = "--right-shift";
	graph_argv[graph_argc++] = "0.05";
	graph_argv[graph_argc++] = "--upward-shift";
	graph_argv[graph_argc++] = "0.05";

	graph_argv[graph_argc++] = "--toggle-round-to-next-tick";
	graph_argv[graph_argc++] = "X";
	graph_argv[graph_argc++] = "--font-size";
	graph_argv[graph_argc++] = "0.03";
	graph_argv[graph_argc++] = "--grid-style";
	graph_argv[graph_argc++] = "3";

	for(i = 0; i < ngraphs; i++) {
		if(i) {
			graph_argv[graph_argc++] = "--reposition";
			graph_argv[graph_argc++] = "0";
			sprintf(buf2, "%.3f", i * 0.9/ngraphs );
			graph_argv[graph_argc++] = strdup(buf2);
			graph_argv[graph_argc++] = "1";
		}
		graph_argv[graph_argc++] = dfnames[i];
	}
	graph_argv[graph_argc++] = NULL;
	printf("running %s", graph_prog);
	for(i = 0; i < graph_argc-1; i++) {
		printf(" %s", graph_argv[i]);
	}
	putchar('\n');
	
	outfd = open(file, O_WRONLY|O_CREAT, 0644);
	if(outfd < 0) {
		perror(file);
		goto done;
	}

	fflush(stdout);
	fflush(stderr);
	switch(pid = fork()) {
	case -1: /* error */
		perror("fork");
		break;

	case 0: /* child */
		dup2(outfd, 1);
		close(outfd);
		execv(graph_prog, graph_argv);
		perror(graph_prog);
		_exit(127);
		break;

	default: /* parent */
		/* FIXME:tell
		   do this in such a way that we can keep the GUI running */
		rc = waitpid(pid, &status, 0);
		if(status != 0) {
			printf("%x exited with status=0x%x\n", 
			       graph_prog, status);
		/* FIXME:tell: pop up suitable messages instead of hex status*/
		}
		break;
	}
	
 done: 
	for(i = 0; i < ngraphs; i++) {
		/* unlink(dfnames[i]); */
		g_free(dfnames[i]);
	}
	g_free(dfnames);
	g_free(graph_argv);
}


/* glue routines to gtk menus */
void
cmd_export_postscript(GtkWidget *w)
{
	cmd_print("gwave_out.ps", "ps", 0, 0);
}

void
cmd_export_pnm(GtkWidget *w)
{
	cmd_print("gwave_out.pnm", "pnm", 0, 0);

}
