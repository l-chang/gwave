/*
 * test routine for WaveFile data file readers
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.2  1998/09/17 18:25:09  tell
 * prints out variable type
 *
 * Revision 1.1  1998/08/31 21:00:28  tell
 * Initial revision
 *
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <glib.h>

#include "wavefile.h"

int
main(int argc, char **argv)
{
	WaveFile *wf;
	int i, j;
	extern int optind;
	extern char *optarg;
	int v_flag = 0;
	int x_flag = 0;
	int l_flag = 0;
	int errflg = 0;
	char *filetype = NULL;
	int c;

	while ((c = getopt (argc, argv, "lt:vx")) != EOF) {
		switch(c) {
		case 'v':
			v_flag = 1;
			break;
		case 'l':
			l_flag = 1;
			break;
		case 't':
			filetype = optarg;
			break;
		case 'x':
			x_flag = 1;
			break;
		default:
			errflg = 1;
			break;
		}
	}

	if(errflg || optind >= argc)  {
		fprintf(stderr, "usage: %s [-ltvx] file\n", argv[0]);
		exit(1);
	}
	
	spicestream_msg_level = DBG;
	wf = wf_read(argv[optind], filetype);
	if(!wf) {
		if(errno)
			perror(argv[1]);
		fprintf(stderr, "test_read: unable to read file\n");
		exit(1);
	}
	printf("filename: \"%s\"\n", wf->wf_filename);
	printf("independent variable:\n");
	printf("  name: \"%s\"\n", wf->iv->wv_name);
	printf("  type: %s\n", vartype_name_str(wf->iv->wv_type));
	printf("  npts: %d\n", wf->nvalues);
	printf("  min: %g\n", wf->iv->wds->min);
	printf("  max: %g\n", wf->iv->wds->max);
	printf("  blocks: %d/%d\n", wf->iv->wds->bpused, wf->iv->wds->bpsize);

	printf("columns: %d\n", wf->wf_ncols);
	printf("dependent variables: %d\n", wf->wf_ndv);
	for(i = 0; i < wf->wf_ndv; i++) {
		printf(" dv[%d] \"%s\" ", i, wf->dv[i].wv_name);
		printf(" (type=%s)", vartype_name_str(wf->dv[i].wv_type));
		if(wf->dv[i].wv_ncols > 1)
			printf(" (%d columns)\n", wf->dv[i].wv_ncols);
		for(j = 0; j < wf->dv[i].wv_ncols; j++) {
			if(wf->dv[i].wv_ncols > 1)
				printf("    col[%d] ", j);
			printf("blocks=%d/%d ",
			       wf->dv[i].wds[j].bpused, wf->dv[i].wds[j].bpsize);
			printf("min=%g ", wf->dv[i].wds[j].min);
			printf("max=%g ", wf->dv[i].wds[j].max);
			printf("first=%g ", wds_get_point(&wf->dv[i].wds[j], 0));
			printf("last=%g\n", wds_get_point(&wf->dv[i].wds[j],
						 wf->nvalues-1));
		}
	}

	if(l_flag) {
		putchar('\n');
		printf("      %10s", wf->iv->wv_name);
		for(i = 0; i < wf->wf_ndv; i++) {
			printf(" %10s", wf->dv[i].wv_name);
		}
		putchar('\n');
		for(j = 0; j < wf->nvalues; j++) {
			printf("[%3d] %10g", j, wds_get_point(wf->iv->wds, j));
			for(i = 0; i < wf->wf_ndv; i++) {
				printf(" %10g", 
				       wds_get_point(&wf->dv[i].wds[0], j));
			}
			putchar('\n');
		}
	}

	if(v_flag) {
		double mytm;
		double delta;
		int idx;

		mytm = wds_get_point(wf->iv->wds, 0);
		delta = (wds_get_point(wf->iv->wds, wf->nvalues-1) - mytm) / 40;
		printf("40 divisions, delta=%g\n", delta);
		for(i = 0; i < 41; i++, mytm += delta) {
			idx = wf_find_point(wf->iv, mytm);
			printf("last %8s < %8.4g is %8.4g at [%2d];",
			       wf->iv->wv_name,
			       mytm,
			       wds_get_point(wf->iv->wds, idx),
			       idx);
			fflush(stdout);
			printf("%8s at %8s=%8.4g is %8.4g\n",
			       wf->dv[0].wv_name,
			       wf->iv->wv_name,
			       mytm,
			       wv_interp_value(&wf->dv[0], mytm));
		}
   	}

	exit(0);
}
