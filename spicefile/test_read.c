/*
 * test routine for WaveFile data file readers
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.5  2003/07/30 06:18:49  sgt
 * better handling of the last point in a wavevar,
 * in particular when wv_interp_val asks for a point beyond the end of the iv range
 * enhance test_read.c to
 *
 * Revision 1.4  2000/08/09 23:37:39  sgt
 * ss_hspice.c - wrote sf_guessrows_hsbin routine.
 * others - instrumented to count reallocs and print out the number.
 *
 * Revision 1.3  2000/01/07 05:04:48  tell
 * updating with old changes as we construct the CVS
 *
 * Revision 1.2  1998/09/17 18:25:09  tell
 * prints out variable type
 *
 * Revision 1.1  1998/08/31 21:00:28  tell
 * Initial revision
 *
 */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <glib.h>

#include "wavefile.h"

void test_interp(WvTable *wt, double mytm);
void dump_table_info(WvTable *wt);
void dump_wavevar(gpointer p, gpointer u);

int
main(int argc, char **argv)
{
	WaveFile *wf;
	WvTable *wt;
	int i, j;
	extern int optind;
	extern char *optarg;
	int a_flag = 0;
	int v_flag = 0;
	int x_flag = 0;
	int l_flag = 0;
	int errflg = 0;
	char *filetype = NULL;
	int c;

	while ((c = getopt (argc, argv, "alt:vx")) != EOF) {
		switch(c) {
		case 'a':
			a_flag = 1;
			break;
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
		fprintf(stderr, "usage: %s [-altvx] file\n", argv[0]);
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
	printf("wf_ntables: %d\n", wf->wf_ntables);

	for(i = 0; i < wf->wf_ntables; i++) {
		printf("table %d", i);
		wt = wf_wtable(wf, i);
		if(wt->name) {
			printf(" %s=%.3g", wt->name, wt->swval);
		}
		putchar('\n');
		dump_table_info(wt);
	}
	wf_foreach_wavevar(wf, dump_wavevar, NULL);

	wt = wf_wtable(wf, 0);
	if(a_flag && wt->wt_ndv > 2) {
		// test wf_add_var();
		WaveVar *dv1;
		WaveVar *dv2;
		WaveVar *dvn;
		char *nname;
		int name_len;

		printf("before add: ndv=%d\n", wt->wt_ndv);
		dv1 = wt_dv(wt, wt->wt_ndv-2);
		name_len = strlen(dv1->wv_name);
		dv2 = wt_dv(wt, wt->wt_ndv-1);
		name_len += strlen(dv2->wv_name);
		name_len += 18;
		nname = g_new0(char, name_len);

		sprintf(nname, "calc( %s - %s )", 
			dv1->wv_name,
			dv2->wv_name);
			
		wf_add_var(wf, nname, 1, MATH, NULL);
		printf("after add: ndv=%d\n", wt->wt_ndv);
		dvn = wt_dv(wt, wt->wt_ndv-1);

		for(j = 0; j < wt->nvalues; j++) {
			wf_set_point(&dvn->wds[0], j, 
				      wds_get_point(&dv1->wds[0], j) -
				      wds_get_point(&dv2->wds[0], j));
		}
	}

	if(l_flag) {
		int t;
		WaveVar *dv;
		for(t = 0; t < wf->wf_ntables; t++) {
			printf("table %d: ", t);
			wt = wf_wtable(wf, t);
			if(wt->name) {
				printf(" %s=%g", wt->name, wt->swval);
			}
			putchar('\n');
			printf("      %10s", wt->iv->wv_name);
			for(i = 0; i < wt->wt_ndv; i++) {
				dv = wt_dv(wt, i);
				printf(" %10s", dv->wv_name);
			}
			putchar('\n');
			for(j = 0; j < wt->nvalues; j++) {
				printf("[%3d] %10g", j, wds_get_point(wt->iv->wds, j));
				for(i = 0; i < wt->wt_ndv; i++) {
					dv = wt_dv(wt, i);
					printf(" %10g", 
					       wds_get_point(&dv->wds[0], j));
				}
				putchar('\n');
			}
		}
	}

	wt = wf_wtable(wf, 0);
	if(v_flag) {
		double mytm;
		double delta;

		mytm = wds_get_point(wt->iv->wds, 0);
		delta = (wds_get_point(wt->iv->wds, wt->nvalues-1) - mytm) / 40.0;
		printf("40 divisions, delta=%g\n", delta);
		for(i = 0; i <= 41; i++, mytm += delta) {
			test_interp(wt, mytm);
		}
		mytm = wds_get_point(wt->iv->wds, wt->nvalues-2);
		putchar('\n');
		test_interp(wt, mytm);
   	}
	wf_free(wf);
	exit(0);
}

void dump_table_info(WvTable *wt)
{
	WaveFile *wf = wt->wf;

	printf("independent variable:\n");
	printf("  name: \"%s\"\n", wt->iv->wv_name);
	printf("  type: %s\n", vartype_name_str(wt->iv->wv_type));
	printf("  npts: %d\n", wt->nvalues);
	printf("  min: %.3g\n", wt->iv->wds->min);
	printf("  max: %.3g\n", wt->iv->wds->max);
	printf("  blocks: %d/%d\n", wt->iv->wds->bpused, wt->iv->wds->bpsize);
	printf("  reallocs: %d\n", wt->iv->wds->nreallocs);

	printf("columns: %d\n", wf->wf_ncols);
	printf("dependent variables: %d\n", wf->wf_ndv);
}

void
dump_wavevar(gpointer p, gpointer u)
{
	WaveVar *wv = (WaveVar *)p;
	int j;

	printf(" dv \"%s\" ", wv->wv_name);
	printf(" (type=%s)", vartype_name_str(wv->wv_type));

	if(wv->wv_ncols > 1)
		printf(" (%d columns)\n", wv->wv_ncols);

	for(j = 0; j < wv->wv_ncols; j++) {
		if(wv->wv_ncols > 1)
			printf("    col[%d] ", j);
		printf("blocks=%d/%d ",
		       wv->wds[j].bpused, wv->wds[j].bpsize);
		printf("min=%.3g ",wv->wds[j].min);
		printf("max=%.3g ", wv->wds[j].max);
		printf("first=%.3g ", wds_get_point(&wv->wds[j], 0));
		printf("last=%.3g\n", wds_get_point(&wv->wds[j],
						  wv->wv_nvalues-1));
	}
}

void
test_interp(WvTable *wt, double mytm)
{
	int idx;
	WaveVar *wv;
	idx = wf_find_point(wt->iv, mytm);
	printf("last %8s < %14.8g is %14.8g at [%4d];",
	       wt->iv->wv_name,
	       mytm,
	       wds_get_point(wt->iv->wds, idx),
	       idx);
	fflush(stdout);
	wv = wt_dv(wt, 0);
	printf("%8s at %8s=%14.8g is %14.8g\n",
	       wv->wv_name,
	       wt->iv->wv_name,
	       mytm,
	       wv_interp_value(wv, mytm));
}
