/*
 * test routine for analog file readers
 *
 * $Log: not supported by cvs2svn $
 * Revision 1.1  1998/08/31 21:00:28  tell
 * Initial revision
 *
 */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <glib.h>
#include "reader.h"

int
main(int argc, char **argv)
{
	DataFile *df;
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
		fprintf(stderr, "usage: %s [-l] file\n", argv[0]);
		exit(1);
	}

	analog_read_debug = 1;
	df = analog_read_file(argv[optind], filetype);
	if(!df) {
		if(errno)
			perror(argv[1]);
		fprintf(stderr, "unable to read file\n");
		exit(1);
	}
	printf("filename: \"%s\"\n", df->filename);
	printf("independent variable:\n");
	printf("  name: \"%s\"\n", df->iv->d.name);
	printf("  type: %s\n", vartype_name_str(df->iv->d.type));
	printf("  npts: %d\n", df->iv->d.nvalues);
	printf("  min: %g\n", df->iv->d.min);
	printf("  max: %g\n", df->iv->d.max);
	printf("  blocks: %d/%d\n", df->iv->d.bpused, df->iv->d.bpsize);

	printf("dependent variables: %d\n", df->ndv);
	for(i = 0; i < df->ndv; i++) {
		printf(" dv[%d] \"%s\" ", i, df->dv[i]->d.name);
		printf(" (type=%s)", vartype_name_str(df->dv[i]->d.type));
		printf("(%d values) ", df->dv[i]->d.nvalues);
		printf("blocks=%d/%d\n",
		       df->dv[i]->d.bpused, df->dv[i]->d.bpsize);
		printf("   min=%g ", df->dv[i]->d.min);
		printf("max=%g ", df->dv[i]->d.max);
		printf("first=%g ", an_get_point(&df->dv[i]->d, 0));
		printf("last=%g\n", an_get_point(&df->dv[i]->d, df->dv[i]->d.nvalues-1));
	}

	if(l_flag) {
		putchar('\n');
		printf("      %10s", df->iv->d.name);
		for(i = 0; i < df->ndv; i++) {
			printf("%10s", df->dv[i]->d.name);
		}
		putchar('\n');
		for(j = 0; j < df->iv->d.nvalues; j++) {
			printf("[%3d] %10g", j, an_get_point(&df->iv->d, j));
			for(i = 0; i < df->ndv; i++) {
				printf(" %10g", an_get_point((DataSet *)df->dv[i], j));
			}
			putchar('\n');
		}
	}
	if(v_flag) {
		double mytm;
		double delta;
		int idx;

		mytm = an_get_point((DataSet *)df->iv, 0);
		delta = (an_get_point((DataSet *)df->iv, 0)
			+ an_get_point((DataSet *)df->iv, df->iv->d.nvalues-1)) / 40;
		printf("40 divisions, delta=%g\n", delta);
		for(i = 0; i < 41; i++, mytm += delta) {
			idx = an_find_point(df->iv, mytm);
			printf("last %s < %g is %g at [%d];",
			       df->iv->d.name,
			       mytm,
			       an_get_point((DataSet *)df->iv, idx),
			       idx);
			fflush(stdout);
			printf("%s at %s=%g is %g\n",
			       df->dv[0]->d.name,
			       df->iv->d.name,
			       mytm,
			       an_interp_value(df->dv[0], mytm));
		}
   	}

	exit(0);
}
