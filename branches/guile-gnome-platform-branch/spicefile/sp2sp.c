/*
 * sp2sp - test program for spicestream library and
 * rudimentary spicefile format converter.
 *
 * Copyright (C) 1998,1999  Stephen G. Tell
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
 * License along with this program; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <errno.h>
#include <glib.h>
#include <limits.h>
#include "spicestream.h"

#define SWEEP_NONE 0
#define SWEEP_PREPEND 1
#define SWEEP_HEAD 2

static const char NPY_MAGIC[] = "\x93NUMPY";
static const int NPY_MAJOR = 1;
static const int NPY_MINOR = 0;
static const int NPY_MAX_HDR_LEN = 256 * 256;
static const int NPY_PREAMBLE_LEN = 6 + 1 + 1 + 2;

#if __BYTE_ORDER == __LITTLE_ENDIAN
static const char NPY_ENDIAN_CHAR = '<';
#else
static const char NPY_ENDIAN_CHAR = '>';
#endif


int g_verbose = 0;
int sweep_mode = SWEEP_PREPEND;
char *progname = "sp2sp";

static void ascii_header_output(SpiceStream *sf, int *enab, int nidx, FILE *of);
static void ascii_data_output(SpiceStream *sf, int *enab, int nidx,
			      double begin_val, double end_val, int ndigits, FILE *of);
static void numpy_output(SpiceStream *sf, int *enab, int nidx,
			 double begin_val, double end_val, int ndigits, FILE *of);
static int numpy_header_output(int ndims, int* shape, int len, FILE *of);
static int numpy_footer_output(SpiceStream *sf, int *indices, int nidx,
		                       int *sweeprows, FILE *of);
static int parse_field_numbers(int **index, int *idxsize, int *nsel,
			       char *list, int nfields);
static int parse_field_names(int **index, int *idxsize, int *nsel,
			     char *list, SpiceStream *sf);
static VarType get_vartype_code(char *vartype);

static void usage()
{
	int i;
	char *s;

	fprintf(stderr, "usage: %s [options] file\n", progname);
	fprintf(stderr, " options:\n");
	fprintf(stderr, "  -b V          begin output after independent-variable value V is reached\n");
	fprintf(stderr, "                instead of start of input\n");
	fprintf(stderr, "  -c T          Convert output to type T\n");
	fprintf(stderr, "  -d N          use N significant digits in output\n");
	fprintf(stderr, "  -e V          stop after independent-variable value V is reached\n");
	fprintf(stderr, "                instead of end of input.\n");
  
	fprintf(stderr, "  -f f1,f2,...  Output only fields named f1, f2, etc.\n");
	fprintf(stderr, "  -n n1,n2,...  Output only fields n1, n2, etc;\n");
	fprintf(stderr, "                independent variable is field number 0\n");
	fprintf(stderr, "  -o file       Output to named file instead of default '-' stdout.\n");
	fprintf(stderr, "  -u U          Output only variables with units of type; U\n");
	fprintf(stderr, "                U = volts, amps, etc.\n");
	fprintf(stderr, "  -s S          Handle sweep parameters as S:\n");
	fprintf(stderr, "  -s head         add header-like comment line\n");
	fprintf(stderr, "  -s prepend      prepend columns to all output lines\n");
	fprintf(stderr, "  -s none         ignore sweep info\n");
	fprintf(stderr, "  -t T          Assume that input is of type T\n");
	fprintf(stderr, "  -v            Verbose - print detailed signal information\n");
	fprintf(stderr, " output format types:\n");
	fprintf(stderr, "   none - no data output\n");
	fprintf(stderr, "   ascii - lines of space-seperated numbers, with header\n");
	fprintf(stderr, "   nohead - lines of space-seperated numbers, no headers\n");
	fprintf(stderr, "   cazm - CAzM format\n");
	fprintf(stderr, "   numpy - .npy data followed by python str(dict) and footer len uint16\n");
	fprintf(stderr, " input format types:\n");
	
	i = 0;
	while(s = ss_filetype_name(i++)) {
		fprintf(stderr, "    %s\n", s);
	}
}

int
main(int argc, char **argv)
{
	SpiceStream *sf;
	FILE *of;

	int i;
	int idx;
	extern int optind;
	extern char *optarg;
	int x_flag = 0;
	int errflg = 0;
	char *infiletype = "hspice";
	char *outfiletype = "ascii";
	char *outfilename = "-";
	char *fieldnamelist = NULL;
	char *fieldnumlist = NULL;
	int *out_indices = NULL;
	int outi_size = 0;
	int nsel;
	VarType vartype = UNKNOWN;
	int c;
	int ndigits = 7;
	double begin_val = -DBL_MAX;
	double end_val = DBL_MAX;

	while ((c = getopt (argc, argv, "b:c:d:e:f:n:s:t:u:o:vx")) != EOF) {
		switch(c) {
		case 'v':
			spicestream_msg_level = DBG;
			g_verbose = 1;
			break;
		case 'b':
			begin_val = atof(optarg);
			break;
		case 'c':
			outfiletype = optarg;
			break;
		case 'd':
			ndigits = atoi(optarg);
			if(ndigits < 5)
				ndigits = 5;
			break;
		case 'e':
			end_val = atof(optarg);
			break;
		case 'f':
			fieldnamelist = optarg;
			break;
		case 'n':
			fieldnumlist = optarg;
			break;
		case 's':
			if(strcmp(optarg, "none") == 0)
				sweep_mode = SWEEP_NONE;
			else if(strcmp(optarg, "prepend") == 0)
				sweep_mode = SWEEP_PREPEND;
			else if(strcmp(optarg, "head") == 0)
				sweep_mode = SWEEP_HEAD;
			else {
				fprintf(stderr, "unknown sweep-data style %s\n", optarg);
				exit(1);
			}
			break;
		case 't':
			infiletype = optarg;
			break;
		case 'u':
			vartype = get_vartype_code(optarg);
			break;
		case 'x':
			spicestream_msg_level = DBG;
			x_flag = 1;
			break;
		case 'o':
			outfilename = optarg;
			break;
		default:
			errflg = 1;
			break;
		}
	}

	if(errflg || optind >= argc)  {
		usage();
		exit(1);
	}

	sf = ss_open(argv[optind], infiletype);
	if(!sf) {
		if(errno)
			perror(argv[optind]);
		fprintf(stderr, "unable to read file\n");
		exit(1);
	}
	if(g_verbose) {
		printf("filename: \"%s\"\n", sf->filename);
		printf("  columns: %d\n", sf->ncols);
		printf("  tables: %d\n", sf->ntables);
		printf("independent variable:\n");
		printf("  name: \"%s\"\n", sf->ivar->name);
		printf("  type: %s\n", vartype_name_str(sf->ivar->type));
		printf("  col: %d\n", sf->ivar->col);
		printf("  ncols: %d\n", sf->ivar->ncols);
		printf("sweep parameters: %d\n", sf->nsweepparam);
		for(i = 0; i < sf->nsweepparam; i++) {
			printf("  name: \"%s\"\n", sf->spar[i].name);
			printf("  type: %s\n", vartype_name_str(sf->spar[i].type));
		}
		printf("dependent variables: %d\n", sf->ndv);
		for(i = 0; i < sf->ndv; i++) {
			SpiceVar *dvar;
			dvar = ss_dvar(sf, i);
			printf(" dv[%d] \"%s\" ", i, dvar->name);
			printf(" (type=%s col=%d ncols=%d)\n", 
			       vartype_name_str(dvar->type),
			       dvar->col,
			       dvar->ncols);
		}
	}

	if(strcmp(outfilename, "-") == 0) {
		if(strcmp(outfiletype, "numpy") == 0) {
			fprintf(stderr, "cannot output to stdout for 'numpy' format, use -o\n");
			exit(1);
		}
		of = stdout;
	} else {
		of = (FILE *)fopen64(outfilename, "w"); /* DJW: why is the cast needed? */
		if(!of) {
			if(errno)
				perror(outfilename);
			fprintf(stderr, "unable to open output file\n");
			exit(1);
		}
	}

	if(fieldnamelist == NULL && fieldnumlist == NULL) {
		out_indices = g_new0(int, sf->ndv+1);
		nsel = 0;
		idx = 0;
		for(i = 0; i < sf->ndv+1; i++) {
			SpiceVar *dvar;
			dvar = ss_dvar(sf, i-1);

			if(i == 0 || 
			   (vartype == UNKNOWN 
			    || dvar->type == vartype)) {
				out_indices[idx++] = i;
				nsel++;
			}
		}
	}
	if(fieldnumlist)
		if(parse_field_numbers(&out_indices, &outi_size, &nsel, 
				    fieldnumlist, sf->ndv+1) < 0)
			exit(1);
	if(fieldnamelist)
		if(parse_field_names(&out_indices, &outi_size, &nsel,
				  fieldnamelist, sf) < 0)
			exit(1);
	if(nsel == 0) {
		fprintf(stderr, "No fields selected for output\n");
		exit(0);
	}

	if(strcmp(outfiletype, "cazm") == 0) {
		fprintf(of, "* CAZM-format output converted with sp2sp\n");
		fprintf(of, "\n");
		fprintf(of, "TRANSIENT ANALYSIS\n");
		ascii_header_output(sf, out_indices, nsel, of);
		ascii_data_output(sf, out_indices, nsel, begin_val, end_val, ndigits, of);
	} else if(strcmp(outfiletype, "ascii") == 0) {
		ascii_header_output(sf, out_indices, nsel, of);
		ascii_data_output(sf, out_indices, nsel, begin_val, end_val, ndigits, of);
	} else if(strcmp(outfiletype, "nohead") == 0) {
		ascii_data_output(sf, out_indices, nsel, begin_val, end_val, ndigits, of);
	} else if(strcmp(outfiletype, "numpy") == 0) {
		numpy_output(sf, out_indices, nsel, begin_val, end_val, ndigits, of);
	} else if(strcmp(outfiletype, "none") == 0) {
		/* do nothing */
	} else {
		fprintf(stderr, "%s: invalid output type name: %s\n",
			progname, outfiletype);
	}

	ss_close(sf);
	close(of);

	exit(0);
}

/*
 * print all column headers.  
 * For multicolumn variables, ss_var_name will generate a column name
 * consisting of the variable name plus a suffix.
 */
static void
ascii_header_output(SpiceStream *sf, int *indices, int nidx, FILE *of)
{
	int i, j;
	char buf[1024];

	if((sf->nsweepparam > 0) && (sweep_mode == SWEEP_PREPEND)) {
		for(i = 0; i < sf->nsweepparam; i++) {
			fprintf(of, "%s ", sf->spar[i].name);	
		}
	}
	for(i = 0; i < nidx; i++) {
		if(i > 0)
			fputc(' ', of);
		if(indices[i] == 0) {
			ss_var_name(sf->ivar, 0, buf, 1024);
			fprintf(of, "%s", buf);
		} else {
			int varno = indices[i]-1;
			SpiceVar *dvar;
			dvar = ss_dvar(sf, varno);
			for(j = 0; j < dvar->ncols; j++) {
				if(j > 0)
					fputc(' ', of);
				ss_var_name(dvar, j, buf, 1024);
				fprintf(of, "%s", buf);
			}
		}
	}
	fputc('\n', of);
}

/*
 * print data as space-seperated columns.
 */
static void
ascii_data_output(SpiceStream *sf, int *indices, int nidx, 
		  double begin_val, double end_val, int ndigits, FILE *of)
{
	int i, j, tab;
	int rc;
	double ival;
	double *dvals;
	double *spar = NULL;
	int done;

	dvals = g_new(double, sf->ncols);
	if(sf->nsweepparam > 0)
		spar = g_new(double, sf->nsweepparam);
	
	done = 0;
	tab = 0;
	while(!done) {
		if(sf->nsweepparam > 0) {
			if(ss_readsweep(sf, spar) <= 0)
				break;
		}
		if(tab > 0 && sweep_mode == SWEEP_HEAD) {
			fprintf(of, "# sweep %d;", tab);
			for(i = 0; i < sf->nsweepparam; i++) {
				fprintf(of, " %s=%g", sf->spar[i].name, spar[i]);
			}
			fputc('\n', of);
		}
		while((rc = ss_readrow(sf, &ival, dvals)) > 0) {
			if(ival < begin_val)
				continue;
			if(ival > end_val) {
				/* past end_val, but can only stop reading
				   early if if there is only one sweep-table
				   in the file. */ 
				if(sf->ntables == 1)
					break;
				else
					continue;
			}

			if((sf->nsweepparam > 0) && (sweep_mode == SWEEP_PREPEND)) {
				for(i = 0; i < sf->nsweepparam; i++) {
					fprintf(of, "%.*g ", ndigits, spar[i]);
				}
			}
			for(i = 0; i < nidx; i++) {
				if(i > 0)
					fputc(' ', of);
				if(indices[i] == 0)
					fprintf(of, "%.*g", ndigits, ival);
				else {
					int varno = indices[i]-1;
					SpiceVar *dvar = ss_dvar(sf, varno);
					int dcolno = dvar->col - 1;
					for(j = 0; j < dvar->ncols; j++) {
						if(j > 0)
							fputc(' ', of);
						fprintf(of, "%.*g", ndigits,
						       dvals[dcolno+j]);
					}
				}
			}
			fputc('\n', of);
		}
		if(rc == -2) {  /* end of sweep, more follow */
			if(sf->nsweepparam == 0)
				sweep_mode = SWEEP_HEAD;
			tab++;
		} else {  	/* EOF or error */
			done = 1;
		}
	}
	g_free(dvals);
	if(spar)
		g_free(spar);
}

static int
numpy_header_output(int ndims, int *shape, int len, FILE *of)
{
    /* More documentation about the npy format at numpy/lib/format.py */

    char header[NPY_MAX_HDR_LEN];
    char descr[5];
    int i;
    int hlen;
    int nspaces = 0;

    strcpy(header, "{'descr':'");

    descr[0] = NPY_ENDIAN_CHAR;
    descr[1] = 'f';
    sprintf(descr+2, "%d", (int) sizeof(float));

    strcat(header, descr);
    strcat(header, "', 'fortran_order':False, 'shape': (");
    for(i=0; i < ndims; i++) {
		hlen = strlen(header);
		sprintf(header+hlen, "%d", shape[i]);

		if(i != ndims-1) {
			strcat(header, ", ");
		}
    }
    strcat(header, ") }");
    hlen = strlen(header);

    if(len == -1) {
		/* bogus header values or just write the length needed */
		nspaces = 16 - ((NPY_PREAMBLE_LEN + hlen + 1) % 16);

    } else if(len < hlen) {
		fprintf(stderr, "numpy_header_output: requested header len is too small\n");

    } else if(((len + NPY_PREAMBLE_LEN) % 16) != 0) {
		fprintf(stderr, "requested header len does not align to mult of 16\n");

    } else {
		/* pad to the (longer) header length */
		nspaces = len - hlen - 1;
    }

    if(hlen + nspaces + 1 > NPY_MAX_HDR_LEN) {
		fprintf(stderr, "npy header too long (%d)\n", hlen+nspaces+1);
		return 0;
    }

    for(i=0; i < nspaces; i++) {
		strcat(header, " ");
    }
    strcat(header, "\n");
    hlen = strlen(header);

    /* preamble */
    fprintf(of, NPY_MAGIC);
    fwrite(&NPY_MAJOR, sizeof(char), 1, of);
    fwrite(&NPY_MINOR, sizeof(char), 1, of);
    fputc((0xff & hlen), of);
    fputc((0xff & (hlen >> 8)), of);
    fwrite(header, sizeof(char), hlen, of);

    return hlen;
}


static int
numpy_footer_output(SpiceStream *sf, int *indices, int nidx, int *sweeprows, FILE *of)
{
	int i, j;
	int foot_start = 0;
	int foot_len = 0;
	char buf[1024];
	double *spar = NULL;

	foot_start = ftell(of);

	/*
	 * make footer dict
	 */
	fprintf(of, "{ ");

	/* sweep variables, these values prepend the data columns for each row */
	fprintf(of, "'sweepvars':(");
	/*for(i=0; i < sf->ntables; i++) {*/
	if(sf->ntables > 1) {
		for(j = 0; j < sf->nsweepparam; j++) {
			fprintf(of, "'%s'", sf->spar[j].name);
			fprintf(of, ",");
		}
	}
	fprintf(of, "), ");

	fprintf(of, "'sweeprows':(");
	if(sf->ntables > 1) {
		for(i=0; i < sf->ntables; i++) {
			fprintf(of, "(%d,%d),", sweeprows[2*i], sweeprows[2*i+1]);
		}
	}
	fprintf(of, "), ");

	fprintf(of, "'cols':(");
	for(i = 0; i < nidx; i++) {
		if(indices[i] == 0) {
			ss_var_name(sf->ivar, 0, buf, 1024);
			fprintf(of, "'%s', ", buf);
		} else {
			int varno = indices[i]-1;
			SpiceVar *dvar = ss_dvar(sf, varno);
			int dcolno = dvar->col - 1;
			for(j = 0; j < dvar->ncols; j++) {
				ss_var_name(dvar, j, buf, 1024);
				fprintf(of, "'%s', ", buf);
			}
		}
	}
	fprintf(of, ") }");

	/* space-pad to end on a 16 byte boundary */
	while(((ftell(of)+3) % 16) != 0) {
		fputc(' ', of);
	}
	fputc('\n', of);
	foot_len = ftell(of) - foot_start + 2;
	fputc((0xFF & foot_len), of);
	fputc((0xFF & (foot_len >> 8)), of);

	return foot_len;
}



/*
 * print data as a .npy format array
 * See numpy/lib/format.py for details of the .npy file format.
 */
static void
numpy_output(SpiceStream *sf, int *indices, int nidx, 
		  double begin_val, double end_val, int ndigits, FILE *of)
{
	int i, j, tab;
	int rc;
	int npy_hlen = 0;
	int foot_len = 0;
	int npy_end = 0;
	int ndims = 0, ncols = 0, nrows = 0;
	int *sweeprows;
	int shape[3];
	char buf[1024];
	char npy_descr[5], npy_preamble[NPY_PREAMBLE_LEN], npy_header[NPY_MAX_HDR_LEN];
	float val;
	double ival;
	double *dvals;
	double *spar = NULL;
	int done;


	/*
	 * write sham npy preamble + header to reserve space, don't know nrows yet
	 */
	ndims = 2;
	shape[0] = INT_MAX;
	shape[1] = INT_MAX;
	npy_hlen = numpy_header_output(ndims, shape, -1, of);

	/* now write rows */
	dvals = g_new(double, sf->ncols);
	sweeprows = g_new(int, 2 * sf->ntables);
	if(sf->nsweepparam > 0)
		spar = g_new(double, sf->nsweepparam);
	
	done = 0;
	tab = 0;
	nrows = 0;
	sweeprows[2*tab] = nrows;
	while(!done) {
		if(sf->nsweepparam > 0) {
			if(ss_readsweep(sf, spar) <= 0)
				break;
		}

		while((rc = ss_readrow(sf, &ival, dvals)) > 0) {
			if(ival < begin_val)
				continue;
			if(ival > end_val) {
				/* past end_val, but can only stop reading
				   early if if there is only one sweep-table
				   in the file. */ 
				if(sf->ntables == 1)
					break;
				else
					continue;
			}


			if(sf->nsweepparam > 0) {
				for(i = 0; i < sf->nsweepparam; i++) {
					val = (float)spar[i];
					fwrite(&val, sizeof(float), 1, of);
				}
			}
			for(i = 0; i < nidx; i++) {
				if(indices[i] == 0) {
					val = (float)ival;
					fwrite(&val, sizeof(float), 1, of);
				} else {
					int varno = indices[i]-1;
					SpiceVar *dvar = ss_dvar(sf, varno);
					int dcolno = dvar->col - 1;
					for(j = 0; j < dvar->ncols; j++) {
						val = (float)dvals[dcolno+j];
						fwrite(&val, sizeof(float), 1, of);
					}
				}
			}
			nrows += 1;
		}
		if(rc == -2) {  /* end of sweep, more follow */
			if(sf->nsweepparam == 0)
				sweep_mode = SWEEP_HEAD;

			sweeprows[2*tab+1] = nrows-1;
			tab++;
			sweeprows[2*tab] = nrows;
		} else {  	/* EOF or error */
			done = 1;
			sweeprows[2*tab+1] = nrows-1;
		}
	}

	npy_end = ftell(of);


	/* write trailing string representation of a python dict describing the data */
	foot_len = numpy_footer_output(sf, indices, nidx, sweeprows, of);


	/*
	 * go back and overwrite npy header with correct values
	 */
	fseek(of, 0, SEEK_SET);

	/* calc num cols of data, second dimension */
	ncols = sf->nsweepparam;
	for(i = 0; i < nidx; i++) {
		if(indices[i] == 0) { /* independent var */
			ncols += 1;
		} else {
			int varno = indices[i]-1;
			SpiceVar *dvar = ss_dvar(sf, varno);
			ncols += dvar->ncols;
		}
	}
	shape[0] = nrows;
	shape[1] = ncols;
	int npy_hlen2 = numpy_header_output(ndims, shape, npy_hlen, of);

	if(npy_hlen2 != npy_hlen) {
	    fprintf(stderr, "numpy OOPS, inconsistent header lengths\n");
	}

	fclose(of);

	g_free(dvals);
	g_free(sweeprows);
	if(spar)
		g_free(spar);
}





static int parse_field_numbers(int **indices, int *idxsize, int *nidx, 
			       char *list, int nfields)
{
	int n, i;
	char *fnum;
	int err = 0;
	int *idx;
	if(!*indices || idxsize == 0) {
		*idxsize = nfields*2;
		idx = g_new0(int, *idxsize);
		*indices = idx;
		*nidx = 0;
	}

	fnum = strtok(list, ", \t");
	i = 0;
	while(fnum) {
		if(*nidx >= *idxsize) {
			*idxsize *= 2;
			idx = g_realloc(idx, (*idxsize) * sizeof(int));
			*indices = idx;
		}
		n = atoi(fnum);
		if(n < 0 || n >= nfields) {
			fprintf(stderr, "bad field number in -n option: %s\n", fnum);
			err = -1;
		} else {
			idx[i++] = n;
			(*nidx)++;
		}
		fnum = strtok(NULL, ", \t");
	}
	return err;
}


/*
 * Try looking for named dependent variable.  Try twice, 
 * first as-is, then with "v(" prepended the way hspice mangles things.
 */
static int find_dv_by_name(char *name, SpiceStream *sf)
{
	int i;
	SpiceVar *dvar;

	for(i = 0; i < sf->ndv; i++) {
		dvar = ss_dvar(sf, i);
		if(strcasecmp(name, dvar->name) == 0)
			return i;
	}
	for(i = 0; i < sf->ndv; i++) {
		dvar = ss_dvar(sf, i);
		if(strncasecmp("v(", dvar->name, 2) == 0
		   && strcasecmp(name, &dvar->name[2]) == 0)
			return i;
	}
	return -1;
}

/*
 * parse comma-seperated list of field names.  Turn on the output-enables
 * for the listed fields.
 */
static int parse_field_names(int **indices, int *idxsize, int *nidx,
			     char *list, SpiceStream *sf)
{
	int err = 0;
	int n;
	char *fld;
	int i;
	int *idx;

	if(!*indices || idxsize == 0) {
		*idxsize = (sf->ndv+1)*2;
		idx = g_new0(int, *idxsize);
		*indices = idx;
		*nidx = 0;
	}

	fld = strtok(list, ", \t");
	i = 0;
	while(fld) {
		if(*nidx >= *idxsize) {
			*idxsize *= 2;
			idx = g_realloc(idx, (*idxsize) * sizeof(int));
			*indices = idx;
		}
		if(strcasecmp(fld, sf->ivar->name)==0) {
			idx[i++] = 0;
			(*nidx)++;
		} else if((n = find_dv_by_name(fld, sf)) >= 0) {
			idx[i++] = n+1;
			(*nidx)++;
		} else {
			fprintf(stderr, "field name in -f option not found in file: %s\n", fld);
			err = -1;
		}
		fld = strtok(NULL, ", \t");
	}
	return err;
}


struct vtlistel {
	VarType t;
	char *s;
};
static struct vtlistel vtlist[] = {
	{TIME, "time"},
	{VOLTAGE, "volt"},
	{VOLTAGE, "volts"},
	{VOLTAGE, "voltage"},
	{CURRENT, "current"},
	{CURRENT, "amps"},
	{FREQUENCY, "freq"},
	{FREQUENCY, "frequency"},
	{FREQUENCY, "hertz"},
	{UNKNOWN, NULL},
};

/*
 * Given a variable type name, return a numeric VarType.
 * Returns 0 (UNKNOWN) if no match.
 */
static VarType get_vartype_code(char *vartype)
{
	int i;
	for(i = 0; vtlist[i].s; i++) {
		if(strcasecmp(vartype, vtlist[i].s) == 0)
			return vtlist[i].t;
	}
	return UNKNOWN;
}
/*
 * vim:tabstop=4 noexpandtab
 */
