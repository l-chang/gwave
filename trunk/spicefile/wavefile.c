/*
 * wavefile.c - stuff for working with entire datasets of waveform data.
 *
 * Copyright 1999, Stephen G. Tell.
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

#include "ssintern.h"
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <config.h>
#ifdef HAVE_GTK
#include <glib.h>
#else
#include "glib_fake.h"
#endif
#include "wavefile.h"


#ifdef HAVE_POSIX_REGEXP
#include <regex.h>
#define REGEXP_T regex_t
#define regexp_test(c,s) (regexec((c), (s), 0, NULL, 0) == 0)
static regex_t *
regexp_compile(char *str)
{
	int err;
	char ebuf[128];
	regex_t *creg;

	creg = g_new(regex_t, 1);
	err = regcomp(creg, str, REG_NOSUB|REG_EXTENDED);
	if(err) {
		regerror(err, creg, ebuf, sizeof(ebuf));
		fprintf(stderr, "internal error (in regexp %s):\n", str);
		fprintf(stderr, "  %s\n", ebuf);
		exit(1);
	}
	return creg;
}

#else		
#include "regexp.h"	/* Henry Spencer's V8 regexp */
#define REGEXP_T regexp
#define regexp_test(c,s) regexec((c), (s))
#define regexp_compile(s) regcomp(s)
#endif

WaveFile *wf_finish_read(SpiceStream *ss);
void wf_init_dataset(WDataSet *ds);
inline void wf_set_point(WDataSet *ds, int n, double val);
void wf_free_dataset(WDataSet *ds);

typedef struct {
	char *name;
	char *fnrexp;
	REGEXP_T *creg;/* compiled form of regexp */
} DFormat;

/* table associating file typenames with filename regexps.
 * Typenames should be those supported by spicefile.c.
 *
 * Filename patterns are full egrep-style 
 * regular expressions, NOT shell-style globs.
 */
static DFormat format_tab[] = {
	{"hspice", "\\.(tr|sw|ac)[0-9]$" },
	{"cazm", "\\.[BNW]$" },
	{"spice3raw", "\\.raw$" },
	{"spice2raw", "\\.rawspice$" },
	{"ascii", "\\.(asc|acs|ascii)$" }, /* ascii / ACS format */
};
static const int NFormats = sizeof(format_tab)/sizeof(DFormat);

/*
 * Read a waveform data file.
 *  If the format name is non-NULL, only tries reading in specified format.
 *  If format not specified, tries to guess based on filename, and if
 *  that fails, tries all of the readers until one sucedes.
 *  Returns NULL on failure after printing an error message.
 * 
 * TODO: use some kind of callback or exception so that client
 * can put the error messages in a GUI or somthing.
 */
WaveFile *wf_read(char *name, char *format)
{
	FILE *fp;
	SpiceStream *ss;
	int i;

	unsigned int tried = 0; /* bitmask of formats. */

	g_assert(NFormats <= 8*sizeof(tried));
	fp = fopen64(name, "r");
	if(fp == NULL) {
		perror(name);
		return NULL;
	}

	if(format == NULL) {
		for(i = 0; i < NFormats; i++) {
			if(!format_tab[i].creg) {
				format_tab[i].creg = regexp_compile(format_tab[i].fnrexp);
			}
			if(regexp_test(format_tab[i].creg, name))
			{
				tried |= 1<<i;
				ss = ss_open_internal(fp, name, format_tab[i].name);
				if(ss) {
					ss_msg(INFO, "wf_read", "%s: read with format \"%s\"", name, format_tab[i].name);
					return wf_finish_read(ss);
				}

				if(fseek(fp, 0L, SEEK_SET) < 0) {
					perror(name);
					return NULL;
				}
					
			}
		}
		if(tried == 0)
			ss_msg(INFO, "wf_read", "%s: couldn't guess a format from filename suffix.", name);
		/* no success with formats whose regexp matched filename,
		* try the others.
		*/
		for(i = 0; i < NFormats; i++) {
			if((tried & (1<<i)) == 0) {
				ss = ss_open_internal(fp, name, format_tab[i].name);
				if(ss)
					return wf_finish_read(ss);
				tried |= 1<<i;
				if(fseek(fp, 0L, SEEK_SET) < 0) {
					perror(name);
					return NULL;
				}
			}
		}
		ss_msg(ERR, "wf_read", "%s: couldn't read with any format\n", name);
		return NULL;
	} else { /* use specified format only */
		ss = ss_open_internal(fp, name, format);
		if(ss)
			return wf_finish_read(ss);
		else
			return NULL;
	}
}

/* 
 * read all of the data from a SpiceStream and store it in the WaveFile
 * structure
 */
WaveFile *wf_finish_read(SpiceStream *ss)
{
	WaveFile *wf;
	int row;
	int i, j, rc;
	double ival;
	double last_ival;
	double *dvals;
	WaveVar *dv;

	wf = g_new0(WaveFile, 1);
	wf->ss = ss;

	wf->iv = g_new0(WaveVar, 1);
	wf->iv->sv = ss->ivar;
	wf->iv->wfile = wf;
	wf->iv->wds = g_new0(WDataSet, 1);
	wf_init_dataset(wf->iv->wds);

	wf->dv = g_new0(WaveVar, wf->ss->ndv);
	for(i = 0; i < wf->ss->ndv; i++) {
		wf->dv[i].wfile = wf;
		wf->dv[i].sv = &ss->dvar[i];
		wf->dv[i].wds = g_new0(WDataSet, wf->dv[i].sv->ncols);
		for(j = 0; j < wf->dv[i].sv->ncols; j++)
			wf_init_dataset(&wf->dv[i].wds[j]);
	}
	
	dvals = g_new(double, ss->ncols);
	row= 0;
	wf->nvalues = 0;
	last_ival = -1.0e29;
	while((rc = ss_readrow(ss, &ival, dvals)) > 0) {
		if(row > 0 && ival < last_ival) {
			ss_msg(ERR, "wavefile_read", "independent variable is not nondecreasing at row %d; ival=%g last_ival=%g\n", row, ival, last_ival);
			rc = -1;
			break;
		}
		last_ival = ival;
		wf_set_point(wf->iv->wds, row, ival);
		for(i = 0; i < wf->ss->ndv; i++) {
			dv = &wf->dv[i];
			for(j = 0; j < dv->wv_ncols; j++)
				wf_set_point(&dv->wds[j], row,
					     dvals[dv->sv->col - 1 + j ]);
		}
		row++;
		wf->nvalues++;
	}
	g_free(dvals);
	ss_close(ss);
	if(rc == -2) {
		ss_msg(WARN, "wavefile_read", "Multiple sweeps found; ignoring all but the first one");
		return wf;		
	} else if(rc < 0) {
		wf_free(wf);
		return NULL;
	} else {
		return wf;
	}
}

/* 
 * Free all memory used by a WaveFile
 */
void
wf_free(WaveFile *wf)
{
	int i;

	wf_free_dataset(wf->iv->wds);
	g_free(wf->iv);
	for(i = 0; i < wf->ss->ndv; i++)
		wf_free_dataset(wf->dv[i].wds);
	g_free(wf->dv);
	ss_delete(wf->ss);
	g_free(wf);
}

/*
 * initialize common elements of WDataSet structure 
 */ 
void
wf_init_dataset(WDataSet *ds)
{
	ds->min = G_MAXDOUBLE;
	ds->max = -G_MAXDOUBLE;
	
	ds->bpsize = DS_INBLKS;
	ds->bptr = g_new0(double *, ds->bpsize);
	ds->bptr[0] = g_new(double, DS_DBLKSIZE);
	ds->bpused = 1;
	ds->nreallocs = 0;
}

/*
 * free up memory pointed to by a DataSet, but not the dataset itself.
 */
void
wf_free_dataset(WDataSet *ds)
{
	int i;
	for(i = 0; i < ds->bpused; i++)
		if(ds->bptr[i])
			g_free(ds->bptr[i]);
	g_free(ds->bptr);
	g_free(ds);
}

/*
 * expand dataset's storage to add one more block.
 */
void
wf_expand_dset(WDataSet *ds)
{
	if(ds->bpused >= ds->bpsize) {
		ds->bpsize *= 2;
		ds->bptr = g_realloc(ds->bptr, ds->bpsize * sizeof(double*));
		ds->nreallocs++;
	}
	ds->bptr[ds->bpused++] = g_new(double, DS_DBLKSIZE);
}

/*
 * set single value in dataset.   Probably can be inlined.
 */
void
wf_set_point(WDataSet *ds, int n, double val)
{
	int blk, off;
	blk = ds_blockno(n);
	off = ds_offset(n);
	while(blk >= ds->bpused)
		wf_expand_dset(ds);

	ds->bptr[blk][off] = val;
	if(val < ds->min)
		ds->min = val;
	if(val > ds->max)
		ds->max = val;
}

/*
 * get single point from dataset.   Probably can be inlined.
 */
double
wds_get_point(WDataSet *ds, int n)
{
	int blk, off;
	blk = ds_blockno(n);
	off = ds_offset(n);
	g_assert(blk <= ds->bpused);
	g_assert(off < DS_DBLKSIZE);

	return ds->bptr[blk][off];
}

/*
 * Use a binary search to return the index of the point 
 * whose value is the largest not greater than ival.  
 * if ival is equal or greater than the max value of the
 * independent variable, return the index of the last point.
 *
 * Only works on independent-variables, which we require to
 * be nondecreasing and have only a single column.
 * 
 * Further, if there are duplicate values, returns the highest index
 * that has the same value.
 */
int
wf_find_point(WaveVar *iv, double ival)
{
	WDataSet *ds = iv->wds;
	double cval;
	int a, b;
	int n = 0;

	a = 0;
	b = iv->wfile->nvalues - 1;
	if(ival >= ds->max)
		return b;
	while(a+1 < b) {
		cval = wds_get_point(ds, (a+b)/2);
/*		printf(" a=%d b=%d ival=%g cval=%g\n", a,b,ival,cval); */
		if(ival < cval)
			b = (a+b)/2;
		else
			a = (a+b)/2;


		g_assert(n++ < 32);  /* > 2 ** 32 points?  must be a bug! */
	}
	return a;
}

/*
 * return the value of the dependent variable dv at the point where
 * its associated independent variable has the value ival.
 *
 * FIXME:tell 
 * make this fill in an array of dependent values,
 * one for each column in the specified dependent variable.
 * This will be better than making the client call us once for each column,
 * because we'll only have to search for the independent value once.
 * (quick hack until we need support for complex and other multicolumn vars:
 * just return first column's value.)
 */
double
wv_interp_value(WaveVar *dv, double ival)
{
	int li, ri;   /* index of points to left and right of desired value */
	double lx, rx;  /* independent variable's value at li and ri */
	double ly, ry;  /* dependent variable's value at li and ri */
	WaveVar *iv;

	iv = dv->wv_iv;

   	li = wf_find_point(iv, ival);
	ri = li + 1;
	if(ri >= dv->wfile->nvalues)
		return wds_get_point(dv->wds, dv->wfile->nvalues-1);

	lx = wds_get_point(&iv->wds[0], li);
	rx = wds_get_point(&iv->wds[0], ri);
/*	g_assert(lx <= ival); */
	if(li > 0 && lx > ival) {
		fprintf(stderr, "wv_interp_value: assertion failed: lx <= ival for %s: ival=%g li=%d lx=%g\n", dv->wv_name, ival, li, lx);
	}

	ly = wds_get_point(&dv->wds[0], li);
	ry = wds_get_point(&dv->wds[0], ri);

	if(ival > rx) { /* no extrapolation allowed! */
		return ry;
	}

	return ly + (ry - ly) * ((ival - lx)/(rx - lx));
}

/*
 * Find a named variable, return pointer to WaveVar
 */
WaveVar *
wf_find_variable(WaveFile *wf, char *varname)
{
	int i;
	for(i = 0; i < wf->wf_ndv; i++) {
		if(0==strcmp(wf->dv[i].wv_name, varname))
			return &wf->dv[i];
	}
	return NULL;
}
