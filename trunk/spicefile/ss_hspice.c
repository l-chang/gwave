/*
 * ss_hspice.c: HSPICE routines for SpiceStream
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
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <float.h>

#include <config.h>
#ifdef HAVE_GTK
#include <glib.h>
#else
#include "glib_fake.h"
#endif
#include "spicestream.h"

extern SpiceStream *sf_rdhdr_hspice(char *name, FILE *fp);
extern SpiceStream *sf_rdhdr_hsascii(char *name, FILE *fp);
extern SpiceStream *sf_rdhdr_hsbin(char *name, FILE *fp);

static int sf_readrow_hsascii(SpiceStream *sf, double *ivar, double *dvars);
static int sf_readrow_hsbin(SpiceStream *sf, double *ivar, double *dvars);
static SpiceStream *hs_process_header(int nauto, int nprobe, 
				      int nsweepparam, char *line, char *name);
static int sf_readsweep_hsascii(SpiceStream *sf, double *svar);
static int sf_readsweep_hsbin(SpiceStream *sf, double *svar);

static int sf_endblock_hsbin(SpiceStream *sf);


/* Read spice-type file header - autosense hspice binary or ascii */
SpiceStream *
sf_rdhdr_hspice(char *name, FILE *fp)
{
	int c;
	if((c = getc(fp)) == EOF)
		return NULL;
	ungetc(c, fp);

	if((c & 0xff) < ' ')
		return sf_rdhdr_hsbin(name, fp);
	else
		return sf_rdhdr_hsascii(name, fp);
      
	return NULL;
}

/* Read spice-type file header - hspice ascii */
SpiceStream *
sf_rdhdr_hsascii(char *name, FILE *fp)
{
	SpiceStream *sf = NULL;
	char *line = NULL;
	int nauto, nprobe, nsweepparam, ntables;
	int lineno = 0;
	int linesize = 1024;
	int lineused;
	char lbuf[256];
	char nbuf[16];
	char *cp;
	int maxlines = 500;	/* should calculate based on # of columns */

	if(fgets(lbuf, sizeof(lbuf), fp) == NULL)
		return NULL;
	lineno++;
	
	/* version of post format */
	if(strncmp(&lbuf[16], "9007", 4) != 0 
	   && strncmp(&lbuf[16], "9601", 4) != 0)
		return NULL;
	strncpy(nbuf, &lbuf[0], 4);
	nbuf[4] = 0;
	nauto = atoi(nbuf);

	strncpy(nbuf, &lbuf[4], 4);
	nbuf[4] = 0;
	nprobe = atoi(nbuf);
	
	strncpy(nbuf, &lbuf[8], 4);
	nbuf[4] = 0;
	nsweepparam = atoi(nbuf);
	
	if(fgets(lbuf, sizeof(lbuf), fp) == NULL) /* date, time etc. */
		return NULL;
	lineno++;
	if(fgets(lbuf, sizeof(lbuf), fp) == NULL) /*always just a single '0'?*/
		return NULL;
	ntables = atoi(lbuf);
	if(ntables == 0)
		ntables = 1;
	lineno++;
	
	/* lines making up a fixed-field structure with variable-types and
	 * variable names.
	 * variable names can get split across lines! so we remove newlines,
	 * paste all of the lines together, and then deal with the
	 * whole header at once.
	 * A variable name of "$&%#" indicates the end!
	 */
	line = g_new0(char, linesize);
	lineused = 0;
	do {
		int len;
		if(fgets(lbuf, sizeof(lbuf), fp) == NULL)
			return NULL;
		lineno++;
		if((cp = strchr(lbuf, '\n')) != NULL)
			*cp = 0;
		len = strlen(lbuf);
		if(lineused + len + 1 > linesize) {
			linesize *= 2;
			if(linesize > 200000) {
				ss_msg(ERR, "rdhdr_ascii", "internal error - failed to find end of header\n; linesize=%d line=\n%s\n", linesize, line);
				exit(4);
			}
				
			line = g_realloc(line, linesize);
		}
		strcat(line, lbuf);
		lineused += len;

	} while(!strstr(line, "$&%#") && lineno < maxlines);
	if(lineno == maxlines) {
		ss_msg(DBG, "rdhdr_hsascii", "%s:%d: end of hspice header not found", name,lineno);
		goto fail;
	}

	sf = hs_process_header(nauto, nprobe, nsweepparam, line, name);
	if(!sf)
		goto fail;
	sf->fp = fp;
	sf->readrow = sf_readrow_hsascii;
	sf->linebuf = line;
	sf->linep = NULL;
	sf->lbufsize = linesize;
	sf->ntables = ntables;
	sf->read_tables = 0;
	sf->read_sweepparam = 0;
	sf->readsweep = sf_readsweep_hsascii;

	ss_msg(DBG, "rdhdr_hsascii", "expect %d columns", sf->ncols);

	return sf;

 fail:
	if(line)
		g_free(line);
	return NULL;

}

/* Read spice-type file header - hspice binary */
SpiceStream *
sf_rdhdr_hsbin(char *name, FILE *fp)
{

	SpiceStream *sf = NULL;
	char *ahdr = NULL;
	int ahdrsize;
	int datasize;
	int nauto, nprobe, nsweepparam, ntables;
	char nbuf[16];
	
	gint32 ibuf[4];

	if(fread(ibuf, sizeof(gint32), 4, fp) != 4) {
		ss_msg(DBG, "rdhdr_hsbin", "EOF reading block1 header");
		return NULL;
	}
	if(ibuf[0] != 4 || ibuf[2] != 4) {
		ss_msg(DBG, "rdhdr_hsbin", "unexepected values in block1 header");
		return NULL;
	}
	ahdrsize = ibuf[3];
	ahdr = g_new(char, ahdrsize+1);
	if(fread(ahdr, sizeof(char), ahdrsize, fp) != ahdrsize) {
		ss_msg(DBG, "rdhdr_hsbin", "EOF reading block1");
		goto fail;
	}
	ahdr[ahdrsize] = '\0';
	if(fread(ibuf, sizeof(gint32), 1, fp) != 1) {
		ss_msg(DBG, "rdhdr_hsbin", "EOF reading block1 trailer");
		goto fail;
	}
	if(ibuf[0] != ahdrsize) {
		ss_msg(DBG, "rdhdr_hsbin", "block1 trailer mismatch");
		goto fail;
	}
	/* ahdr is an ascii header that describes the variables in
	 * much the same way that the first lines of the ascii format do,
	 * except that there are no newlines
	 */

	if(strncmp(&ahdr[16], "9007", 4) != 0 	/* version of post format */
	   && strncmp(&ahdr[16], "9601", 4) != 0)
		goto fail;
	strncpy(nbuf, &ahdr[0], 4);
	nbuf[4] = 0;
	nauto = atoi(nbuf);	/* number of automaticly-included variables,
				   first one is independent variable */
	strncpy(nbuf, &ahdr[4], 4);
	nbuf[4] = 0;
	nprobe = atoi(nbuf);	/* number of user-requested columns */

	strncpy(nbuf, &ahdr[8], 4);
	nbuf[4] = 0;
	nsweepparam = atoi(nbuf);	/* number of sweep parameters */

	ntables = atoi(&ahdr[176]);
	if(ntables == 0)
		ntables = 1;

	sf = hs_process_header(nauto, nprobe, nsweepparam, &ahdr[256], name);
	if(!sf)
		goto fail;
	
	if(fread(ibuf, sizeof(gint32), 4, fp) != 4) {
		ss_msg(DBG, "rdhdr_hsbin", "EOF reading block2 header");
		goto fail;
	}
	if(ibuf[0] != 4 || ibuf[2] != 4) {
		ss_msg(DBG, "rdhdr_hsbin", "unexepected values in block2 header");
		goto fail;
	}

	datasize = ibuf[3];
	sf->expected_vals = datasize / sizeof(float);
	sf->read_vals = 0;
	
	ss_msg(DBG, "rdhdr_hsbin", "datasize=%d expect %d columns, %d values; reading block2 at 0x%lx", datasize, sf->ncols, sf->expected_vals, ftell(fp));


	sf->fp = fp;
	sf->readrow = sf_readrow_hsbin;
	sf->readsweep = sf_readsweep_hsbin;

	sf->ntables = ntables;
	sf->read_tables = 0;
	sf->read_sweepparam = 0;

	return sf;
 fail:
	if(ahdr)
		g_free(ahdr);
	if(sf->dvar)
		g_free(sf->dvar);
	if(sf)
		g_free(sf);

	return NULL;
}

/* common code for reading ascii or binary hspice headers.
 * Given a string of ascii header information, set up the
 * SpiceStream structure appropriately.
 * Returns NULL on failure.
 */
static SpiceStream *
hs_process_header(int nauto, int nprobe, int nsweepparam, char *line, char *name)
{
	char *cp;
	char *signam;
	SpiceStream *sf;
	int i;
	int ncols;
	int hstype;

/* type of independent variable */
	cp = strtok(line, " \t\n");
	if(!cp) {
		ss_msg(DBG, "hs_process_header", "%s: initial vartype not found on header line.", name);
		return NULL;
	}
	sf = ss_new(NULL, name, nauto-1 + nprobe, nsweepparam);
	hstype = atoi(cp);
	switch(hstype) {
	case 1:
		sf->ivar->type = TIME;
		break;
	case 2:
		sf->ivar->type = FREQUENCY;
		break;
	case 3:
		sf->ivar->type = VOLTAGE;
		break;
	default:
		sf->ivar->type = UNKNOWN;
		break;
	}
	sf->ivar->col = 0;
	sf->ivar->ncols = 1;
	sf->ncols = 1;

/* dependent variable types */
	for(i = 0; i < sf->ndv; i++) {
		cp = strtok(NULL, " \t\n");
		if(!cp) {
			ss_msg(DBG, "hs_process_header", "%s: not enough vartypes on header line", name);
			return NULL;
		}
		if(!isdigit(cp[0])) {
			ss_msg(DBG, "hs_process_header", "%s: bad vartype %d [%s] on header line", name, i, cp);
			return NULL;
		}
		hstype = atoi(cp);
		switch(hstype) {
		case 1:
		case 2:
			sf->dvar[i].type = VOLTAGE;
			break;
		case 8:
		case 15:
		case 22:
			sf->dvar[i].type = CURRENT;
			break;
		default:
			sf->dvar[i].type = UNKNOWN;
			break;
		}

		/* how many columns comprise this variable? */
		sf->dvar[i].col = sf->ncols;
		if(i < nauto-1 && sf->ivar->type == FREQUENCY) {
			sf->dvar[i].ncols = 2;
		} else {
			sf->dvar[i].ncols = 1;
		}
		sf->ncols += sf->dvar[i].ncols;
	}

/* independent variable name */
	signam = strtok(NULL, " \t\n"); 
	if(!signam) {
		ss_msg(DBG, "hs_process_header", "%s: no IV name found on header line", name);
		goto fail;
	}
	sf->ivar->name = g_strdup(signam);
	
 /* dependent variable names */
	for(i = 0; i < sf->ndv; i++) {
		if((signam = strtok(NULL, " \t\n")) == NULL) {
			ss_msg(DBG, "hs_process_header", "%s: not enough DV names found on header line", name);
			goto fail;
		}
		sf->dvar[i].name = g_strdup(signam);
	}
/* sweep parameter names */
	for(i = 0; i < sf->nsweepparam; i++) {
		if((signam = strtok(NULL, " \t\n")) == NULL) {
			ss_msg(DBG, "hs_process_header", "%s: not enough sweep parameter names found on header line", name);
			goto fail;
		}
		sf->spar[i].name = g_strdup(signam);
	}

	return sf;

 fail:
	ss_delete(sf);
	return NULL;
}


/*
 * helper routine: get next value from ascii hspice file.
 * the file is line-oriented, with fixed-width fields on each line.
 * Lines may look like either of these two examples:
0.66687E-090.21426E+010.00000E+000.00000E+000.25000E+010.71063E-090.17877E+01
 .00000E+00 .30000E+01 .30000E+01 .30000E+01 .30000E+01 .30000E+01 .30092E-05

 */
static int
sf_getval_hsascii(SpiceStream *sf, double *val)
{
	char vbuf[16];
	char *vp;

	if(!sf->linep || (*sf->linep==0) || *sf->linep == '\n') {
		if(fgets(sf->linebuf, sf->lbufsize, sf->fp) == NULL)
			return 0;
		sf->linep = sf->linebuf;
		/* fprintf(stderr, "#line: %s\n", sf->linebuf); */
	}
	strncpy(vbuf, sf->linep, 11);
	sf->linep += 11;
	vbuf[11] = 0;
	vp = vbuf;
	while(isspace(*vp)) /* atof doesn't like spaces */
		vp++;
	*val = atof(vp);
	/* fprintf(stderr, "#vp=\"%s\" val=%f\n", vp, val); */
	return 1;
}

/* Read row of values from ascii hspice-format file.
 * Returns:
 *	1 on success.  also fills in *ivar scalar and *dvars vector
 *	0 on EOF
 *	-1 on error  (may change some ivar/dvar values)
 *	-2 on end of table, with more tables supposedly still to be read.
 */

static int 
sf_readrow_hsascii(SpiceStream *sf, double *ivar, double *dvars)
{
	int i;

	if(!sf->read_sweepparam) { /* first row of table */
		if(sf_readsweep_hsascii(sf, NULL) <= 0) /* discard sweep parameters, if any */
			return -1;  
	}
	if(sf_getval_hsascii(sf, ivar) == 0)
		return 0;
	if(*ivar >= 1.0e29) { /* "infinity" at end of data table */
		sf->read_tables++;
		if(sf->read_tables == sf->ntables)
			return 0; /* EOF */
		else
			sf->read_sweepparam = 0;
			return -2;  /* end of table, more tables follow */
	}

	for(i = 0; i < sf->ncols-1; i++) {
		if(sf_getval_hsascii(sf, &dvars[i]) == 0) {
			ss_msg(ERR, "sf_readrow_hsascii", "%s: data field %d missing", sf->filename, i);
			return -1;
		}
	}
	return 1;
}

/* Read row of values from binary hspice-format file.
 * Returns:
 *	1 on success.  also fills in *ivar scalar and *dvars vector
 *	0 on EOF
 *	-1 on error  (may change some ivar/dvar values)
 */
static int 
sf_readrow_hsbin(SpiceStream *sf, double *ivar, double *dvars)
{
	int i;
	float val;
	long pos;

	if(!sf->read_sweepparam) { /* first row of table */
		if(sf_readsweep_hsascii(sf, NULL) <= 0) /* discard sweep parameters, if any */
			return -1;  
	}
	if(fread(&val, sizeof(float), 1, sf->fp) != 1) {
		pos = ftell(sf->fp);
		ss_msg(ERR, "sf_readrow_hsbin", "unexepected EOF in data at offset 0x%lx", pos);
		return 0;
	}
	sf->read_vals++;
	if(sf->read_vals > sf->expected_vals) {
		pos = ftell(sf->fp);
		ss_msg(DBG, "sf_readrow_hsbin", "exiting after %d values at offset 0x%lx",
		       sf->read_vals, pos);
		return 0;
	}
	if(val >= (1e30 - DBL_EPSILON)) {
		pos = ftell(sf->fp);
		ss_msg(DBG, "sf_readrow_hsbin", "end of table at infinite ivar value at offset 0x%lx", pos);

		return sf_endblock_hsbin(sf);
	}
	*ivar = val;

	for(i = 0; i < sf->ncols-1; i++) {
		if(sf->read_vals >= sf->expected_vals) {
			pos = ftell(sf->fp);
			ss_msg(ERR, "sf_readrow_hsbin", "mid-row exit after %d values at field %d, offset 0x%lx",
			       sf->read_vals, i, pos);
			return 0;
		}
		
		if(fread(&val, sizeof(float), 1, sf->fp) != 1) {
			pos = ftell(sf->fp);
			ss_msg(ERR, "sf_readrow_hsbin", "unexpected EOF at field %d, offset 0x%lx", i, pos);
			return -1;
		}
		dvars[i] = val;
		sf->read_vals++;
	}
	return 1;
}

/*
 * End of block processing for hspice binary files.
 * the file is assumed to be positioned for reading the single-word block
 * trailer.
 *
 * Returns:
 *	-1 on error or unexpected EOF
 *	-2 on end of block with another block to follow
 *	0 on normal EOF
 */
static int
sf_endblock_hsbin(SpiceStream *sf)
{
	gint32 ibuf[4];
	int datasize;

	sf->read_tables++;
	if(fread(ibuf, sizeof(gint32), 1, sf->fp) != 1) {
		ss_msg(ERR, "sf_endblock_hsbin", "EOF reading block trailer");
		return -1;
	}
	if(fread(ibuf, sizeof(gint32), 4, sf->fp) != 4) {
		if(sf->read_tables == sf->ntables) {
			return 0;
		} else {
			ss_msg(ERR, "sf_endblock_hsbin", "EOF but more tables expected");
			return -1;
		}

	}
	if(ibuf[0] != 4 || ibuf[2] != 4) {
		ss_msg(ERR, "sf_endblock_hsbin", "unexepected values in block2 header");
		return -1;
	}

	datasize = ibuf[3];
	sf->expected_vals = datasize / sizeof(float);
	sf->read_vals = 0;
	sf->read_sweepparam = 0;
	return -2;  /* end of table, more tables follow */

	if(sf->read_tables == sf->ntables)
		return 0; /* EOF */
}

/*
 * Read the sweep parameters from an HSPICE ascii or binary file
 * This routine must be called before the first sf_readrow_hsascii call in each data
 * table.  If it has not been called before the first readrow call, it will be called
 * with a NULL svar pointer to read and discard the sweep data. 
 *
 * returns:
 *	1 on success
 * 	-1 on error
 */
static int
sf_readsweep_hsascii(SpiceStream *sf, double *svar)
{
	int i;
	double val;
	for(i = 0; i < sf->nsweepparam; i++) {
		if(sf_getval_hsascii(sf, &val) == 0) {
			ss_msg(ERR, "sf_readsweep_hsascii", "unexpected EOF reading sweep parameters\n");
			return -1;
		}
		if(svar)
			svar[i] = val;
	}
	
	sf->read_sweepparam = 1;
	return 1;
}

static int
sf_readsweep_hsbin(SpiceStream *sf, double *svar)
{
	int i;
	long pos;
	float val;
	for(i = 0; i < sf->nsweepparam; i++) {
		if(fread(&val, sizeof(float), 1, sf->fp) != 1) {
			pos = ftell(sf->fp);
			ss_msg(ERR, "sf_readsweep_hsbin", "unexepected EOF in data at offset 0x%lx", pos);
			return 0;
		}
		if(svar)
			svar[i] = val;
	}
	
	sf->read_sweepparam = 1;
	return 1;
}

