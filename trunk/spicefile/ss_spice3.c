/*
 * ss_spice3.c: routines for SpiceStream that handle the file formats
 * 	known as Berkeley Spice3 Rawfile
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
 * You should have received a copy of the GNU Library General Public
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

static int sf_readrow_s3raw(SpiceStream *sf, double *ivar, double *dvars);
char *msgid = "s3raw";

/* convert variable type string from spice3 raw file to 
 * our type numbers
 */
static VarType
sf_str2type_s3raw(char *s)
{
	if(strcasecmp(s, "voltage") == 0)
		return VOLTAGE;
	else if(strcasecmp(s, "current") == 0)
		return CURRENT;
	else if(strcasecmp(s, "frequency") == 0)
		return FREQUENCY;
	else if(strcasecmp(s, "time") == 0)
		return TIME;
	else return UNKNOWN;
}


/* Read spice-type file header - Berkeley Spice3 "raw" format */
SpiceStream *
sf_rdhdr_s3raw(char *name, FILE *fp)
{
	SpiceStream *sf = NULL;
	char *line = NULL;
	char *signam;
	int lineno = 0;
	int linesize = 1024;
	int dvsize = 128;
	char *key, *val;
	int nvars, npoints;
	int got_nvars = 0;
	int got_values = 0;
	int dtype_complex = 0;
	int binary = 0;
	int i;
	
	while(fread_line(fp, &line, &linesize) != EOF) {
		lineno++;
		key = strtok(line, ":");
		if(!key) {
			ss_msg(ERR, msgid, "%s:%d: syntax error, expected \"keyword:\"", name, lineno);
			g_free(line);
			return NULL;
		}
		if(strcmp(key, "Flags") == 0) {
			while(val = strtok(NULL, " ,\t\n")) {
				if(strcmp(val, "real") == 0) {
					dtype_complex = 0;
				}
				if(strcmp(val, "complex") == 0) {
					dtype_complex = 1;
				}
			}
		} else if(strcmp(key, "No. Variables") == 0) {
			val = strtok(NULL, " \t\n");
			if(!val) {
				ss_msg(ERR, msgid, "%s:%d: syntax error, expected integer", name, lineno);
				g_free(line);
				return NULL;
			}
			nvars = atoi(val);
			got_nvars = 1;
		} else if(strcmp(key, "No. Points") == 0) {
			val = strtok(NULL, " \t\n");
			if(!val) {
				ss_msg(ERR, msgid, "%s:%d: syntax error, expected integer", name, lineno);
				g_free(line);
				return NULL;
			}
			npoints = atoi(val);
		} else if(strcmp(key, "Variables") == 0) {
			if(!got_nvars) {
				ss_msg(ERR, msgid, "%s:%d: \"Variables:\" before \"No. Variables:\"", name, lineno, i);
				goto err;
				
			}
			sf = ss_new(fp, name, nvars-1, 0);
			sf->ncols = 1;
			sf->ntables = 1;
			for(i = 0; i < nvars; i++) {
				char *vnum, *vname, *vtypestr;
				if(fread_line(fp, &line, &linesize) == EOF) {
					ss_msg(ERR, msgid, "%s:%d: Unexpected EOF in \"Variables:\" at var %d", name, lineno, i);
					goto err;
				}
				lineno++;
				vnum = strtok(line, " \t\n");
				vname = strtok(NULL, " \t\n");
				vtypestr = strtok(NULL, " \t\n");
				if(!vnum || !vname || !vtypestr) {
					ss_msg(ERR, msgid, "%s:%d: expected number name type", name, lineno);
					goto err;
				}
				if(i == 0) { /* assume Ind.Var. first */
					sf->ivar->name = g_strdup(vname);
					sf->ivar->type = sf_str2type_s3raw(vtypestr);
					sf->ivar->col = 0;
					/* ivar can't really be two-column,
					   this is a flag that says to
					   discard 2nd point */
					if(dtype_complex)
						sf->ivar->ncols = 2;
					else
						sf->ivar->ncols = 1;
						
				} else {
					sf->dvar[i-1].name = g_strdup(vname);
					sf->dvar[i-1].type = sf_str2type_s3raw(vtypestr);
					sf->dvar[i-1].col = sf->ncols;
					if(dtype_complex)
						sf->dvar[i-1].ncols = 2;
					else
						sf->dvar[i-1].ncols = 1;

					sf->ncols += sf->dvar[i-1].ncols;
				}
			}
		} else if(strcmp(key, "Values") == 0) {
			got_values = 1;
			break;
		} else if(strcmp(key, "Binary") == 0) {
			binary = 1;
			got_values = 1;
			break;
		}
		if(got_values)
			break;
	}
	if(!sf) {
		ss_msg(ERR, msgid, "%s:%d: no \"Variables:\" section in header", name, lineno);
		goto err;
	}
	if(!got_values) {
		ss_msg(ERR, msgid, "%s:%d: EOF without \"Values:\" in header", name, lineno);
		goto err;
	}
	if(binary) {
		ss_msg(ERR, msgid, "%s: Binary Spice3 file not handled yet; set SPICE_ASCIIRAWFILE=1\n", name);
		goto err;
	}
	sf->lineno = lineno;
	sf->linebuf = line;
	sf->lbufsize = linesize;
	sf->readrow = sf_readrow_s3raw;
	ss_msg(DBG, msgid, "Done with header at offset 0x%lx\n", ftell(sf->fp));
	
	return sf;
err:
	if(line)
		g_free(line);
	if(sf) {
		ss_delete(sf);
	}
	return NULL;
}

/*
 * Read row of values from an ascii spice3 raw file
 */
static int
sf_readrow_s3raw(SpiceStream *sf, double *ivar, double *dvars)
{
	int i;
	int frownum;
	char *tok;

	if(fread_line(sf->fp, &sf->linebuf, &sf->lbufsize) == EOF) {
		return 0;  /* normal EOF */
	}
	sf->lineno++;
	/* first line of a set contains row number, independent variable */
	tok = strtok(sf->linebuf, " \t\n,");
	if(!tok) {
		ss_msg(ERR, msgid, "%s:%d: expected row number", 
		       sf->filename, sf->lineno);
		return -1;
	}
	if(!isdigit(*tok)) {
		ss_msg(WARN, msgid, "%s:%d: expected row number, got \"%s\". Note: only one dataset per file is supported, extra garbage ignored", 
		       sf->filename, sf->lineno, tok);
		return 0;
	}
	frownum = atoi(tok);
	/* todo: check for expected and maximum row number */

	tok = strtok(NULL, " \t\n,");
	if(!tok) {
		ss_msg(WARN, msgid, "%s:%d: expected ivar value", 
		       sf->filename, sf->lineno);
		return -1;
	}
	*ivar = atof(tok);
	
	for(i = 0; i < sf->ndv; i++) {
		SpiceVar *dv;
		dv = &sf->dvar[i];

		if(fread_line(sf->fp, &sf->linebuf, &sf->lbufsize) == EOF) {
			ss_msg(ERR, msgid, "%s:%d unexpected EOF at dvar %d",
			       sf->filename, sf->lineno, i);
			return -1;
		}
		sf->lineno++;

		tok = strtok(sf->linebuf, " \t\n,");
		if(!tok) {
			ss_msg(ERR, msgid, "%s:%d: expected value", 
			       sf->filename, sf->lineno);
			return -1;
		}
		dvars[dv->col-1] = atof(tok);

		if(dv->ncols > 1) {
			tok = strtok(NULL, " \t\n,");
			if(!tok) {
				ss_msg(ERR, msgid, "%s:%d: expected second value", 
				       sf->filename, sf->lineno);
				return -1;
			}
			dvars[dv->col] = atof(tok);
		}
	}
	return 1;
}
