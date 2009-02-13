/* Key routines extracted from:
 * 	scwmrepl.c,v 1.19 2000/01/22 21:13:16 gjb Exp $
 * Copyright (C) 1997-2000, Maciej Stachowiak and Greg J. Badros
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <xgexec.h>

#ifdef HAVE_READLINE
#include <readline/readline.h>
#ifdef HAVE_HISTORY
#include <readline/history.h>
#endif /* HAVE_HISTORY */
#endif /* HAVE_READLINE */

#ifdef HAVE_READLINE
char *scwm_complete(const char *text, int state)
{
	static char *result=NULL;
	static char *last=NULL;
	static char *completions[1024];

	extern Display *display;
	extern Window w;

	if (!state) {
		char *szSymbol;
		char *query, *output, *error;
		unsigned n;

		if (!last || strcmp(last,text)!=0) {
			if (last) {
				g_free(last);
			}
			last=strdup(text);
			if (result) {
				g_free(result);
				output = NULL;
			}
			query = malloc(strlen(text)+30);
			sprintf(query,"(apropos-internal \"^%s\")",text);
			result = xgexec_exec_full(display, w, query, &output, &error);
			if (error) {
				if (strlen(error) > 0)
					fprintf(stderr,"Got error querying apropos-internal for completion: %s",error);
				g_free(error);
				error = NULL;
			}
			if (output) {
				g_free(output);
				output = NULL;
			}
			free(query);
			query = NULL;
		}

		/* result is something like "(documentation documentation-debug doc-files)" */
		szSymbol = strtok(result+1," \t)");
		n=0;
		while (n<1023 && szSymbol) {
			completions[n++] = strdup(szSymbol);
			szSymbol = strtok(NULL," \t)");
		}
		completions[n]=NULL;
	}
	return completions[state];
}

void init_readline()
{
	rl_completion_entry_function = scwm_complete;
}
#endif

int appending_fgets(char **sofar)
{
#ifdef HAVE_READLINE
	char *buffer;
	unsigned pos,len;

	buffer=readline("gwave> ");
	if (buffer==NULL) {
		return 0;
	}
	len=strlen(buffer);
#if HAVE_HISTORY
	if (len>0) {
		add_history(buffer);
	}
#endif
	pos=strlen(*sofar);
	*sofar=g_realloc(*sofar,pos+len+2);
	strncpy(*sofar+pos,buffer,len);
	(*sofar)[pos+len]='\n';
	(*sofar)[pos+len+1]=0;
#else
	char buffer [512];

	fputs("gwave> ", stdout);
	do {
		fgets(buffer, 512, stdin);
		if (strlen(buffer)==0) {
			return 0;
		}
		*sofar=g_realloc(*sofar,strlen(*sofar)+strlen(buffer)+1);
		strcat(*sofar,buffer);
	} while (buffer[strlen(buffer)-1]!='\n');
#endif

	return 1;
}

int check_balance(char *expr) {
	/* If you think _this_ is hairy, try doing it for C statements. */
	int i;
	int end;
	int non_whitespace_p=0;
	int paren_count=0;
	int prev_separator=1;
	int quote_wait=0;

	end=strlen(expr);
	i=0;
	while (i<end) {
		switch(expr[i]) {
		case ';' :
			/* skip till newline. */
			do {
				i++;
			} while (expr[i]!='\n' && i<end);
			break;
		case ' ':
		case '\n':
		case '\t':
		case '\r':
			if (non_whitespace_p && paren_count==0 && !quote_wait) {
				return i;
			} else {
				prev_separator=1;
				i++;
			}
			break;
		case '\"' :
			if (non_whitespace_p && paren_count==0 &&
			    !quote_wait) {
				return i;
			} else {
				/* skip past ", ignoring \" */
				do {
					i++;
					if (i < end && expr[i]=='\\') {
						i++;
					}
				} while (i < end && expr[i]!='\"');
				i++;
				if (paren_count==0) {
					if (i < end) {
						return i;
					} else {
						return 0;
					}
				} else {
					prev_separator=1;
					non_whitespace_p=1;
					quote_wait=0;
				}
			}
			break;
		case '#' :
			if (non_whitespace_p && paren_count==0 &&
			    !quote_wait) {
				return i;
			} else {
				if (prev_separator && i+1<end && expr[i+1]=='{') {
					/* skip past }#, ignoring \} */
					do {
						i++;
						if (i < end && expr[i]=='\\') {
							i++;
						}
					} while (i < end && !(expr[i]=='}' && i+1<end
							      && expr[i+1]=='#'));
					i+=2;
					if (paren_count==0) {
						if (i < end) {
							return i;
						} else {
							return 0;
						}
					} else {
						prev_separator=1;
						non_whitespace_p=1;
						quote_wait=0;
					}
					/* MS:FIXME:: Handle #\) properly! */
				} else {
					prev_separator=0;
					quote_wait=0;
					non_whitespace_p=1;
					i++;
				}
			}
			break;
		case '(' :
			if (non_whitespace_p && paren_count==0 &&!quote_wait) {
				return i;
			} else {
				i++;
				paren_count++;
				non_whitespace_p=1;
				prev_separator=1;
				quote_wait=0;
			}
			break;
		case ')' :
			paren_count--;
			if (non_whitespace_p && paren_count==0) {
				return i+1;
			} else {
				i++;
				non_whitespace_p=1;
				prev_separator=1;
				quote_wait=0;
			}
			break;
		case '\'' :
			if (prev_separator) {
				non_whitespace_p=1;
				quote_wait=1;
				prev_separator=1;
				i++;
			} else {
				non_whitespace_p=1;
				prev_separator=0;
				i++;
			}
			break;
		default :
			prev_separator=0;
			quote_wait=0;
			non_whitespace_p=1;
			i++;
			break;
		}
	}
	return 0;
}

char *split_at(char **to_split, int i) {
	char *out;
	char *ret;

	out=strdup((*to_split)+i);

	(*to_split)[i]=0;
	ret=strdup(*to_split);
	free(*to_split);
	*to_split=out;
	return ret;
}
