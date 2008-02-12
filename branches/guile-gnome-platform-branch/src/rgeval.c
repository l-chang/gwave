/*
 * evaluate guile string from a remote-execute system, and 
 * return seperate result, stdout, stderr strings.
 * 
 * Based on code from events.c from the scwm window manager.
 *
 * Copyright (C) 1998, 1999, 2000  Greg J. Badros and Maciej Stachowiak 
 */
#include <stdio.h>
#include <string.h>
#include <guile/gh.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <scwm_guile.h>
#include <guile-compat.h>

char *
remote_guile_eval(char *req, char **outp, char **errp)
{
#define FUNC_NAME "remote_guile_eval"

        SCM val, str_val;
	char *ret, *output, *error;
        SCM o_port, e_port;
        
        /* Temporarily redirect output and error to string ports. 
           Note that the port setting functions return the current previous
           port. */
        o_port = scm_set_current_output_port(make_output_strport(FUNC_NAME));
        e_port = scm_set_current_error_port(make_output_strport(FUNC_NAME));
        
        /* Evaluate the request expression and free it. */
        val = scwm_safe_eval_str((char *) req);

        str_val = scm_strprint_obj(val);
        ret = scm_to_locale_string(str_val);
        
        /* restore output and error ports; use returned o_port/e_port
           below for getting the strings back */
        o_port = scm_set_current_output_port(o_port);
        e_port = scm_set_current_error_port(e_port);
        
        /* Retrieve output and errors */
	if(outp) {
		output = scm_to_locale_string(scm_strport_to_string(o_port));
		*outp = output;
	}
	if(errp) {
		error = scm_to_locale_string(scm_strport_to_string(e_port));
		*errp = error;
	}

	return ret;
}
#undef FUNC_NAME
