/* 
 * spicefile.h - definitions for a file reader for the analog
 * output files of various spice-like simulators.
 *
 * Copyright 1998,1999 Stephen G. Tell.
 *
 *
 */

#ifdef __cplusplus
extern "C" {
#endif 

typedef struct _SpiceStream SpiceStream;
typedef struct _SpiceVar SpiceVar;


typedef enum {
	UNKNOWN = 0,
	TIME = 1,
	VOLTAGE = 2,
	CURRENT = 3,
	FREQUENCY = 4,
} VarType;

typedef enum SSMsgLevel_tag {DBG = -1, INFO = 0, WARN = 1, ERR = 2} SSMsgLevel;
extern FILE *ss_error_file;
typedef void (*SSMsgHook) (char *s);
extern SSMsgHook ss_error_hook;
extern SSMsgLevel spicestream_msg_level;

/* header data on each variable mentioned in the file
 * For sweep parameters, ncols will be 0.
 */
struct _SpiceVar {
	char *name;
	VarType type;
	int col;    /* index of (first) column of data that goes with this variable */
	int ncols;  /* number of columns of data for this variable; complex numbers have two */
};	

typedef int (*SSReadRow) (SpiceStream *sf, double *ivar, double *dvars);
typedef int (*SSReadSweep) (SpiceStream *sf, double *spar);

struct _SpiceStream {
	char *filename;
	int filetype;
	int ndv;	/* number of dependent variables */
	int ncols;	/* number of columns of data readrow will fill in */
	SpiceVar *ivar; /* ptr to independent-variable info */
	SpiceVar *dvar; /* ptr to array of dependent variable info */
	SpiceVar *spar; /* ptr to array of sweep parameter info */

	SSReadRow readrow;  /* func to read one row of data points */
	SSReadSweep readsweep;  /* func to read one row of data points */
	int ntables;	/* number of data tables in the file */
	int nsweepparam; /* number of sweep parameter values at the start of each table */

	/* the following stuff is for private use of reader routines */
	FILE *fp;
	int lineno;
	char *linebuf;
	int lbufsize;
	int expected_vals;
	int read_vals;
	int read_tables;
	int read_sweepparam;
	char *linep;
};

#define ss_readrow(sf, ivp, dvp) ((sf->readrow)(sf, ivp, dvp))
#define ss_readsweep(sf, swp) ((sf->readsweep)(sf, swp))

extern SpiceStream *ss_open(char *filename, char *type);
extern SpiceStream *ss_open_fp(FILE *fp, char *type);
extern SpiceStream *ss_open_internal(FILE *fp, char *name, char *type);
extern SpiceStream *ss_new(FILE *fp, char *name, int ndv, int nspar);
extern void ss_close(SpiceStream *sf);
extern char *ss_var_name(SpiceVar *sv, int col, char *buf, int n);
extern char *vartype_name_str(VarType type);
extern int fread_line(FILE *fp, char **bufp, int *bufsize);
extern void ss_msg(SSMsgLevel type, const char *id, const char *msg, ...);
extern char *ss_filetype_name(int n);


#ifdef __cplusplus
	   }
#endif 
