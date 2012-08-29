/* SCHEME->C - initialization and server interface */

/* Copyright (c) 1989-1993 Hewlett-Packard Development Company, L.P.
 *		All Rights Reserved

 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */


/* This module defines some basic global objects and initializes those parts
   of the SCHEME->C runtime system which are written in C.  For
   compatibility with other modules, the routines and Scheme globals provided
   by these routines appear as members of the module "sc".
*/

/* Definitions for objects within sc */

#include <string.h>		/* for strcmp(), strncmp() */
#include "objects.h"
#include "scinit.h"
#include "heap.h"
#include "apply.h"
#include "callcc.h"
#include "cio.h"
#include <gc/gc.h>
#if GC_WITHOUT_TABLES
#else
#include <gc/gc_tiny_fl.h>
#endif
#ifndef NULL
#define NULL 0
#endif

/* Definitions for objects elsewhere in the Scheme system */

extern  TSCP  scrt1_reverse( XAL1( TSCP ) );
#ifdef __GNUC__
extern  TSCP  scdebug_error( XAL3( TSCP, TSCP, TSCP ) ) __attribute__((noreturn));
#else
extern  TSCP  scdebug_error( XAL3( TSCP, TSCP, TSCP ) );
#endif
extern  TSCP  screp__init();
extern	TSCP  screp_scheme2c( XAL1( TSCP ) );

static void  init_procs();

/* Global data structure for this module. */

static S2CINT  empty[4];		/* Empty string and empty vector
					   are allocated on a 4-byte boundary
					   from here */

static S2CINT  expandfailed = 0;	/* Expansion failure flag */

static S2CINT  module_initialized = 0;

S2CINT  sc_timeslice = MAXS2CINT,	/* Counter for time slicing. */
	sc_timesliceinit = 10000,	/* Time slice value */
	sc_stackbytes = 5000;		/* # of bytes of stack */

char *sc_topofstack,			/* Top-of-stack limit. */
     *sc_savetopofstack;		/* Save it here on stack overflow */

/* Command line arguments and environment variables which control the heap are
   interpreted by the following functions.
*/

static  char *heapfilename = NULL;	/* Pointer to heap file name */

static S2CINT   scheap,			/* Heap size in megabytes */
	       scmaxheap,		/* Heap allowed to grow this big */
	       sclimit;			/* % at which to do total collection */

/* Get value from either the command line or the environment. */

static char*  getargval( S2CINT argc, char *argv[], char* cl, char* env )
{
	S2CINT  i;

	for  (i = 1; i < argc-1; i++)  {
	   if  (strcmp( argv[ i ], cl ) == 0)  return( argv[ i+1 ] );
	}
	return( sc_getenv( env ) );
}

/* Convert a string to a number, returning -1 on an error */

static S2CINT  getinteger( char* cp )
{
	S2CINT  value = 0;

	if  (*cp == 0)  value = -1;
	while  (*cp  &&  *cp >= '0'  &&  *cp <= '9')  {
	   value = value*10+*cp-'0';
	   cp = cp+1;
	}
	if  (*cp == 0)  return( value );  else  return( -1 );
}

/* Decode all Scheme->C specific arguments. */

static void  decodearguments( S2CINT argc, char *argv[] )
{
	char  *val;

	// TODO
#if 0
	val = getargval( argc, argv, "-sch", "SCHEAP" );
	if  (val != NULL)  {
	   scheap = getinteger( val );
	   if  (scheap < SCMINHEAP)  scheap = SCMINHEAP;
	   if  (scheap > SCMAXHEAP)  scheap = SCMAXHEAP;
	}
	else  scheap = SCHEAP;
	val = getargval( argc, argv, "-scmh", "SCMAXHEAP" );
	if  (val != NULL)  {
	   scmaxheap = getinteger( val );
	   if  (scmaxheap < scheap)  scmaxheap = scheap;
	   if  (scmaxheap > SCMAXHEAP)  scmaxheap = SCMAXHEAP;
	}
	else  scmaxheap = scheap*5;
	if  (scmaxheap > SCMAXHEAP)  scmaxheap = SCMAXHEAP;
	heapfilename = getargval( argc, argv, "-schf", "SCHEAPFILE" );
	val = getargval( argc, argv, "-scgc", "SCGCINFO" );
	if  (val != NULL)  {
	   sc_gcinfo = getinteger( val );
	   if  (sc_gcinfo < 0  ||  sc_gcinfo > 2)  sc_gcinfo = 0;
	}
	else  sc_gcinfo = 0;
	val = getargval( argc, argv, "-scl", "SCLIMIT" );
	if  (val != NULL)  {
	   sclimit = getinteger( val );
	   if  (sclimit < MINSCLIMIT)  sclimit = SCLIMIT;
	   if  (sclimit > MAXSCLIMIT)  sclimit = SCLIMIT;
	}
	else  sclimit = SCLIMIT;
#endif
}

/* The command line arguments passed to a program with a Scheme main are
   formed into a list of strings by the following function.  It is accessed
   as CLARGUMENTS within the compiler.  If an argument of the form: -scm <name>
   is provided, then a list of command line arguments will not be
   returned, and the function <name> will be invoked as the "main" program
   with the command line arguments.  All flags of the form:  -sc... <value>
   are reserved for use of the Scheme system and will be deleted from the
   command line.
*/

TSCP  sc_clarguments( int argc, char *argv[] )
{
	int  i;
	TSCP  argl, main;

	argl = EMPTYLIST;
	main = FALSEVALUE;
	i = 0;
	while  (i < argc)  {
	   if  (strcmp( argv[ i ], "-scm" ) == 0)  {
	      main = sc_string_2d_3esymbol( CSTRING_TSCP( argv[ ++i ] ) );
	   }
	   else  if  (strncmp( argv[ i ], "-sc", 3 ) == 0)  {
	      i++;
	   }
	   else  {
	      argl = sc_cons( CSTRING_TSCP( argv[ i ] ), argl );
	   }
	   i++;
	}
	argl = scrt1_reverse( argl );
	if  (main != FALSEVALUE)  {
	   sc_apply_2dtwo( *T_U( main )->symbol.ptrtovalue,
	   	     	   sc_cons( argl, EMPTYLIST ) );
	   SCHEMEEXIT();
	}
	return( argl );
}


/* The client program examines and sets the number of Scheme procedure calls
   per time slice by the following procedures.
*/

TSCP  sc_time_2dslice_v;

TSCP  sc_set_2dtime_2dslice_21_v;

TSCP  sc_time_2dslice()  {
	return( C_FIXED( sc_timesliceinit ) );
}

TSCP  sc_set_2dtime_2dslice_21( TSCP ticks )
{
	if  (TSCPTAG( ticks ) != FIXNUMTAG  ||  FIXED_C( ticks ) <= 0)
	   sc_error( "SET-TIME-SLICE!", "Argument is not a POSITIVE INTEGER",
		     EMPTYLIST );
	sc_timesliceinit = FIXED_C( ticks );
	sc_timeslice = sc_timesliceinit;
	return( ticks );
}

/* The client program examines and sets the size of the Scheme stack in bytes
   by the following procedures.
*/

TSCP  sc_stack_2dsize_v;

TSCP  sc_set_2dstack_2dsize_21_v;

TSCP  sc_stack_2dsize()
{
	return( C_FIXED( sc_stackbytes ) );
}

TSCP  sc_set_2dstack_2dsize_21( TSCP bytes )
{
	char  *ts;

	if  (TSCPTAG( bytes ) != FIXNUMTAG  ||
	     FIXED_C( bytes ) <= (STACKFUDGE*2))
	   sc_error( "SET-STACK-SIZE!",
	   	     "Argument is not a POSITIVE INTEGER >= ~s",
		     LIST1( C_FIXED( STACKFUDGE*2 ) ) );
	sc_stackbytes = FIXED_C( bytes );
#ifdef STACK_GROWS_POSITIVE
	ts = ((char*)sc_stackbase)+sc_stackbytes-STACKFUDGE;
#else
	ts = ((char*)sc_stackbase)-sc_stackbytes+STACKFUDGE;
#endif
	sc_topofstack = ts;
	return( bytes );
}

/* Side tables are allocated by calling the following procedure with the
   first and last heap pages, and pointers to the pagegeneration, type,
   lock and link tables.  An allocation failure will cause the pointers to
   be returned as NULL.
*/

static void  allocate_sidetables( S2CINT first,	/* heap pages */
				  S2CINT last,
						/* Ptrs to ptrs to tbls */
				  unsigned char **pagegen,
				  unsigned char **type,
	       			  unsigned char **lock,
				  PAGELINK  **link )
{
	S2CINT  bytes;
	char*  addr;

	typedef unsigned char uchar;

	if  ( (*pagegen = (uchar*)sc_gettable( (last-first+2)*sizeof( unsigned char ),
					       ~module_initialized )) != NULL  &&
	      (*type = (uchar*)sc_gettable( (last-first+2)*sizeof( unsigned char ),
					    ~module_initialized )) != NULL  &&
	      (*lock = (uchar* )sc_gettable( (last-first+2)*sizeof( unsigned char ),
					     ~module_initialized )) != NULL  &&
	      (*link = (PAGELINK*)sc_gettable( (last-first+2)
					       *sizeof( PAGELINK ),
					       ~module_initialized ))
	      != NULL )  {
	   return;
	}
	expandfailed = 1;
	sc_freetable( *pagegen );
	sc_freetable( *type );
	sc_freetable( *lock );
	sc_freetable( *link );
	*pagegen = *type = *lock = NULL;
	*link = NULL;
}

/* The following function is called to initialize the heap from scratch. */
#ifdef STDERR_ISNT_UNBUFFERED
#include <stdio.h>
#endif

/* Initialization from a compiled Scheme program. */

void  sc_restoreheap( S2CINT desiredheap, int argc, char *argv[],
		      void (*mainproc)() )
{
	if  (module_initialized)  return;
	if  (desiredheap  &&  desiredheap > scheap)  {
	   scheap = desiredheap;
	}
	decodearguments( argc, argv );
	sc_newheap();
}

/* This initialization function is provided to allow automatic initialization
   from a Modula-2 program.
*/

void  sc__init()
{
	if  (module_initialized)  return;
	decodearguments( 0, (char**)NULL );
	sc_newheap();
}

/* Routines coded in C call the following function to access the Scheme ERROR
   function.  SYMBOL is a string representing the function name.  FORMAT is a
   string which is a format descriptor.  ARGS is a list of TSCP arguments.
*/

sc_error( char *symbol, char *format, TSCP args )
{
	sc_timeslice = 1000000;
	sc_savetopofstack = sc_topofstack;
#ifdef STACK_GROWS_POSITIVE
	sc_topofstack = (char*)MAXS2CINT;
#else
	sc_topofstack = 0;
#endif
	scdebug_error( sc_string_2d_3esymbol( CSTRING_TSCP( symbol ) ),
		       CSTRING_TSCP( format ), args );
}


#if NO_GC
#else
#if GC_WITHOUT_TABLES
#else
extern void **normal_freelists;
extern void **ptrfree_freelists;
#endif
#endif

sc_newheap()
{
	S2CINT  i, j, page, pagecnt;
	TSCP  unknown;
	SCP  ep;
#ifndef NO_GC
	GC_INIT();
#if GC_WITHOUT_TABLES
#else
	normal_freelists = GC_malloc(GC_TINY_FREELISTS*sizeof(void*));
	void **freelist = normal_freelists;
	while(freelist < normal_freelists + GC_TINY_FREELISTS)
	  *(freelist++) = (void*)1;
	ptrfree_freelists = GC_malloc(GC_TINY_FREELISTS*sizeof(void*));
	freelist = ptrfree_freelists;
	while(freelist < ptrfree_freelists + GC_TINY_FREELISTS)
	  *(freelist++) = (void*)1;
#endif
#endif

#ifdef STDERR_ISNT_UNBUFFERED
      /* Older versions of SunOS (before 4.1.x?) may have a line-buffered
       * stderr. According to "man stdio" on SunOS 4.1.2 and 5.2, stderr
       * _should_ be unbuffered nowadays.
       * If stderr isn't unbuffered, then logging messages written
       * _before_ the heap has been initialized will cause some malloc-ing,
       * which in turn confuses the heap management.
       * This is the place to patch stderr if necessary.
       */
      setbuf(stderr, (char*)0);
#endif

	sc_emptylist = EMPTYLIST;
	ep = (SCP)((((S2CINT)((char*)&empty[0]))+(sizeof(S2CINT)-1)) &
		   ~(((S2CINT)sizeof(S2CINT))-1));
	ep->vector.length = 0;
	ep->vector.tag = VECTORTAG;
	sc_emptyvector = U_T( ep, EXTENDEDTAG );
	ep = (SCP)(((char*)ep)+sizeof(S2CINT));
	ep->string.length = 0;
	ep->string.tag = STRINGTAG;
	sc_emptystring = U_T( ep, EXTENDEDTAG );
	STRING_CHAR( sc_emptystring, 0 ) = 0;
	sc_falsevalue = FALSEVALUE;
	sc_truevalue = TRUEVALUE;
	sc_eofobject = EOFOBJECT;
	sc_undefined = UNDEFINED;
	sc_constants = NULL;
	sc_globals = NULL;
	// FIXME
#if 1
	sc_whenfreed = EMPTYLIST;
	sc_freed = EMPTYLIST;
	sc_globals = addtoSCPTRS( sc_globals, &sc_freed );
#endif
	sc_clink = EMPTYLIST;
	sc_globals = addtoSCPTRS( sc_globals, &sc_clink );
	sc_stacktrace = NULL;
	sc_obarray = sc_make_2dvector( C_FIXED( 1023 ), EMPTYLIST );
	sc_initializevar( "*OBARRAY*", &sc_obarray, sc_obarray );
	sc_setstdio();
	init_procs();
	unknown = sc_makeprocedure( 0, 0, sc_unknowncall, EMPTYLIST );
	TX_U( unknown )->procedure.required = 255;
	for  (i = 0;  i <= 3;  i++)  {
	   sc_unknownproc[ i ] = unknown;
	   sc_globals = addtoSCPTRS( sc_globals, &sc_unknownproc[ i ] );
	}
	sc_arm_mathtraps();
	sc_schememode = STANDALONESCHEME;
	sc_cioinit();
	module_initialized = -1;
}

/* The following function returns informations about the implementation.  The
   form of the function follows a recent proposal on rrrs-authors.  The result
   is a list of strings or #F's of the form:

	  (<name> <version> <MACHINE> <CPU> <OS> <FS> . <supports>)
*/

TSCP  sc_implementation_v;

TSCP  sc_implementation()
{
	return(
	   sc_cons(
	      CSTRING_TSCP( "Scheme->C" ),
	      sc_cons(
		 CSTRING_TSCP( "15mar93jfb" ),
		 sc_cons(
#ifdef IMPLEMENTATION_MACHINE
		    CSTRING_TSCP( IMPLEMENTATION_MACHINE ),
#else
		    FALSEVALUE,
#endif
		    sc_cons(
#ifdef IMPLEMENTATION_CPU
		       CSTRING_TSCP( IMPLEMENTATION_CPU ),
#else
		       FALSEVALUE,
#endif
		       sc_cons(
#ifdef IMPLEMENTATION_OS
			  CSTRING_TSCP( IMPLEMENTATION_OS ),
#else
			  FALSEVALUE,
#endif
			  sc_cons(
#ifdef IMPLEMENTATION_FS
			  CSTRING_TSCP( IMPLEMENTATION_FS ),
#else
			  FALSEVALUE,
#endif
			      EMPTYLIST
				 )
			      )
			   )
			)
		     )
		  )
	      );
}

/* The client program evaluates a Scheme expression by calling the procedure
 * scheme2c with the following arguments:
 *
 *	input_expression:	null terminated ASCII string containing
 *				a Scheme expression.
 *
 *	status:			evaluation status returned here.
 *
 *	output:			stdout-port contents returned here.
 *
 *	error:			stderr-port contents return here.
 *
 * See the Scheme implementation in screp.sc for details.
 */

void  scheme2c( char *input_expression, int *status,
		char **output, char **error )
{
	TSCP  x;
	S2CINT  *sp;

	if  (module_initialized == 0)  {
	   sc__init();
	   sc_schememode = EMBEDDEDSCHEME;
	   screp__init();
	}
	STACKPTR( sp );
	if  ((S2CUINT)sp > (S2CUINT)sc_stackbase)  {
	   /* Stack was cut back, move sc_stackbase */
	   sc_stackbase = sp;
	}
	sc_topofstack = ((char*)sc_stackbase)-sc_stackbytes+STACKFUDGE;
	sc_clink = EMPTYLIST;
	sc_stacktrace = NULL;
	sc_timeslice = sc_timesliceinit;
	x = screp_scheme2c( CSTRING_TSCP( input_expression ) );
	*status = FIXED_C( PAIR_CAR( x ) );
	*output = (char*)&STRING_CHAR( PAIR_CAR( PAIR_CDR( x ) ), 0 );
	*error = (char*)&STRING_CHAR( PAIR_CAR( PAIR_CDR( PAIR_CDR( x ) ) ),
				      0 );
}

/* The variables holding the values of the functions defined in this module
   are initialized by the following procedure.
*/

static void  init_procs()
{
	INITIALIZEVAR( "COLLECT",
		       ADR( sc_collect_v ),
		       MAKEPROCEDURE( 0,
				      0, sc_collect, EMPTYLIST ) );
	INITIALIZEVAR( "COLLECT-ALL",
		       ADR( sc_collect_2dall_v ),
		       MAKEPROCEDURE( 0,
				      0, sc_collect_2dall, EMPTYLIST ) );
	INITIALIZEVAR( "CONS",
		       ADR( sc_cons_v ),
		       MAKEPROCEDURE( 2, 0, sc_cons, EMPTYLIST ) );
	INITIALIZEVAR( "MAKE-STRING",
		       ADR( sc_make_2dstring_v ),
		       MAKEPROCEDURE( 1,
				      1,
				      sc_make_2dstring, EMPTYLIST ) );
	INITIALIZEVAR( "STRING-COPY",
		       ADR( sc_string_2dcopy_v ),
		       MAKEPROCEDURE( 1,
				      0,
				      sc_string_2dcopy, EMPTYLIST ) );
	INITIALIZEVAR( "MAKE-VECTOR",
		       ADR( sc_make_2dvector_v ),
		       MAKEPROCEDURE( 1,
				      1,
				      sc_make_2dvector, EMPTYLIST ) );
	INITIALIZEVAR( "MAKE-%RECORD",
		       ADR( sc_make_2d_25record_v ),
		       MAKEPROCEDURE( 1,
				      1,
				      sc_make_2d_25record, EMPTYLIST ) );
 	INITIALIZEVAR( "C-STRING->STRING",
		       ADR( sc_c_2dstring_2d_3estring_v ),
		       MAKEPROCEDURE( 1,
				      0,
				      sc_c_2dstring_2d_3estring, EMPTYLIST ) );
       INITIALIZEVAR( "STRING->SYMBOL",
		       ADR( sc_string_2d_3esymbol_v ),
		       MAKEPROCEDURE( 1,
				      0,
				      sc_string_2d_3esymbol, EMPTYLIST ) );
	INITIALIZEVAR( "STRING->UNINTERNED-SYMBOL",
		       ADR( sc_d_2dsymbol_ab4b4447_v ),
		       MAKEPROCEDURE( 1,
				      0,
				      sc_d_2dsymbol_ab4b4447,
				      EMPTYLIST ) );
	INITIALIZEVAR( "UNINTERNED-SYMBOL?",
		       ADR( sc_uninterned_2dsymbol_3f_v ),
		       MAKEPROCEDURE( 1,
				      0,
				      sc_uninterned_2dsymbol_3f,
				      EMPTYLIST ) );
	INITIALIZEVAR( "CALL-WITH-CURRENT-CONTINUATION",
		       ADR( sc_ntinuation_1af38b9f_v ),
		       MAKEPROCEDURE( 1, 0, sc_callcc, EMPTYLIST ) );
	INITIALIZEVAR( "IMPLEMENTATION-INFORMATION",
		       ADR( sc_implementation_v ),
		       MAKEPROCEDURE( 0,
				      0, sc_implementation, EMPTYLIST ) );
	INITIALIZEVAR( "TIME-SLICE",
		       ADR( sc_time_2dslice_v ),
		       MAKEPROCEDURE( 0, 0, sc_time_2dslice, EMPTYLIST ) );
	INITIALIZEVAR( "SET-TIME-SLICE!",
		       ADR( sc_set_2dtime_2dslice_21_v ),
		       MAKEPROCEDURE( 1, 0, sc_set_2dtime_2dslice_21,
				      EMPTYLIST ) );
	INITIALIZEVAR( "STACK-SIZE",
		       ADR( sc_stack_2dsize_v ),
		       MAKEPROCEDURE( 0, 0, sc_stack_2dsize, EMPTYLIST ) );
	INITIALIZEVAR( "SET-STACK-SIZE!",
		       ADR( sc_set_2dstack_2dsize_21_v ),
		       MAKEPROCEDURE( 1, 0, sc_set_2dstack_2dsize_21,
				      EMPTYLIST ) );
	INITIALIZEVAR( "COLLECT-INFO",
		       ADR( sc_collect_2dinfo_v ),
		       MAKEPROCEDURE( 0, 0, sc_collect_2dinfo, EMPTYLIST ) );
	INITIALIZEVAR( "SET-GCINFO!",
		       ADR( sc_set_2dgcinfo_21_v ),
		       MAKEPROCEDURE( 1, 0, sc_set_2dgcinfo_21, EMPTYLIST ) );
	INITIALIZEVAR( "SET-GENERATION-LIMIT!",
		       ADR( sc_2dlimit_21_de4d3427_v ),
		       MAKEPROCEDURE( 1, 0, sc_2dlimit_21_de4d3427,
				      EMPTYLIST ) );
	INITIALIZEVAR( "SET-MAXIMUM-HEAP!",
		       ADR( sc_set_2dmaximum_2dheap_21_v ),
		       MAKEPROCEDURE( 1, 0, sc_set_2dmaximum_2dheap_21,
				      EMPTYLIST ) );
	INITIALIZEVAR( "TIME-OF-DAY",
		       ADR( sc_time_2dof_2dday_v ),
		       MAKEPROCEDURE( 0, 0, sc_time_2dof_2dday, EMPTYLIST ) );
	MAXDISPLAY( 0 );
	return;
}
