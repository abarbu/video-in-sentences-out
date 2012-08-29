/* SCHEME->C */

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

/* This module implements the object storage storage system. */

/* Import definitions */

#include <stdlib.h>		/* for abort(3) */
#include "objects.h"
#include "scinit.h"
#include "heap.h"
#include "callcc.h"
#include "apply.h"
#include "cio.h"

#if NO_GC
#else

#include <gc/gc_mark.h>

#if GC_WITHOUT_TABLES
#else
void **normal_freelists;
void **ptrfree_freelists;

#define MY_GC_FAST_MALLOC_GRANS(result,granules,tiny_fl,num_direct,kind,init) \
  {									\
    if (GC_EXPECT((granules) >= GC_TINY_FREELISTS,0)) {			\
      if (kind == NORMAL)						\
	result = GC_malloc(granules*GC_GRANULE_BYTES);			\
      else if (kind == PTRFREE)						\
	result = GC_malloc_atomic(granules*GC_GRANULE_BYTES);		\
      else { printf("kind error %d\n", kind); exit(1); }		\
    } else {								\
      void **my_fl = (tiny_fl) + (granules);				\
      void *my_entry=*my_fl;						\
      void *next;							\
									\
      while (GC_EXPECT((GC_word)my_entry				\
		       <= (num_direct) + GC_TINY_FREELISTS + 1, 0)) {	\
	/* Entry contains counter or NULL */				\
	if ((GC_word)my_entry - 1 < (num_direct)) {			\
	  /* Small counter value, not NULL */				\
	  *my_fl = (char *)my_entry + (granules) + 1;			\
	  if (kind == NORMAL)						\
	    result = GC_malloc(granules*GC_GRANULE_BYTES);		\
	  else if (kind == PTRFREE)					\
	    result = GC_malloc_atomic(granules*GC_GRANULE_BYTES);	\
	  /* printf("from list\n"); */					\
	  goto out;							\
	} else {							\
	  /* Large counter or NULL */					\
	  GC_generic_malloc_many(((granules) == 0? GC_GRANULE_BYTES :	\
				  GC_RAW_BYTES_FROM_INDEX(granules)),	\
				 kind, my_fl);				\
	  my_entry = *my_fl;						\
	  if (my_entry == 0) {						\
	    result = (*GC_get_oom_fn())((granules)*GC_GRANULE_BYTES);	\
	    goto out;							\
	  }								\
	}								\
      }									\
      next = *(void **)(my_entry);					\
      result = (void *)my_entry;					\
      *my_fl = next;							\
      init;								\
      GC_ASSERT(GC_size(result) >= (granules)*GC_GRANULE_BYTES);	\
      GC_ASSERT((kind) == PTRFREE || ((GC_word *)result)[1] == 0);	\
    out: ;								\
    }									\
  }
#endif
#endif

#ifndef NULL
#define NULL 0
#endif

#if defined(AMD64)
/* The following code is used to read the stack pointer.  The register
   number is passed in to force an argument to be on the stack, which in
   turn can be used to find the address of the top of stack.
*/

S2CINT  *sc_processor_register( S2CINT reg )
{
  asm ("mov %%rsp, %0;" :"=r"(reg));
  return reg;
}
#endif

#if defined(LINUX) || defined(HP700) \
  || defined(MIPS) || defined(FREEBSD) || defined(LINUX_ARM)
/* The following code is used to read the stack pointer.  The register
   number is passed in to force an argument to be on the stack, which in
   turn can be used to find the address of the top of stack.
*/

S2CINT  *sc_processor_register( S2CINT reg )
{
  asm ("mov %%esp, %0;" :"=r"(reg));
  return reg;
}
#endif

/* Set the following define to a non-zero value to enable all heap checks
   that are normally enabled by sc_gcinfo == 2.
*/

#define CHECK_S2C_HEAP 0

S2CINT	sc_gcinfo,		/* controls logging */
	sc_collecting;

TSCP	sc_whenfreed,		/* list of items needing cleanup when free */
	sc_freed;		/* list of free items to be cleanup */

TSCP	sc_after_2dcollect_v,	/* Collection status callback */
	sc__2afrozen_2dobjects_2a_v;
				/* User managed frozen object list */

S2CINT	*sc_stackbase;		/* pointer to base of the stack */

static double  starttime = 0.0;	/* Processor time at start of interval */
double  sc_usertime = 0.0,	/* Scheme time */
	sc_idletime = 0.0,	/* Outside Scheme time */
	sc_gctime = 0.0;	/* Collection time */

/* Application and collection time is kept by the following routine.  When
   a timer is stopped, it updates it and returns the delta added to it.
*/

double  sc_stoptimer( double *timer )
{
	double  currenttime, delta;

	currenttime = sc_cputime();
	delta = currenttime-starttime;
	*timer = *timer+delta;
	starttime = currenttime;
	return( delta );
}

/* Each time a weak-cons is created, an entry is made on the following list.
   Each entry of the list is a 3-element vector with the following fields:

      pointer to the next entry (or EMPTYLIST)
      pointer to the cons cell
      cell to hold the original value from the CAR of the cons cell
*/

#define  WEAK_LINK( x )	VECTOR_ELEMENT( x, C_FIXED( 0 ) )
#define  WEAK_CONS( x ) VECTOR_ELEMENT( x, C_FIXED( 1 ) )
#define  WEAK_CAR( x )  VECTOR_ELEMENT( x, C_FIXED( 2 ) )
#define  MAKE_WEAK	sc_make_2dvector( C_FIXED( 3 ), EMPTYLIST )


/* Errors detected during garbage collection are logged by the following
   procedure.  If any errors occur, the program will abort after logging
   them.  More than 30 errors will result in the program being aborted at
   once.
*/

static SCP  moving_object;

static S2CINT  pointer_errors = 0;

static void  pointererror( char* msg, S2CUINT pp )
{
	sc_log_string( "***** COLLECT pointer error in " );
	sc_log_hex( (S2CUINT)moving_object );
	sc_log_string ( ", " );
	sc_log_hex( (S2CUINT)pp );
	sc_log_string( msg );
	if  (++pointer_errors == 30)  sc_abort();
}

/* The size of an extended object in words is returned by the following
   function.
*/

static S2CINT  extendedsize( SCP obj )
{
	switch  (obj->extendedobj.tag)  {

	   case  SYMBOLTAG:
	      return( SYMBOLSIZE );

	   case  STRINGTAG:
	      return( STRINGSIZE( obj->string.length ) );

	   case  VECTORTAG:
	      return( VECTORSIZE( obj->vector.length ) );

	   case  PROCEDURETAG:
	      return( PROCEDURESIZE );

	   case  CLOSURETAG:
	      return( CLOSURESIZE( obj->closure.length ) );

	   case  CONTINUATIONTAG:
	      return( CONTINUATIONSIZE( obj->continuation.length ) );

	   case  RECORDTAG:
	      return( RECORDSIZE( obj->record.length ) );

	   case  DOUBLEFLOATTAG:
	      return( DOUBLEFLOATSIZE );

	   case  FORWARDTAG:
	      return( FORWARDSIZE( obj->forward.length ) );

	   case  WORDALIGNTAG:
	      return( WORDALIGNSIZE );

	   default:
	      sc_log_string( "***** COLLECT Unknown extended object: " );
	      sc_log_hex( (S2CUINT)obj );
	      sc_log_string( " " );
	      sc_log_hex( (S2CUINT)obj->extendedobj.tag );
	      sc_log_string( "\n" );
	      sc_abort();
	}
}

/* Object cleanup actions are invoked here at the end of garbage collection.
*/

void  sc_apply_when_unreferenced()
{
  // TODO
  return;

	/* TSCP  freed, object_procedure; */
	/* struct  {			/\* Save sc_unknowncall's state here *\/ */
	/*    TSCP  arg[MAXARGS]; */
	/*    TSCP  proc[4]; */
	/*    S2CINT   count; */
	/* } save; */
	/* S2CINT  i; */

	/* /\* Save sc_freed and sc_unknowncall's state *\/ */
	/* for  (i = 0; i < 4; i++)  save.proc[ i ] = sc_unknownproc[ i ]; */
	/* for  (i = 0; i < MAXARGS; i++)  save.arg[ i ] = sc_arg[ i ]; */
	/* save.count = sc_unknownargc; */
	/* freed = sc_freed; */
	/* sc_freed = EMPTYLIST; */

	/* /\* Apply the when-unreferenced procedures *\/ */
	/* while  (freed != EMPTYLIST)  { */
	/*    object_procedure = PAIR_CAR( freed ); */
	/*    sc_apply_2dtwo( PAIR_CDR( object_procedure ), */
	/* 		  sc_cons( PAIR_CAR( object_procedure ), EMPTYLIST ) ); */
	/*    freed = PAIR_CDR( freed ); */
	/* } */

	/* /\* Restore sc_unknowncall's state *\/ */
	/* for  (i = 0; i < 4; i++)  sc_unknownproc[ i ] = save.proc[ i ]; */
	/* for  (i = 0; i < MAXARGS; i++)  sc_arg[ i ] = save.arg[ i ]; */
	/* sc_unknownargc = save.count; */
}


/* This function is called to check the obarray to make sure that it is
   intact.
*/

static void  check_obarray()
{
	S2CINT  i, len;
	PATSCP  ep;
	TSCP  lp, symbol, value;
	SCP  obarray;

	obarray = T_U( sc_obarray );
	if  (TSCPTAG( sc_obarray ) != EXTENDEDTAG  ||
	     obarray->vector.tag != VECTORTAG)  {
	   sc_log_string( "***** COLLECT OBARRAY is not a vector " );
	   sc_log_hex( (S2CUINT)sc_obarray );
	   sc_log_string( "\n" );
	   sc_abort();
	}
	len = obarray->vector.length;
	if  (len != 1023)   {
	   sc_log_string( "***** COLLECT OBARRAY length is wrong " );
	   sc_log_hex( (S2CUINT)sc_obarray );
	   sc_log_string( "\n" );
	   sc_abort();
	}
	ep = &obarray->vector.element0;
	for  (i = 0;  i < len;  i++)  {
	   lp = *ep++;
	   while  (lp != EMPTYLIST)  {
	      if  (TSCPTAG( lp ) != PAIRTAG)  {
		 sc_log_string(
		 	"***** COLLECT OBARRAY element is not a list " );
		 sc_log_hex( (S2CUINT)lp );
		 sc_log_string( "\n" );
		 sc_abort();
	      }
	      symbol = T_U( lp )->pair.car;
	      if  (T_U( symbol )->symbol.tag != SYMBOLTAG)  {
		 sc_log_string(
			"***** COLLECT OBARRAY entry is not a symbol " );
		 sc_log_hex( (S2CUINT)symbol );
		 sc_log_string( "\n" );
		 sc_abort();
	      }
	      lp = T_U( lp )->pair.cdr;
	   }
	}
}

/* Garbage collection is invoked to attempt to recover free storage when a
   request for storage cannot be met.  It will recover using a generational
   version of the "mostly copying" method.  See the .h file or the research
   reports for more details.
*/

TSCP  sc_collect_v;

TSCP  sc_collect()
{
	MUTEXON;
#if NO_GC
#else
	GC_gcollect();
#endif
	MUTEXOFF;
	return( TRUEVALUE );
}

/* A complete garbage collection can be forced by calling the following
   procedure.
*/

TSCP  sc_collect_2dall_v;

TSCP  sc_collect_2dall()
{
	MUTEXON;
#if NO_GC
#else
	// FIXME THIS IS DEBIAN IDIOCY
#if GC_WITHOUT_TABLES
	GC_gcollect();
#else
	GC_gcollect_and_unmap();
#endif
#endif
	MUTEXOFF;
	return( TRUEVALUE );
}

/* Information about the heap is returned by the following procedure.  It
   returns a list of the currently allocated heap (in bytes), the total
   size of the heap (in bytes), the total time spent in the application
   (in seconds), the total time spent garbage collecting (in seconds), the
   maximum size of the heap (in bytes), and the generational collection limit
  (a per cent).
*/

TSCP  sc_collect_2dinfo_v;

TSCP  sc_collect_2dinfo()
{
	double  currenttime;

	currenttime = sc_cputime();
#if NO_GC
	return( sc_cons( C_FIXED( 0 ),
		sc_cons( C_FIXED( 0 ),
		sc_cons( DOUBLE_TSCP( sc_usertime+currenttime-starttime ),
		sc_cons( DOUBLE_TSCP( 0 ),
		sc_cons( C_FIXED( 0 ),
		sc_cons( C_FIXED( 0 ), EMPTYLIST ) ) ) ) ) ) );
#else
	return( sc_cons( C_FIXED( GC_get_heap_size()-GC_get_free_bytes() ),
		sc_cons( C_FIXED( GC_get_heap_size() ),
		sc_cons( DOUBLE_TSCP( sc_usertime+currenttime-starttime ),
		sc_cons( DOUBLE_TSCP( 0 ),
		sc_cons( C_FIXED( 0 ),
		sc_cons( C_FIXED( 0 ), EMPTYLIST ) ) ) ) ) ) );
#endif

}

/* The logging of garbage collection information in controlled by the
   following procedure.
*/

TSCP  sc_set_2dgcinfo_21_v;

TSCP  sc_set_2dgcinfo_21( TSCP flag )
{
	return( C_FIXED( 0 ) );
}

/* The generational collection limit is set by the following procedure. */

TSCP  sc_2dlimit_21_de4d3427_v;

TSCP  sc_2dlimit_21_de4d3427( TSCP limit )
{
  if  (TSCPTAG( limit ) != FIXNUMTAG  ||
       FIXED_C( limit ) < 10  ||  FIXED_C( limit ) > 45)
    sc_error( "SET-GENERATION-LIMIT!",
	      "ARGUMENT is not in the range [10-45]: ~s",
	      LIST1( limit ) );
  // TODO GC_call_with_alloc_lock
  GC_set_free_space_divisor(FIXED_C( limit ));
  return( limit );
}

/* The maximum heap size is set by the following procedure. */

TSCP  sc_set_2dmaximum_2dheap_21_v;

TSCP  sc_set_2dmaximum_2dheap_21( TSCP maxheap )
{
  if (TSCPTAG(maxheap)==FIXNUMTAG)
    GC_set_max_heap_size(FIXED_C(maxheap)/sizeof(void*));
  else if (TSCPTAG(maxheap)==EXTENDEDTAG&&
	   TSCP_EXTENDEDTAG(maxheap)==DOUBLEFLOATTAG)
    GC_set_max_heap_size(((int)FLOAT_VALUE(maxheap))/sizeof(void*));
  else sc_error("SET-MAXIMUM-HEAP!",
		"ARGUMENT is not a number: ~s",
		LIST1(maxheap));
  return(maxheap);
}

inline SCP sc_allocateheap( S2CINT wordsize, S2CINT tag, S2CINT rest, S2CINT kind )
{
  SCP alloc;
#if NO_GC
  alloc = malloc(wordsize*sizeof(S2CINT));
#else
  if(NORMAL == kind)
    alloc = GC_MALLOC(wordsize*sizeof(S2CINT));
  else if(PTRFREE == kind)
    alloc = GC_MALLOC_ATOMIC(wordsize*sizeof(S2CINT));
  else { printf("Bad allocateheap kind %d\n", kind); exit(1); }
#endif
  alloc->extendedobj.tag = tag;
  alloc->extendedobj.rest = rest;
  return( alloc );
}


/* Double floating point numbers are constructed by the following function.  It
   is called with a double floating point value and it returns a pointer to
   the Scheme object with that value.
*/

#ifdef SPARC
extern void sc_set_double( XAL2( int* , double ) );
#define SET_FLOAT_VALUE( scp, val ) sc_set_double(&(scp)->doublefloat.value[0], (val) )
#else
#define SET_FLOAT_VALUE( scp, val ) (scp)->doublefloat.value = (val)
#endif


TSCP sc_makedoublefloat( double value )
{
	MUTEXON;
	SCP pp = sc_allocateheap( DOUBLEFLOATSIZE, DOUBLEFLOATTAG, 0, PTRFREE );
	SET_FLOAT_VALUE( pp, value );
	MUTEXOFF;
	return( U_T( pp, EXTENDEDTAG ) );
}

/* The following function forms a dotted-pair with any two Scheme pointers.  It
   returns a tagged pointer to the pair as its value.
*/

TSCP  sc_cons_v;

TSCP  sc_cons( TSCP x, TSCP y )
{
#if NO_GC
  SCP  oconsp = malloc(CONSBYTES);
#else
#if GC_WITHOUT_TABLES
   SCP  oconsp = GC_MALLOC(CONSBYTES);
#else
  /* FIXME: One past the end issue in gc_inline.h */
  SCP  oconsp;
  // TODO: 4 hardcoded
  size_t grans = GC_WORDS_TO_WHOLE_GRANULES((CONSSIZE*sizeof(S2CINT)/4));
  MY_GC_FAST_MALLOC_GRANS(oconsp, grans, normal_freelists, 0, NORMAL, {} );
#endif
#endif
  oconsp->pair.car = x;
  oconsp->pair.cdr = y;
  return( U_T( oconsp, PAIRTAG ) );
}

/* The following boolean is used by the stack tracing code to decide whether
   a pointer is a TSCP or a C string.

   The implementation with the original Scheme->C garbage collector
   this was guaranteed to always return the right answer, this is now
   a heuristic */

TSCP  sc_schemepointer( TSCP any )
{
  int tag = TSCPTAG(any);
  SCP pp = T_U(any);
  void *base = GC_base((void*)pp);

  if(!base)
    return FALSEVALUE;

  if(FIXNUMTAG == tag
     || EXTENDEDTAG == tag
     || SYMBOLTAG == tag
     || STRINGTAG == tag
     || VECTORTAG == tag
     || PROCEDURETAG == tag
     || CONTINUATIONTAG == tag
     || CLOSURETAG == tag
     || RECORDTAG == tag
     || DOUBLEFLOATTAG == tag
     || IMMEDIATETAG == tag
     || PAIRTAG == tag
     || CHARACTERTAG == tag)
    /* FORWARDTAG and WORDALIGNTAG do not occur anymore*/
    return TRUEVALUE;

  return FALSEVALUE;
}

/* The following procedure is used to verify that a Scheme data structure is
   correct.  If the structure is correct, it is returned.  If it is not
   correct, then the structure is dumped on the log and #f is returned.
*/

struct  SEEN { struct SEEN* prev;  TSCP value; };

struct SEEN*  seenp;

/* Put a breakpoint on this procedure to catch verification problems */

#ifdef __GNUC__
static void verifyfail() __attribute__((noreturn));
#endif
static void verifyfail()
{
	sc_abort();
}

TSCP  sc_verifyobject( TSCP any )
{
	S2CINT  i;
	struct SEEN  seen, *sp;

	if  ((S2CINT)any & 1)  {
	   sp = seenp;
	   while  (sp != NULL)  {
	      if  (sp->value == any)  return( any );
	      sp = sp->prev;
	   }
	   seen.prev = seenp;
	   seenp = &seen;
	   seen.value = any;
	}

	switch  TSCPTAG( any )  {

	   case  FIXNUMTAG:
	      return( any );

	   case  EXTENDEDTAG:
	      if  (any == sc_emptyvector  ||  any == sc_emptystring)  {
		 seenp = seen.prev;
		 return( any );
	      }
	      if  (sc_schemepointer( any ) == FALSEVALUE)  verifyfail();
	      switch  TSCP_EXTENDEDTAG( any )  {

		 case  SYMBOLTAG:
		    sc_verifyobject( SYMBOL_NAME( any ) );
		    sc_verifyobject( SYMBOL_VALUE( any ) );
		    sc_verifyobject( SYMBOL_PROPERTYLIST( any ) );
		    seenp = seen.prev;
		    return( any );

		 case  STRINGTAG:
		    seenp = seen.prev;
		    return( any );

		 case  VECTORTAG:
		    for  (i = 0; i < VECTOR_LENGTH( any ); i++ )
		       sc_verifyobject( VECTOR_ELEMENT( any, C_FIXED( i ) ) );
		    seenp = seen.prev;
		    return( any );

		 case  PROCEDURETAG:
		    sc_verifyobject( PROCEDURE_CLOSURE( any ) );
		    seenp = seen.prev;
		    return( any );

		 case  CLOSURETAG:
		    sc_verifyobject( CLOSURE_CLOSURE( any ) );
		    for  (i = 0; i < CLOSURE_LENGTH( any ); i++)
		       sc_verifyobject( CLOSURE_VAR( any, i ) );
		    seenp = seen.prev;
		    return( any );

		 case  CONTINUATIONTAG:
		    seenp = seen.prev;
		    return( any );

		 case  RECORDTAG:
		    sc_verifyobject( RECORD_METHODS( any ) );
		    for  (i = 0; i < RECORD_LENGTH( any ); i++ )
		       sc_verifyobject( RECORD_ELEMENT( any, C_FIXED( i ) ) );
		    seenp = seen.prev;
		    return( any );

		 case  DOUBLEFLOATTAG:
		    seenp = seen.prev;
		    return( any );

		 default:
		    verifyfail();
	      }

	   case  IMMEDIATETAG:
	      if  (any == EMPTYLIST  ||  any == FALSEVALUE  ||
		   any == TRUEVALUE  ||  any == EOFOBJECT   ||
		   any == UNDEFINED  ||
		   TSCPIMMEDIATETAG( any ) == CHARACTERTAG)
		 return( any );
	      verifyfail();

	   case  PAIRTAG:
	      if  (sc_schemepointer( any ) == FALSEVALUE)  verifyfail();
	      sc_verifyobject( PAIR_CAR( any ) );
	      sc_verifyobject( PAIR_CDR( any ) );
	      seenp = seen.prev;
	      return( any );
	   default:
	     verifyfail();
	}
}
