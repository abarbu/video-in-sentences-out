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


/* Some objects require cleanup actions when they are freed.  For example,
   when a file port is recovered, the corresponding file needs to be closed.
   Such objects are noted by the procedure (WHEN-UNREFERENCED object action),
   where object is any Scheme object and action is either #F indicating that
   nothing should be done, or a procedure that takes one argument.  When a
   procedure is supplied, it will be called when a garbage collection occurs
   and there are no references to that object.  In order to implement this
   function, the runtime system will keep two alists, SC_WHENFREED and
   SC_FREED.  The first list is those items requiring cleanup when they
   become free, and the second list is those items freed that require
   cleanup now.
*/

#if NO_GC
#define NORMAL 0
#define PTRFREE 0
#else
#include <gc/gc.h>
#if GC_WITHOUT_TABLES
#else
#include <gc/gc_inline.h>
#endif
# define PTRFREE 0
# define NORMAL  1
# define UNCOLLECTABLE 2
// #include <gc/private/gc_priv.h>
#endif

// TODO
extern TSCP  sc_whenfreed,
	     sc_freed;

/* A Scheme program can register a callback with the garbage collector that
   will be called following each collection.  This is done by setting the
   value of AFTER-COLLECT to a procedure that takes three arguments:  the
   heap size in bytes, the current allocation in bytes, and the percent of
   allocation that will force a total collection.
*/

/* Garbage collection and call-with-current-continuation need to know the
   base of the stack, i.e. the value of the stack pointer when the stack is
   empty.  It is computed at initialization time and stored in SC_STACKBASE.
   STACKPTR( x ) sets x to the address of the current top of stack (defined
   in objects.h as it needs to be inlined in compiled code).
*/

extern S2CINT  *sc_stackbase;

/* # of bytes to reserve in user supplied stack for handling stack overflow,
   garbage collection, etc.

   N.B.  This number is subject to change.
*/

#define STACKFUDGE 1500

extern void  sc_apply_when_unreferenced();

extern TSCP  sc_collect_v;

extern TSCP  sc_collect();

extern TSCP  sc_collect_2dall_v;

extern TSCP  sc_collect_2dall();

extern TSCP  sc_collect_2dinfo_v;

extern TSCP  sc_collect_2dinfo();

extern TSCP  sc_set_2dgcinfo_21_v;

extern TSCP  sc_set_2dgcinfo_21( XAL1( TSCP ) );

extern TSCP  sc_2dlimit_21_de4d3427_v;

extern TSCP  sc_2dlimit_21_de4d3427( XAL1( TSCP ) );

extern TSCP  sc_set_2dmaximum_2dheap_21_v;

extern TSCP  sc_set_2dmaximum_2dheap_21( XAL1( TSCP ) );

extern TSCP  sc_setgeneration( XAL2( TSCP *, TSCP ) );

extern SCP  sc_allocateheap( XAL4( S2CINT, S2CINT, S2CINT, S2CINT ) );

extern TSCP  sc_makedoublefloat( XAL1( double ) );

extern TSCP  sc_cons_v;

extern TSCP  sc_cons( XAL2( TSCP, TSCP ) );
