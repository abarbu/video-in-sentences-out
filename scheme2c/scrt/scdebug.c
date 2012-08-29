
/* SCHEME->C */

#include <objects.h>

void scdebug__init();
DEFCSTRING( t2851, "SCDEBUG_TIMEOUT" );
DEFSTATICTSCP( c2803 );
DEFCSTRING( t2852, "main" );
DEFSTATICTSCP( c2765 );
DEFCSTRING( t2853, "ERROR" );
DEFSTATICTSCP( c2757 );
DEFCSTRING( t2854, "***** ~a " );
DEFSTATICTSCP( c2752 );
DEFSTATICTSCP( c2724 );
DEFCSTRING( t2855, "***** ERROR error handler failed!" );
DEFSTATICTSCP( c2721 );
DEFCSTRING( t2856, "SCRT4_CALLSIGNALHANDLER" );
DEFSTATICTSCP( c2710 );
DEFCSTRING( t2857, ">> " );
DEFSTATICTSCP( c2709 );
DEFCSTRING( t2858, "~%***** INTERRUPT *****~%" );
DEFSTATICTSCP( c2708 );
DEFSTATICTSCP( c2699 );
DEFSTATICTSCP( t2859 );
DEFSTATICTSCP( t2860 );
DEFSTATICTSCP( t2861 );
DEFSTATICTSCP( t2862 );
DEFSTATICTSCP( t2863 );
DEFSTATICTSCP( t2864 );
DEFSTATICTSCP( t2865 );
DEFSTATICTSCP( t2866 );
DEFSTATICTSCP( t2867 );
DEFSTATICTSCP( t2868 );
DEFSTATICTSCP( t2869 );
DEFSTATICTSCP( t2870 );
DEFSTATICTSCP( t2871 );
DEFSTATICTSCP( t2872 );
DEFSTATICTSCP( t2873 );
DEFSTATICTSCP( t2874 );
DEFSTATICTSCP( t2875 );
DEFSTATICTSCP( t2876 );
DEFSTATICTSCP( t2877 );
DEFSTATICTSCP( t2878 );
DEFSTATICTSCP( t2879 );
DEFSTATICTSCP( t2880 );
DEFSTATICTSCP( t2881 );
DEFSTATICTSCP( t2882 );
DEFSTATICTSCP( t2883 );
DEFSTATICTSCP( t2884 );
DEFSTATICTSCP( c2698 );
DEFSTATICTSCP( c2693 );
DEFCSTRING( t2885, " in " );
DEFSTATICTSCP( c2670 );
DEFCSTRING( t2886, " ..." );
DEFSTATICTSCP( c2669 );
DEFCSTRING( t2887, "Argument is not a STRING: ~s" );
DEFSTATICTSCP( c2667 );
DEFSTATICTSCP( c2666 );
DEFCSTRING( t2888, " ...)" );
DEFSTATICTSCP( c2645 );
DEFCSTRING( t2889, "(" );
DEFSTATICTSCP( c2644 );
DEFSTATICTSCP( c2640 );
DEFCSTRING( t2891, "LOOP [inside EXEC]" );
DEFSTATICTSCP( t2890 );
DEFCSTRING( t2893, "SCEVAL_INTERPRETED-PROC" );
DEFSTATICTSCP( t2892 );
DEFCSTRING( t2894, "~s is not breakpointed" );
DEFSTATICTSCP( c2541 );
DEFSTATICTSCP( c2536 );
DEFSTATICTSCP( c2471 );
DEFSTATICTSCP( c2464 );
DEFCSTRING( t2895, "~s- " );
DEFSTATICTSCP( c2460 );
DEFSTATICTSCP( c2459 );
DEFCSTRING( t2896, "~s -returns- ~s" );
DEFSTATICTSCP( c2458 );
DEFCSTRING( t2897, "READ-EVAL-PRINT" );
DEFSTATICTSCP( c2445 );
DEFSTATICTSCP( c2444 );
DEFSTATICTSCP( c2443 );
DEFCSTRING( t2898, "~%~s -calls  - ~s" );
DEFSTATICTSCP( c2442 );
DEFSTATICTSCP( c2441 );
DEFCSTRING( t2899, "SCHEME2C" );
DEFSTATICTSCP( c2424 );
DEFCSTRING( t2900, "EMBEDDED [inside BPTER]" );
DEFSTATICTSCP( c2423 );
DEFCSTRING( t2901, " -returns- " );
DEFSTATICTSCP( c2410 );
DEFCSTRING( t2902, " -calls  - " );
DEFSTATICTSCP( c2393 );
DEFSTATICTSCP( c2391 );
DEFCSTRING( t2903, "INTERACTIVE [inside BPTER]" );
DEFSTATICTSCP( c2365 );
DEFCSTRING( t2904, "Not at a breakpoint" );
DEFSTATICTSCP( c2363 );
DEFSTATICTSCP( c2362 );
DEFSTATICTSCP( c2341 );
DEFSTATICTSCP( t2905 );
DEFSTATICTSCP( t2906 );
DEFSTATICTSCP( t2907 );
DEFSTATICTSCP( t2908 );
DEFSTATICTSCP( t2909 );
DEFSTATICTSCP( c2326 );
DEFSTATICTSCP( c2325 );
DEFSTATICTSCP( c2324 );
DEFCSTRING( t2910, "Illegal arguments" );
DEFSTATICTSCP( c2321 );
DEFSTATICTSCP( c2305 );
DEFCSTRING( t2911, "~s is not traced" );
DEFSTATICTSCP( c2290 );
DEFSTATICTSCP( c2285 );
DEFCSTRING( t2912, "Argument not a PAIR: ~s" );
DEFSTATICTSCP( c2270 );
DEFSTATICTSCP( c2269 );
DEFSTATICTSCP( c2218 );
DEFCSTRING( t2913, "==> " );
DEFSTATICTSCP( c2217 );
DEFCSTRING( t2914, "~a~a~s~%" );
DEFSTATICTSCP( c2204 );
DEFCSTRING( t2915, "~a~s~%" );
DEFSTATICTSCP( c2173 );
DEFCSTRING( t2916, "~s is already traced" );
DEFSTATICTSCP( c2164 );
DEFCSTRING( t2917, "Argument is not a PROCEDURE name" );
DEFSTATICTSCP( c2163 );
DEFSTATICTSCP( c2148 );
DEFSTATICTSCP( c2147 );
DEFSTATICTSCP( c2146 );
DEFSTATICTSCP( c2145 );
DEFSTATICTSCP( c2144 );
DEFSTATICTSCP( c2143 );
DEFSTATICTSCP( t2918 );
DEFSTATICTSCP( c2135 );

static void  init_constants()
{
        TSCP  X2, X1;

        c2803 = CSTRING_TSCP( t2851 );
        CONSTANTEXP( ADR( c2803 ) );
        c2765 = CSTRING_TSCP( t2852 );
        CONSTANTEXP( ADR( c2765 ) );
        c2757 = CSTRING_TSCP( t2853 );
        CONSTANTEXP( ADR( c2757 ) );
        c2752 = CSTRING_TSCP( t2854 );
        CONSTANTEXP( ADR( c2752 ) );
        c2724 = STRINGTOSYMBOL( CSTRING_TSCP( "***** inside ERROR" ) );
        CONSTANTEXP( ADR( c2724 ) );
        c2721 = CSTRING_TSCP( t2855 );
        CONSTANTEXP( ADR( c2721 ) );
        c2710 = CSTRING_TSCP( t2856 );
        CONSTANTEXP( ADR( c2710 ) );
        c2709 = CSTRING_TSCP( t2857 );
        CONSTANTEXP( ADR( c2709 ) );
        c2708 = CSTRING_TSCP( t2858 );
        CONSTANTEXP( ADR( c2708 ) );
        c2699 = EMPTYLIST;
        t2859 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-25" ) );
        c2699 = CONS( t2859, c2699 );
        t2860 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-24" ) );
        c2699 = CONS( t2860, c2699 );
        t2861 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-23" ) );
        c2699 = CONS( t2861, c2699 );
        t2862 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-22" ) );
        c2699 = CONS( t2862, c2699 );
        t2863 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-21" ) );
        c2699 = CONS( t2863, c2699 );
        t2864 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-20" ) );
        c2699 = CONS( t2864, c2699 );
        t2865 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-19" ) );
        c2699 = CONS( t2865, c2699 );
        t2866 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-18" ) );
        c2699 = CONS( t2866, c2699 );
        t2867 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-17" ) );
        c2699 = CONS( t2867, c2699 );
        t2868 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-16" ) );
        c2699 = CONS( t2868, c2699 );
        t2869 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-15" ) );
        c2699 = CONS( t2869, c2699 );
        t2870 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-14" ) );
        c2699 = CONS( t2870, c2699 );
        t2871 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-13" ) );
        c2699 = CONS( t2871, c2699 );
        t2872 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-12" ) );
        c2699 = CONS( t2872, c2699 );
        t2873 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-11" ) );
        c2699 = CONS( t2873, c2699 );
        t2874 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-10" ) );
        c2699 = CONS( t2874, c2699 );
        t2875 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-9" ) );
        c2699 = CONS( t2875, c2699 );
        t2876 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-8" ) );
        c2699 = CONS( t2876, c2699 );
        t2877 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-7" ) );
        c2699 = CONS( t2877, c2699 );
        t2878 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-6" ) );
        c2699 = CONS( t2878, c2699 );
        t2879 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-5" ) );
        c2699 = CONS( t2879, c2699 );
        t2880 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-4" ) );
        c2699 = CONS( t2880, c2699 );
        t2881 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-3" ) );
        c2699 = CONS( t2881, c2699 );
        t2882 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-2" ) );
        c2699 = CONS( t2882, c2699 );
        t2883 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-1" ) );
        c2699 = CONS( t2883, c2699 );
        t2884 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-0" ) );
        c2699 = CONS( t2884, c2699 );
        CONSTANTEXP( ADR( c2699 ) );
        c2698 = STRINGTOSYMBOL( CSTRING_TSCP( "INTERACTIVE" ) );
        CONSTANTEXP( ADR( c2698 ) );
        c2693 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV-0" ) );
        CONSTANTEXP( ADR( c2693 ) );
        c2670 = CSTRING_TSCP( t2885 );
        CONSTANTEXP( ADR( c2670 ) );
        c2669 = CSTRING_TSCP( t2886 );
        CONSTANTEXP( ADR( c2669 ) );
        c2667 = CSTRING_TSCP( t2887 );
        CONSTANTEXP( ADR( c2667 ) );
        c2666 = STRINGTOSYMBOL( CSTRING_TSCP( "STRING-LENGTH" ) );
        CONSTANTEXP( ADR( c2666 ) );
        c2645 = CSTRING_TSCP( t2888 );
        CONSTANTEXP( ADR( c2645 ) );
        c2644 = CSTRING_TSCP( t2889 );
        CONSTANTEXP( ADR( c2644 ) );
        c2640 = EMPTYLIST;
        t2890 = CSTRING_TSCP( t2891 );
        c2640 = CONS( t2890, c2640 );
        t2892 = CSTRING_TSCP( t2893 );
        c2640 = CONS( t2892, c2640 );
        CONSTANTEXP( ADR( c2640 ) );
        c2541 = CSTRING_TSCP( t2894 );
        CONSTANTEXP( ADR( c2541 ) );
        c2536 = STRINGTOSYMBOL( CSTRING_TSCP( "DOUNBPT" ) );
        CONSTANTEXP( ADR( c2536 ) );
        c2471 = STRINGTOSYMBOL( CSTRING_TSCP( "UNBPT" ) );
        CONSTANTEXP( ADR( c2471 ) );
        c2464 = STRINGTOSYMBOL( CSTRING_TSCP( "EMBEDDED" ) );
        CONSTANTEXP( ADR( c2464 ) );
        c2460 = CSTRING_TSCP( t2895 );
        CONSTANTEXP( ADR( c2460 ) );
        c2459 = STRINGTOSYMBOL( CSTRING_TSCP( "RESULT" ) );
        CONSTANTEXP( ADR( c2459 ) );
        c2458 = CSTRING_TSCP( t2896 );
        CONSTANTEXP( ADR( c2458 ) );
        c2445 = CSTRING_TSCP( t2897 );
        CONSTANTEXP( ADR( c2445 ) );
        c2444 = STRINGTOSYMBOL( CSTRING_TSCP( "ENV" ) );
        CONSTANTEXP( ADR( c2444 ) );
        c2443 = STRINGTOSYMBOL( CSTRING_TSCP( "PROMPT" ) );
        CONSTANTEXP( ADR( c2443 ) );
        c2442 = CSTRING_TSCP( t2898 );
        CONSTANTEXP( ADR( c2442 ) );
        c2441 = STRINGTOSYMBOL( CSTRING_TSCP( "HEADER" ) );
        CONSTANTEXP( ADR( c2441 ) );
        c2424 = CSTRING_TSCP( t2899 );
        CONSTANTEXP( ADR( c2424 ) );
        c2423 = CSTRING_TSCP( t2900 );
        CONSTANTEXP( ADR( c2423 ) );
        c2410 = CSTRING_TSCP( t2901 );
        CONSTANTEXP( ADR( c2410 ) );
        c2393 = CSTRING_TSCP( t2902 );
        CONSTANTEXP( ADR( c2393 ) );
        c2391 = STRINGTOSYMBOL( CSTRING_TSCP( "BACKTRACE:" ) );
        CONSTANTEXP( ADR( c2391 ) );
        c2365 = CSTRING_TSCP( t2903 );
        CONSTANTEXP( ADR( c2365 ) );
        c2363 = CSTRING_TSCP( t2904 );
        CONSTANTEXP( ADR( c2363 ) );
        c2362 = STRINGTOSYMBOL( CSTRING_TSCP( "PROCEED" ) );
        CONSTANTEXP( ADR( c2362 ) );
        c2341 = EMPTYLIST;
        t2905 = STRINGTOSYMBOL( CSTRING_TSCP( "BPT-PROCS" ) );
        c2341 = CONS( t2905, c2341 );
        X1 = EMPTYLIST;
        X2 = EMPTYLIST;
        t2906 = STRINGTOSYMBOL( CSTRING_TSCP( "X" ) );
        X2 = CONS( t2906, X2 );
        t2907 = STRINGTOSYMBOL( CSTRING_TSCP( "CAR" ) );
        X2 = CONS( t2907, X2 );
        X1 = CONS( X2, X1 );
        X2 = EMPTYLIST;
        X2 = CONS( t2906, X2 );
        X1 = CONS( X2, X1 );
        t2908 = STRINGTOSYMBOL( CSTRING_TSCP( "LAMBDA" ) );
        X1 = CONS( t2908, X1 );
        c2341 = CONS( X1, c2341 );
        t2909 = STRINGTOSYMBOL( CSTRING_TSCP( "MAP" ) );
        c2341 = CONS( t2909, c2341 );
        CONSTANTEXP( ADR( c2341 ) );
        c2326 = STRINGTOSYMBOL( CSTRING_TSCP( "LIST" ) );
        CONSTANTEXP( ADR( c2326 ) );
        c2325 = STRINGTOSYMBOL( CSTRING_TSCP( "DOBPT" ) );
        CONSTANTEXP( ADR( c2325 ) );
        c2324 = STRINGTOSYMBOL( CSTRING_TSCP( "APPLY" ) );
        CONSTANTEXP( ADR( c2324 ) );
        c2321 = CSTRING_TSCP( t2910 );
        CONSTANTEXP( ADR( c2321 ) );
        c2305 = STRINGTOSYMBOL( CSTRING_TSCP( "BPT" ) );
        CONSTANTEXP( ADR( c2305 ) );
        c2290 = CSTRING_TSCP( t2911 );
        CONSTANTEXP( ADR( c2290 ) );
        c2285 = STRINGTOSYMBOL( CSTRING_TSCP( "DOUNTRACE" ) );
        CONSTANTEXP( ADR( c2285 ) );
        c2270 = CSTRING_TSCP( t2912 );
        CONSTANTEXP( ADR( c2270 ) );
        c2269 = STRINGTOSYMBOL( CSTRING_TSCP( "SET-CDR!" ) );
        CONSTANTEXP( ADR( c2269 ) );
        c2218 = STRINGTOSYMBOL( CSTRING_TSCP( "UNTRACE" ) );
        CONSTANTEXP( ADR( c2218 ) );
        c2217 = CSTRING_TSCP( t2913 );
        CONSTANTEXP( ADR( c2217 ) );
        c2204 = CSTRING_TSCP( t2914 );
        CONSTANTEXP( ADR( c2204 ) );
        c2173 = CSTRING_TSCP( t2915 );
        CONSTANTEXP( ADR( c2173 ) );
        c2164 = CSTRING_TSCP( t2916 );
        CONSTANTEXP( ADR( c2164 ) );
        c2163 = CSTRING_TSCP( t2917 );
        CONSTANTEXP( ADR( c2163 ) );
        c2148 = STRINGTOSYMBOL( CSTRING_TSCP( "QUOTE" ) );
        CONSTANTEXP( ADR( c2148 ) );
        c2147 = STRINGTOSYMBOL( CSTRING_TSCP( "DOTRACE" ) );
        CONSTANTEXP( ADR( c2147 ) );
        c2146 = STRINGTOSYMBOL( CSTRING_TSCP( "F" ) );
        CONSTANTEXP( ADR( c2146 ) );
        c2145 = STRINGTOSYMBOL( CSTRING_TSCP( "LAMBDA" ) );
        CONSTANTEXP( ADR( c2145 ) );
        c2144 = STRINGTOSYMBOL( CSTRING_TSCP( "MAP" ) );
        CONSTANTEXP( ADR( c2144 ) );
        c2143 = EMPTYLIST;
        t2918 = STRINGTOSYMBOL( CSTRING_TSCP( "TRACED-PROCS" ) );
        c2143 = CONS( t2918, c2143 );
        X1 = EMPTYLIST;
        X2 = EMPTYLIST;
        X2 = CONS( t2906, X2 );
        X2 = CONS( t2907, X2 );
        X1 = CONS( X2, X1 );
        X2 = EMPTYLIST;
        X2 = CONS( t2906, X2 );
        X1 = CONS( X2, X1 );
        X1 = CONS( c2145, X1 );
        c2143 = CONS( X1, c2143 );
        c2143 = CONS( c2144, c2143 );
        CONSTANTEXP( ADR( c2143 ) );
        c2135 = STRINGTOSYMBOL( CSTRING_TSCP( "TRACE" ) );
        CONSTANTEXP( ADR( c2135 ) );
}

DEFTSCP( scdebug_trace_2dlevel_v );
DEFCSTRING( t2919, "SCDEBUG_TRACE-LEVEL" );
DEFTSCP( scdebug_traced_2dprocs_v );
DEFCSTRING( t2920, "TRACED-PROCS" );
DEFTSCP( scdebug_bpt_2dprocs_v );
DEFCSTRING( t2921, "BPT-PROCS" );
DEFTSCP( scdebug__2aargs_2a_v );
DEFCSTRING( t2922, "*ARGS*" );
DEFTSCP( scdebug__2aresult_2a_v );
DEFCSTRING( t2923, "*RESULT*" );
DEFTSCP( scdebug__2abpt_2denv_2a_v );
DEFCSTRING( t2924, "*BPT-ENV*" );
EXTERNTSCPP( scexpand_install_2dexpander, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scexpand_install_2dexpander_v );
EXTERNTSCPP( scrt1__24__cdr_2derror, XAL1( TSCP ) );
EXTERNTSCP( scrt1__24__cdr_2derror_v );
EXTERNTSCPP( scrt1_cons_2a, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt1_cons_2a_v );

TSCP  scdebug_l2136( x2137, e2138 )
        TSCP  x2137, e2138;
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( "scdebug_l2136 [inside TOP-LEVEL]" );
        if  ( EQ( TSCPTAG( x2137 ), PAIRTAG ) )  goto L2927;
        scrt1__24__cdr_2derror( x2137 );
L2927:
        if  ( FALSE( PAIR_CDR( x2137 ) ) )  goto L2929;
        X1 = CONS( EMPTYLIST, EMPTYLIST );
        X2 = CONS( EMPTYLIST, EMPTYLIST );
        X1 = CONS( scrt1_cons_2a( c2148, 
                                  CONS( PAIR_CDR( x2137 ), X2 ) ), 
                   X1 );
        X2 = CONS( EMPTYLIST, EMPTYLIST );
        X3 = CONS( EMPTYLIST, EMPTYLIST );
        X2 = CONS( scrt1_cons_2a( c2147, CONS( c2146, X3 ) ), 
                   X2 );
        POPSTACKTRACE( scrt1_cons_2a( c2144, 
                                      CONS( scrt1_cons_2a( c2145, 
                                                           CONS( scrt1_cons_2a( c2146, 
                                                                                CONS( EMPTYLIST, 
                                                                                      EMPTYLIST ) ), 
                                                                 X2 ) ), 
                                            X1 ) ) );
L2929:
        POPSTACKTRACE( c2143 );
}

DEFTSCP( scdebug_dotrace_v );
DEFCSTRING( t2932, "DOTRACE" );
EXTERNTSCPP( scrt1_assoc, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt1_assoc_v );
EXTERNTSCPP( scdebug_dountrace, XAL1( TSCP ) );
EXTERNTSCP( scdebug_dountrace_v );
EXTERNTSCPP( scdebug_dounbpt, XAL1( TSCP ) );
EXTERNTSCP( scdebug_dounbpt_v );
EXTERNTSCPP( scrt2_top_2dlevel_2dvalue, XAL1( TSCP ) );
EXTERNTSCP( scrt2_top_2dlevel_2dvalue_v );
EXTERNTSCPP( scdebug_error, XAL3( TSCP, TSCP, TSCP ) );
EXTERNTSCP( scdebug_error_v );
EXTERNTSCPP( scdebug_tracer, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scdebug_tracer_v );
EXTERNTSCPP( sc_cons, XAL2( TSCP, TSCP ) );
EXTERNTSCP( sc_cons_v );
EXTERNTSCPP( scrt2_2dvalue_21_c9d2a496, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt2_2dvalue_21_c9d2a496_v );

TSCP  scdebug_dotrace( n2155 )
        TSCP  n2155;
{
        TSCP  X6, X5, X4, X3, X2, X1;

        PUSHSTACKTRACE( t2932 );
        if  ( FALSE( scrt1_assoc( n2155, scdebug_traced_2dprocs_v ) )
            )  goto L2934;
        scdebug_dountrace( n2155 );
L2934:
        if  ( FALSE( scrt1_assoc( n2155, scdebug_bpt_2dprocs_v ) )
            )  goto L2936;
        scdebug_dounbpt( n2155 );
L2936:
        X1 = FALSEVALUE;
        X2 = scrt2_top_2dlevel_2dvalue( n2155 );
        X1 = CONS( X1, EMPTYLIST );
        if  ( AND( EQ( TSCPTAG( X2 ), EXTENDEDTAG ), 
                   EQ( TSCP_EXTENDEDTAG( X2 ), PROCEDURETAG ) )
            )  goto L2939;
        scdebug_error( c2135, c2163, EMPTYLIST );
L2939:
        if  ( FALSE( scrt1_assoc( n2155, scdebug_traced_2dprocs_v ) )
            )  goto L2941;
        scdebug_error( c2135, 
                       c2164, CONS( n2155, EMPTYLIST ) );
L2941:
        X3 = scdebug_tracer( n2155, X2 );
        SETGEN( PAIR_CAR( X1 ), X3 );
        X6 = sc_cons( PAIR_CAR( X1 ), EMPTYLIST );
        X5 = sc_cons( X2, X6 );
        X4 = sc_cons( n2155, X5 );
        X3 = X4;
        scdebug_traced_2dprocs_v = sc_cons( X3, 
                                            scdebug_traced_2dprocs_v );
        scrt2_2dvalue_21_c9d2a496( n2155, PAIR_CAR( X1 ) );
        POPSTACKTRACE( n2155 );
}

DEFTSCP( scdebug_tracer_v );
DEFCSTRING( t2944, "TRACER" );
EXTERNTSCPP( scrt6_format, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt6_format_v );
EXTERNTSCP( scrt5_trace_2doutput_2dport_v );
EXTERNTSCPP( sc_make_2dstring, XAL2( TSCP, TSCP ) );
EXTERNTSCP( sc_make_2dstring_v );
EXTERNTSCPP( scrt2_min_2dtwo, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt2_min_2dtwo_v );
EXTERNTSCPP( scrt2__2a_2dtwo, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt2__2a_2dtwo_v );
EXTERNTSCPP( scrt2__2b_2dtwo, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt2__2b_2dtwo_v );
EXTERNTSCPP( sc_apply_2dtwo, XAL2( TSCP, TSCP ) );
EXTERNTSCP( sc_apply_2dtwo_v );
EXTERNTSCPP( scrt2__2d_2dtwo, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt2__2d_2dtwo_v );

TSCP  scdebug_l2171( x2172, c2946 )
        TSCP  x2172, c2946;
{
        TSCP  X8, X7, X6, X5, X4, X3, X2, X1;

        PUSHSTACKTRACE( "scdebug_l2171 [inside TRACER]" );
        X1 = DISPLAY( 0 );
        DISPLAY( 0 ) = CLOSURE_VAR( c2946, 0 );
        X2 = DISPLAY( 1 );
        DISPLAY( 1 ) = CLOSURE_VAR( c2946, 1 );
        X4 = CONS( sc_cons( DISPLAY( 0 ), x2172 ), 
                   EMPTYLIST );
        X7 = scdebug_trace_2dlevel_v;
        if  ( BITAND( BITOR( _S2CINT( X7 ), 
                             _S2CINT( _TSCP( 60 ) ) ), 
                      3 ) )  goto L2949;
        if  ( GTE( _S2CINT( X7 ), _S2CINT( _TSCP( 60 ) ) ) )  goto L2951;
        X6 = X7;
        goto L2950;
L2951:
        X6 = _TSCP( 60 );
        goto L2950;
L2949:
        X6 = scrt2_min_2dtwo( X7, _TSCP( 60 ) );
L2950:
        if  ( BITAND( BITOR( _S2CINT( _TSCP( 8 ) ), 
                             _S2CINT( X6 ) ), 
                      3 ) )  goto L2954;
        X5 = _TSCP( ITIMES( FIXED_C( _TSCP( 8 ) ), 
                            _S2CINT( X6 ) ) );
        goto L2955;
L2954:
        X5 = scrt2__2a_2dtwo( _TSCP( 8 ), X6 );
L2955:
        X4 = CONS( sc_make_2dstring( X5, 
                                     CONS( _TSCP( 8210 ), EMPTYLIST ) ), 
                   X4 );
        scrt6_format( scrt5_trace_2doutput_2dport_v, 
                      CONS( c2173, X4 ) );
        X4 = scdebug_trace_2dlevel_v;
        if  ( BITAND( BITOR( _S2CINT( X4 ), 
                             _S2CINT( _TSCP( 4 ) ) ), 
                      3 ) )  goto L2957;
        X5 = _TSCP( IPLUS( _S2CINT( X4 ), 
                           _S2CINT( _TSCP( 4 ) ) ) );
        goto L2958;
L2957:
        X5 = scrt2__2b_2dtwo( X4, _TSCP( 4 ) );
L2958:
        scdebug_trace_2dlevel_v = X5;
        X4 = sc_apply_2dtwo( DISPLAY( 1 ), x2172 );
        X5 = scdebug_trace_2dlevel_v;
        if  ( BITAND( BITOR( _S2CINT( X5 ), 
                             _S2CINT( _TSCP( 4 ) ) ), 
                      3 ) )  goto L2961;
        X6 = _TSCP( IDIFFERENCE( _S2CINT( X5 ), 
                                 _S2CINT( _TSCP( 4 ) ) ) );
        goto L2962;
L2961:
        X6 = scrt2__2d_2dtwo( X5, _TSCP( 4 ) );
L2962:
        scdebug_trace_2dlevel_v = X6;
        X5 = CONS( X4, EMPTYLIST );
        X5 = CONS( c2217, X5 );
        X8 = scdebug_trace_2dlevel_v;
        if  ( BITAND( BITOR( _S2CINT( X8 ), 
                             _S2CINT( _TSCP( 60 ) ) ), 
                      3 ) )  goto L2964;
        if  ( GTE( _S2CINT( X8 ), _S2CINT( _TSCP( 60 ) ) ) )  goto L2966;
        X7 = X8;
        goto L2965;
L2966:
        X7 = _TSCP( 60 );
        goto L2965;
L2964:
        X7 = scrt2_min_2dtwo( X8, _TSCP( 60 ) );
L2965:
        if  ( BITAND( BITOR( _S2CINT( _TSCP( 8 ) ), 
                             _S2CINT( X7 ) ), 
                      3 ) )  goto L2969;
        X6 = _TSCP( ITIMES( FIXED_C( _TSCP( 8 ) ), 
                            _S2CINT( X7 ) ) );
        goto L2970;
L2969:
        X6 = scrt2__2a_2dtwo( _TSCP( 8 ), X7 );
L2970:
        X5 = CONS( sc_make_2dstring( X6, 
                                     CONS( _TSCP( 8210 ), EMPTYLIST ) ), 
                   X5 );
        scrt6_format( scrt5_trace_2doutput_2dport_v, 
                      CONS( c2204, X5 ) );
        X3 = X4;
        DISPLAY( 0 ) = X1;
        DISPLAY( 1 ) = X2;
        POPSTACKTRACE( X3 );
}

TSCP  scdebug_tracer( n2169, p2170 )
        TSCP  n2169, p2170;
{
        TSCP  SD0 = DISPLAY( 0 );
        TSCP  SD1 = DISPLAY( 1 );
        TSCP  SDVAL;

        PUSHSTACKTRACE( t2944 );
        DISPLAY( 0 ) = n2169;
        DISPLAY( 1 ) = p2170;
        SDVAL = MAKEPROCEDURE( 0, 
                               1, 
                               scdebug_l2171, 
                               MAKECLOSURE( EMPTYLIST, 
                                            2, 
                                            DISPLAY( 0 ), 
                                            DISPLAY( 1 ) ) );
        DISPLAY( 0 ) = SD0;
        DISPLAY( 1 ) = SD1;
        POPSTACKTRACE( SDVAL );
}

EXTERNTSCPP( scrt1__24__car_2derror, XAL1( TSCP ) );
EXTERNTSCP( scrt1__24__car_2derror_v );

TSCP  scdebug_l2219( x2220, e2221 )
        TSCP  x2220, e2221;
{
        TSCP  X8, X7, X6, X5, X4, X3, X2, X1;

        PUSHSTACKTRACE( "scdebug_l2219 [inside TOP-LEVEL]" );
        x2220 = CONS( x2220, EMPTYLIST );
        X2 = PAIR_CAR( x2220 );
        if  ( EQ( TSCPTAG( X2 ), PAIRTAG ) )  goto L2975;
        scrt1__24__cdr_2derror( X2 );
L2975:
        X1 = PAIR_CDR( X2 );
        if  ( NEQ( _S2CUINT( X1 ), _S2CUINT( EMPTYLIST ) ) )  goto L2972;
        X2 = scdebug_traced_2dprocs_v;
        X3 = X2;
        X4 = EMPTYLIST;
        X5 = EMPTYLIST;
L2979:
        if  ( NEQ( _S2CUINT( X3 ), _S2CUINT( EMPTYLIST ) ) )  goto L2980;
        X1 = X4;
        goto L2991;
L2980:
        if  ( EQ( TSCPTAG( X3 ), PAIRTAG ) )  goto L2983;
        scrt1__24__car_2derror( X3 );
L2983:
        X8 = PAIR_CAR( X3 );
        if  ( EQ( TSCPTAG( X8 ), PAIRTAG ) )  goto L2987;
        scrt1__24__car_2derror( X8 );
L2987:
        X7 = PAIR_CAR( X8 );
        X6 = sc_cons( X7, EMPTYLIST );
        if  ( NEQ( _S2CUINT( X4 ), _S2CUINT( EMPTYLIST ) ) )  goto L2990;
        X7 = PAIR_CDR( X3 );
        X5 = X6;
        X4 = X6;
        X3 = X7;
        GOBACK( L2979 );
L2990:
        X7 = PAIR_CDR( X3 );
        if  ( EQ( TSCPTAG( X5 ), PAIRTAG ) )  goto L2995;
        scdebug_error( c2269, 
                       c2270, CONS( X5, EMPTYLIST ) );
L2995:
        X5 = SETGEN( PAIR_CDR( X5 ), X6 );
        X3 = X7;
        GOBACK( L2979 );
L2991:
        SETGEN( PAIR_CAR( x2220 ), X1 );
        goto L2973;
L2972:
        X2 = PAIR_CAR( x2220 );
        if  ( EQ( TSCPTAG( X2 ), PAIRTAG ) )  goto L2998;
        scrt1__24__cdr_2derror( X2 );
L2998:
        X1 = PAIR_CDR( X2 );
        SETGEN( PAIR_CAR( x2220 ), X1 );
L2973:
        X1 = CONS( EMPTYLIST, EMPTYLIST );
        X2 = CONS( EMPTYLIST, EMPTYLIST );
        X1 = CONS( scrt1_cons_2a( c2148, 
                                  CONS( PAIR_CAR( x2220 ), X2 ) ), 
                   X1 );
        X2 = CONS( EMPTYLIST, EMPTYLIST );
        X3 = CONS( EMPTYLIST, EMPTYLIST );
        X2 = CONS( scrt1_cons_2a( c2285, CONS( c2146, X3 ) ), 
                   X2 );
        POPSTACKTRACE( scrt1_cons_2a( c2144, 
                                      CONS( scrt1_cons_2a( c2145, 
                                                           CONS( scrt1_cons_2a( c2146, 
                                                                                CONS( EMPTYLIST, 
                                                                                      EMPTYLIST ) ), 
                                                                 X2 ) ), 
                                            X1 ) ) );
}

DEFTSCP( scdebug_dountrace_v );
DEFCSTRING( t3000, "DOUNTRACE" );
EXTERNTSCPP( scrt1_caddr, XAL1( TSCP ) );
EXTERNTSCP( scrt1_caddr_v );
EXTERNTSCPP( scrt1_remove, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt1_remove_v );

TSCP  scdebug_dountrace( n2287 )
        TSCP  n2287;
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( t3000 );
        X1 = scrt1_assoc( n2287, scdebug_traced_2dprocs_v );
        if  ( TRUE( X1 ) )  goto L3003;
        scdebug_error( c2218, 
                       c2290, CONS( n2287, EMPTYLIST ) );
L3003:
        X2 = scrt2_top_2dlevel_2dvalue( n2287 );
        X3 = scrt1_caddr( X1 );
        if  ( NEQ( _S2CUINT( X2 ), _S2CUINT( X3 ) ) )  goto L3005;
        if  ( EQ( TSCPTAG( X1 ), PAIRTAG ) )  goto L3008;
        scrt1__24__cdr_2derror( X1 );
L3008:
        X3 = PAIR_CDR( X1 );
        if  ( EQ( TSCPTAG( X3 ), PAIRTAG ) )  goto L3011;
        scrt1__24__car_2derror( X3 );
L3011:
        X2 = PAIR_CAR( X3 );
        scrt2_2dvalue_21_c9d2a496( n2287, X2 );
L3005:
        scdebug_traced_2dprocs_v = scrt1_remove( X1, 
                                                 scdebug_traced_2dprocs_v );
        POPSTACKTRACE( n2287 );
}

EXTERNTSCPP( scrt1_length, XAL1( TSCP ) );
EXTERNTSCP( scrt1_length_v );

TSCP  scdebug_l2306( x2307, e2308 )
        TSCP  x2307, e2308;
{
        TSCP  X6, X5, X4, X3, X2, X1;

        PUSHSTACKTRACE( "scdebug_l2306 [inside TOP-LEVEL]" );
        X1 = scrt1_length( x2307 );
        if  ( EQ( _S2CUINT( X1 ), _S2CUINT( _TSCP( 4 ) ) ) )  goto L3015;
        if  ( NEQ( _S2CUINT( X1 ), _S2CUINT( _TSCP( 8 ) ) ) )  goto L3017;
        X2 = CONS( EMPTYLIST, EMPTYLIST );
        X3 = CONS( EMPTYLIST, EMPTYLIST );
        if  ( EQ( TSCPTAG( x2307 ), PAIRTAG ) )  goto L3020;
        scrt1__24__cdr_2derror( x2307 );
L3020:
        X2 = CONS( scrt1_cons_2a( c2148, 
                                  CONS( PAIR_CDR( x2307 ), X3 ) ), 
                   X2 );
        POPSTACKTRACE( scrt1_cons_2a( c2324, 
                                      CONS( c2325, X2 ) ) );
L3017:
        if  ( NEQ( _S2CUINT( X1 ), _S2CUINT( _TSCP( 12 ) ) ) )  goto L3022;
        X4 = scrt1_caddr( x2307 );
        X3 = e2308;
        X3 = UNKNOWNCALL( X3, 2 );
        X2 = VIA( PROCEDURE_CODE( X3 ) )( X4, 
                                          e2308, 
                                          PROCEDURE_CLOSURE( X3 ) );
        X3 = CONS( EMPTYLIST, EMPTYLIST );
        X4 = CONS( EMPTYLIST, EMPTYLIST );
        X5 = CONS( EMPTYLIST, EMPTYLIST );
        X4 = CONS( scrt1_cons_2a( c2148, CONS( X2, X5 ) ), 
                   X4 );
        X5 = CONS( EMPTYLIST, EMPTYLIST );
        if  ( EQ( TSCPTAG( x2307 ), PAIRTAG ) )  goto L3026;
        scrt1__24__cdr_2derror( x2307 );
L3026:
        X6 = PAIR_CDR( x2307 );
        if  ( EQ( TSCPTAG( X6 ), PAIRTAG ) )  goto L3029;
        scrt1__24__car_2derror( X6 );
L3029:
        X3 = CONS( scrt1_cons_2a( c2326, 
                                  CONS( scrt1_cons_2a( c2148, 
                                                       CONS( PAIR_CAR( X6 ), 
                                                             X5 ) ), 
                                        X4 ) ), 
                   X3 );
        POPSTACKTRACE( scrt1_cons_2a( c2324, 
                                      CONS( c2325, X3 ) ) );
L3022:
        POPSTACKTRACE( scdebug_error( c2305, c2321, EMPTYLIST ) );
L3015:
        POPSTACKTRACE( c2341 );
}

DEFTSCP( scdebug_dobpt_v );
DEFCSTRING( t3031, "DOBPT" );
EXTERNTSCPP( scdebug_bpter, XAL3( TSCP, TSCP, TSCP ) );
EXTERNTSCP( scdebug_bpter_v );
EXTERNTSCPP( sceval_eval, XAL2( TSCP, TSCP ) );
EXTERNTSCP( sceval_eval_v );

TSCP  scdebug_dobpt( n2344, c2345 )
        TSCP  n2344, c2345;
{
        TSCP  X6, X5, X4, X3, X2, X1;

        PUSHSTACKTRACE( t3031 );
        if  ( FALSE( scrt1_assoc( n2344, scdebug_bpt_2dprocs_v ) )
            )  goto L3033;
        scdebug_dounbpt( n2344 );
L3033:
        X1 = FALSEVALUE;
        X2 = scrt2_top_2dlevel_2dvalue( n2344 );
        X1 = CONS( X1, EMPTYLIST );
        if  ( AND( EQ( TSCPTAG( X2 ), EXTENDEDTAG ), 
                   EQ( TSCP_EXTENDEDTAG( X2 ), PROCEDURETAG ) )
            )  goto L3036;
        scdebug_error( c2305, c2163, EMPTYLIST );
L3036:
        if  ( FALSE( c2345 ) )  goto L3038;
        if  ( EQ( TSCPTAG( c2345 ), PAIRTAG ) )  goto L3041;
        scrt1__24__car_2derror( c2345 );
L3041:
        X5 = PAIR_CAR( c2345 );
        X4 = sceval_eval( X5, EMPTYLIST );
        goto L3039;
L3038:
        X4 = FALSEVALUE;
L3039:
        X3 = scdebug_bpter( n2344, X2, X4 );
        SETGEN( PAIR_CAR( X1 ), X3 );
        X6 = sc_cons( PAIR_CAR( X1 ), EMPTYLIST );
        X5 = sc_cons( X2, X6 );
        X4 = sc_cons( n2344, X5 );
        X3 = X4;
        scdebug_bpt_2dprocs_v = sc_cons( X3, scdebug_bpt_2dprocs_v );
        scrt2_2dvalue_21_c9d2a496( n2344, PAIR_CAR( X1 ) );
        POPSTACKTRACE( n2344 );
}

DEFTSCP( scdebug_reset_2dbpt_v );
DEFCSTRING( t3044, "RESET-BPT" );
EXTERNTSCPP( scdebug_default_2dproceed, XAL0(  ) );
EXTERNTSCP( scdebug_default_2dproceed_v );
EXTERNTSCP( scdebug_proceed_v );

TSCP  scdebug_reset_2dbpt(  )
{
        PUSHSTACKTRACE( t3044 );
        scdebug_trace_2dlevel_v = _TSCP( 0 );
        scdebug_proceed_v = scdebug_default_2dproceed_v;
        POPSTACKTRACE( SET( scdebug__2abpt_2denv_2a_v, FALSEVALUE ) );
}

DEFTSCP( scdebug_default_2dproceed_v );
DEFCSTRING( t3046, "SCDEBUG_DEFAULT-PROCEED" );

TSCP  scdebug_default_2dproceed(  )
{
        PUSHSTACKTRACE( t3046 );
        POPSTACKTRACE( scdebug_error( c2362, c2363, EMPTYLIST ) );
}

DEFTSCP( scdebug_proceed_v );
DEFCSTRING( t3048, "PROCEED" );
DEFTSCP( scdebug_bpter_2dprocname_v );
DEFCSTRING( t3049, "SCDEBUG_BPTER-PROCNAME" );
DEFTSCP( scdebug_bpter_v );
DEFCSTRING( t3050, "SCDEBUG_BPTER" );
EXTERNTSCPP( scrt5_open_2doutput_2dstring, XAL0(  ) );
EXTERNTSCP( scrt5_open_2doutput_2dstring_v );
EXTERNTSCPP( scdebug_dobacktrace, 
             XAL4( TSCP, TSCP, TSCP, TSCP ) );
EXTERNTSCP( scdebug_dobacktrace_v );
EXTERNTSCPP( scrt6_get_2doutput_2dstring, XAL1( TSCP ) );
EXTERNTSCP( scrt6_get_2doutput_2dstring_v );
EXTERNTSCPP( scrt6_display, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt6_display_v );
EXTERNTSCP( scrt5_stderr_2dport_v );

TSCP  scdebug_l2390( c3075 )
        TSCP  c3075;
{
        TSCP  X2, X1;

        PUSHSTACKTRACE( "scdebug_l2390 [inside BPTER]" );
        X1 = DISPLAY( 3 );
        DISPLAY( 3 ) = CLOSURE_VAR( c3075, 0 );
        scrt6_display( DISPLAY( 3 ), 
                       CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        X2 = c2391;
        DISPLAY( 3 ) = X1;
        POPSTACKTRACE( X2 );
}

EXTERNTSCP( scdebug_backtrace_v );
EXTERNTSCPP( scrt6_write, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt6_write_v );
EXTERNTSCPP( scrt6_newline, XAL1( TSCP ) );
EXTERNTSCP( scrt6_newline_v );

TSCP  scdebug_l2396( c3079 )
        TSCP  c3079;
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( "scdebug_l2396 [inside BPTER]" );
        X1 = DISPLAY( 3 );
        DISPLAY( 3 ) = CLOSURE_VAR( c3079, 0 );
        X3 = DISPLAY( 3 );
        X3 = UNKNOWNCALL( X3, 1 );
        X2 = VIA( PROCEDURE_CODE( X3 ) )( TRUEVALUE, 
                                          PROCEDURE_CLOSURE( X3 ) );
        DISPLAY( 3 ) = X1;
        POPSTACKTRACE( X2 );
}

EXTERNTSCPP( screp_jump_2dto_2dscheme2c, XAL1( TSCP ) );
EXTERNTSCP( screp_jump_2dto_2dscheme2c_v );
EXTERNTSCP( screp__2ascheme2c_2dresult_2a_v );

TSCP  scdebug_l2394( c2395, c3077 )
        TSCP  c2395, c3077;
{
        TSCP  X1;
        TSCP  SD3 = DISPLAY( 3 );
        TSCP  SDVAL;

        PUSHSTACKTRACE( "scdebug_l2394 [inside BPTER]" );
        DISPLAY( 3 ) = c2395;
        scdebug_proceed_v = MAKEPROCEDURE( 0, 
                                           0, 
                                           scdebug_l2396, 
                                           MAKECLOSURE( EMPTYLIST, 
                                                        1, 
                                                        DISPLAY( 3 ) ) );
        X1 = CONS( _TSCP( 16 ), EMPTYLIST );
        SDVAL = screp_jump_2dto_2dscheme2c( CONS( screp__2ascheme2c_2dresult_2a_v, 
                                                  X1 ) );
        DISPLAY( 3 ) = SD3;
        POPSTACKTRACE( SDVAL );
}

EXTERNTSCP( sc_ntinuation_1af38b9f_v );

TSCP  scdebug_l2413( x2414, c3089 )
        TSCP  x2414, c3089;
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( "scdebug_l2413 [inside BPTER]" );
        X1 = DISPLAY( 3 );
        DISPLAY( 3 ) = CLOSURE_VAR( c3089, 0 );
        if  ( EQ( _S2CUINT( x2414 ), _S2CUINT( EMPTYLIST ) ) )  goto L3091;
        if  ( EQ( TSCPTAG( x2414 ), PAIRTAG ) )  goto L3094;
        scrt1__24__car_2derror( x2414 );
L3094:
        scdebug__2aresult_2a_v = PAIR_CAR( x2414 );
L3091:
        X3 = DISPLAY( 3 );
        X3 = UNKNOWNCALL( X3, 1 );
        X2 = VIA( PROCEDURE_CODE( X3 ) )( TRUEVALUE, 
                                          PROCEDURE_CLOSURE( X3 ) );
        DISPLAY( 3 ) = X1;
        POPSTACKTRACE( X2 );
}

TSCP  scdebug_l2411( c2412, c3087 )
        TSCP  c2412, c3087;
{
        TSCP  X1;
        TSCP  SD3 = DISPLAY( 3 );
        TSCP  SDVAL;

        PUSHSTACKTRACE( "scdebug_l2411 [inside BPTER]" );
        DISPLAY( 3 ) = c2412;
        scdebug_proceed_v = MAKEPROCEDURE( 0, 
                                           1, 
                                           scdebug_l2413, 
                                           MAKECLOSURE( EMPTYLIST, 
                                                        1, 
                                                        DISPLAY( 3 ) ) );
        X1 = CONS( _TSCP( 20 ), EMPTYLIST );
        SDVAL = screp_jump_2dto_2dscheme2c( CONS( screp__2ascheme2c_2dresult_2a_v, 
                                                  X1 ) );
        DISPLAY( 3 ) = SD3;
        POPSTACKTRACE( SDVAL );
}

TSCP  scdebug_e2373( x2375, c3053 )
        TSCP  x2375, c3053;
{
        TSCP  X8, X7, X6, X5, X4, X3, X2, X1;
        TSCP  SD3 = DISPLAY( 3 );
        TSCP  SDVAL;

        PUSHSTACKTRACE( "EMBEDDED [inside BPTER]" );
        X1 = DISPLAY( 0 );
        DISPLAY( 0 ) = CLOSURE_VAR( c3053, 0 );
        X2 = DISPLAY( 1 );
        DISPLAY( 1 ) = CLOSURE_VAR( c3053, 1 );
        X3 = DISPLAY( 2 );
        DISPLAY( 2 ) = CLOSURE_VAR( c3053, 2 );
        if  ( FALSE( scdebug__2abpt_2denv_2a_v ) )  goto L3055;
        X5 = FALSEVALUE;
        goto L3056;
L3055:
        X5 = TRUEVALUE;
L3056:
        if  ( FALSE( X5 ) )  goto L3070;
        if  ( FALSE( DISPLAY( 2 ) ) )  goto L3061;
        X6 = FALSEVALUE;
        goto L3062;
L3061:
        X6 = TRUEVALUE;
L3062:
        if  ( TRUE( X6 ) )  goto L3067;
        if  ( TRUE( sc_apply_2dtwo( DISPLAY( 2 ), x2375 ) ) )  goto L3067;
L3070:
        X4 = sc_apply_2dtwo( DISPLAY( 1 ), x2375 );
        goto L3071;
L3067:
        X5 = scrt5_open_2doutput_2dstring(  );
        X6 = scdebug_dobacktrace( c2423, 
                                  c2424, _TSCP( 80 ), X5 );
        scdebug__2aargs_2a_v = x2375;
        scdebug__2abpt_2denv_2a_v = X6;
        DISPLAY( 3 ) = scrt6_get_2doutput_2dstring( X5 );
        scdebug_backtrace_v = MAKEPROCEDURE( 0, 
                                             0, 
                                             scdebug_l2390, 
                                             MAKECLOSURE( EMPTYLIST, 
                                                          1, 
                                                          DISPLAY( 3 ) ) );
        scrt6_write( scdebug_trace_2dlevel_v, 
                     CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        scrt6_display( c2393, 
                       CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        X7 = sc_cons( DISPLAY( 0 ), x2375 );
        scrt6_write( X7, 
                     CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        scrt6_newline( CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        X8 = MAKEPROCEDURE( 1, 0, scdebug_l2394, EMPTYLIST );
        X7 = sc_ntinuation_1af38b9f_v;
        X7 = UNKNOWNCALL( X7, 1 );
        VIA( PROCEDURE_CODE( X7 ) )( X8, PROCEDURE_CLOSURE( X7 ) );
        scdebug__2abpt_2denv_2a_v = FALSEVALUE;
        X7 = scdebug_trace_2dlevel_v;
        if  ( BITAND( BITOR( _S2CINT( _TSCP( 4 ) ), 
                             _S2CINT( X7 ) ), 
                      3 ) )  goto L3082;
        X8 = _TSCP( IPLUS( _S2CINT( _TSCP( 4 ) ), 
                           _S2CINT( X7 ) ) );
        goto L3083;
L3082:
        X8 = scrt2__2b_2dtwo( _TSCP( 4 ), X7 );
L3083:
        scdebug_trace_2dlevel_v = X8;
        scdebug__2aresult_2a_v = sc_apply_2dtwo( DISPLAY( 1 ), 
                                                 scdebug__2aargs_2a_v );
        X7 = scdebug_trace_2dlevel_v;
        if  ( BITAND( BITOR( _S2CINT( _TSCP( 4 ) ), 
                             _S2CINT( X7 ) ), 
                      3 ) )  goto L3085;
        X8 = _TSCP( IDIFFERENCE( _S2CINT( _TSCP( 4 ) ), 
                                 _S2CINT( X7 ) ) );
        goto L3086;
L3085:
        X8 = scrt2__2d_2dtwo( _TSCP( 4 ), X7 );
L3086:
        scdebug_trace_2dlevel_v = X8;
        scdebug__2aargs_2a_v = x2375;
        scdebug__2abpt_2denv_2a_v = X6;
        scrt6_write( scdebug_trace_2dlevel_v, 
                     CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        scrt6_display( c2410, 
                       CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        scrt6_write( scdebug__2aresult_2a_v, 
                     CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        scrt6_newline( CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        X8 = MAKEPROCEDURE( 1, 0, scdebug_l2411, EMPTYLIST );
        X7 = sc_ntinuation_1af38b9f_v;
        X7 = UNKNOWNCALL( X7, 1 );
        VIA( PROCEDURE_CODE( X7 ) )( X8, PROCEDURE_CLOSURE( X7 ) );
        scdebug__2abpt_2denv_2a_v = FALSEVALUE;
        X4 = scdebug__2aresult_2a_v;
L3071:
        DISPLAY( 0 ) = X1;
        DISPLAY( 1 ) = X2;
        DISPLAY( 2 ) = X3;
        SDVAL = X4;
        DISPLAY( 3 ) = SD3;
        POPSTACKTRACE( SDVAL );
}

EXTERNTSCPP( scrt6_le_2dtasks_e4d983f4, XAL1( TSCP ) );
EXTERNTSCP( scrt6_le_2dtasks_e4d983f4_v );
EXTERNTSCPP( screp_read_2deval_2dprint, XAL1( TSCP ) );
EXTERNTSCP( screp_read_2deval_2dprint_v );

TSCP  scdebug_x2372( a2427 )
        TSCP  a2427;
{
        TSCP  X2, X1;

        PUSHSTACKTRACE( "XEQ [inside BPTER]" );
        X1 = scrt6_le_2dtasks_e4d983f4( FALSEVALUE );
        X2 = sc_apply_2dtwo( screp_read_2deval_2dprint_v, a2427 );
        scrt6_le_2dtasks_e4d983f4( X1 );
        POPSTACKTRACE( X2 );
}

EXTERNTSCPP( scdebug_dbacktrace_a8071371, XAL1( TSCP ) );
EXTERNTSCP( scdebug_dbacktrace_a8071371_v );

TSCP  scdebug_i2371( x2433, c3099 )
        TSCP  x2433, c3099;
{
        TSCP  X7, X6, X5, X4, X3, X2, X1;

        PUSHSTACKTRACE( "INTERACTIVE [inside BPTER]" );
        X1 = DISPLAY( 0 );
        DISPLAY( 0 ) = CLOSURE_VAR( c3099, 0 );
        X2 = DISPLAY( 1 );
        DISPLAY( 1 ) = CLOSURE_VAR( c3099, 1 );
        X3 = DISPLAY( 2 );
        DISPLAY( 2 ) = CLOSURE_VAR( c3099, 2 );
        if  ( FALSE( DISPLAY( 2 ) ) )  goto L3101;
        X5 = FALSEVALUE;
        goto L3102;
L3101:
        X5 = TRUEVALUE;
L3102:
        if  ( TRUE( X5 ) )  goto L3107;
        if  ( TRUE( sc_apply_2dtwo( DISPLAY( 2 ), x2433 ) ) )  goto L3107;
        X4 = sc_apply_2dtwo( DISPLAY( 1 ), x2433 );
        goto L3110;
L3107:
        X5 = scrt6_format( c2460, 
                           CONS( scdebug_trace_2dlevel_v, EMPTYLIST ) );
        scdebug_backtrace_v = scdebug_dbacktrace_a8071371_v;
        scdebug__2aargs_2a_v = x2433;
        X6 = CONS( scdebug_dobacktrace( scdebug_bpter_2dprocname_v, 
                                        c2445, 
                                        _TSCP( 80 ), FALSEVALUE ), 
                   EMPTYLIST );
        X6 = CONS( c2444, X6 );
        X6 = CONS( X5, X6 );
        X6 = CONS( c2443, X6 );
        X7 = CONS( sc_cons( DISPLAY( 0 ), x2433 ), 
                   EMPTYLIST );
        X6 = CONS( scrt6_format( c2442, 
                                 CONS( scdebug_trace_2dlevel_v, X7 ) ), 
                   X6 );
        scdebug_x2372( CONS( c2441, X6 ) );
        X6 = scdebug_trace_2dlevel_v;
        if  ( BITAND( BITOR( _S2CINT( X6 ), 
                             _S2CINT( _TSCP( 4 ) ) ), 
                      3 ) )  goto L3113;
        X7 = _TSCP( IPLUS( _S2CINT( X6 ), 
                           _S2CINT( _TSCP( 4 ) ) ) );
        goto L3114;
L3113:
        X7 = scrt2__2b_2dtwo( X6, _TSCP( 4 ) );
L3114:
        scdebug_trace_2dlevel_v = X7;
        scdebug__2aresult_2a_v = sc_apply_2dtwo( DISPLAY( 1 ), 
                                                 scdebug__2aargs_2a_v );
        X6 = scdebug_trace_2dlevel_v;
        if  ( BITAND( BITOR( _S2CINT( X6 ), 
                             _S2CINT( _TSCP( 4 ) ) ), 
                      3 ) )  goto L3116;
        X7 = _TSCP( IDIFFERENCE( _S2CINT( X6 ), 
                                 _S2CINT( _TSCP( 4 ) ) ) );
        goto L3117;
L3116:
        X7 = scrt2__2d_2dtwo( X6, _TSCP( 4 ) );
L3117:
        scdebug_trace_2dlevel_v = X7;
        X6 = CONS( scdebug_dobacktrace( scdebug_bpter_2dprocname_v, 
                                        c2445, 
                                        _TSCP( 80 ), FALSEVALUE ), 
                   EMPTYLIST );
        X6 = CONS( c2444, X6 );
        X6 = CONS( scdebug__2aresult_2a_v, X6 );
        X6 = CONS( c2459, X6 );
        X6 = CONS( X5, X6 );
        X6 = CONS( c2443, X6 );
        X7 = CONS( scdebug__2aresult_2a_v, EMPTYLIST );
        X6 = CONS( scrt6_format( c2458, 
                                 CONS( scdebug_trace_2dlevel_v, X7 ) ), 
                   X6 );
        X4 = scdebug_x2372( CONS( c2441, X6 ) );
L3110:
        DISPLAY( 0 ) = X1;
        DISPLAY( 1 ) = X2;
        DISPLAY( 2 ) = X3;
        POPSTACKTRACE( X4 );
}

EXTERNTSCPP( sc_scheme_2dmode, XAL0(  ) );
EXTERNTSCP( sc_scheme_2dmode_v );

TSCP  scdebug_bpter( n2367, p2368, c2369 )
        TSCP  n2367, p2368, c2369;
{
        TSCP  X3, X2, X1;
        TSCP  SD0 = DISPLAY( 0 );
        TSCP  SD1 = DISPLAY( 1 );
        TSCP  SD2 = DISPLAY( 2 );
        TSCP  SDVAL;

        PUSHSTACKTRACE( t3050 );
        DISPLAY( 0 ) = n2367;
        DISPLAY( 1 ) = p2368;
        DISPLAY( 2 ) = c2369;
        X1 = _TSCP( 0 );
        X2 = _TSCP( 0 );
        X2 = CONS( X2, EMPTYLIST );
        X1 = CONS( X1, EMPTYLIST );
        X3 = MAKEPROCEDURE( 0, 
                            1, 
                            scdebug_e2373, 
                            MAKECLOSURE( EMPTYLIST, 
                                         3, 
                                         DISPLAY( 0 ), 
                                         DISPLAY( 1 ), 
                                         DISPLAY( 2 ) ) );
        SETGEN( PAIR_CAR( X1 ), X3 );
        X3 = MAKEPROCEDURE( 0, 
                            1, 
                            scdebug_i2371, 
                            MAKECLOSURE( EMPTYLIST, 
                                         3, 
                                         DISPLAY( 0 ), 
                                         DISPLAY( 1 ), 
                                         DISPLAY( 2 ) ) );
        SETGEN( PAIR_CAR( X2 ), X3 );
        X3 = sc_scheme_2dmode(  );
        if  ( EQ( _S2CUINT( X3 ), _S2CUINT( c2464 ) ) )  goto L3118;
        SDVAL = PAIR_CAR( X2 );
        DISPLAY( 0 ) = SD0;
        DISPLAY( 1 ) = SD1;
        DISPLAY( 2 ) = SD2;
        POPSTACKTRACE( SDVAL );
L3118:
        SDVAL = PAIR_CAR( X1 );
        DISPLAY( 0 ) = SD0;
        DISPLAY( 1 ) = SD1;
        DISPLAY( 2 ) = SD2;
        POPSTACKTRACE( SDVAL );
}

DEFTSCP( scdebug_backtrace_v );
DEFCSTRING( t3120, "BACKTRACE" );
DEFTSCP( scdebug_dbacktrace_a8071371_v );
DEFCSTRING( t3121, "SCDEBUG_INTERACTIVE-BACKTRACE" );
EXTERNTSCP( scrt5_debug_2doutput_2dport_v );

TSCP  scdebug_dbacktrace_a8071371( c2466 )
        TSCP  c2466;
{
        TSCP  X1;

        PUSHSTACKTRACE( t3121 );
        if  ( FALSE( c2466 ) )  goto L3123;
        if  ( EQ( TSCPTAG( c2466 ), PAIRTAG ) )  goto L3126;
        scrt1__24__car_2derror( c2466 );
L3126:
        X1 = PAIR_CAR( c2466 );
        goto L3124;
L3123:
        X1 = _TSCP( 80 );
L3124:
        scdebug_dobacktrace( scdebug_bpter_2dprocname_v, 
                             c2445, 
                             X1, scrt5_debug_2doutput_2dport_v );
        POPSTACKTRACE( FALSEVALUE );
}

TSCP  scdebug_l2472( x2473, e2474 )
        TSCP  x2473, e2474;
{
        TSCP  X8, X7, X6, X5, X4, X3, X2, X1;

        PUSHSTACKTRACE( "scdebug_l2472 [inside TOP-LEVEL]" );
        x2473 = CONS( x2473, EMPTYLIST );
        X2 = PAIR_CAR( x2473 );
        if  ( EQ( TSCPTAG( X2 ), PAIRTAG ) )  goto L3132;
        scrt1__24__cdr_2derror( X2 );
L3132:
        X1 = PAIR_CDR( X2 );
        if  ( NEQ( _S2CUINT( X1 ), _S2CUINT( EMPTYLIST ) ) )  goto L3129;
        X2 = scdebug_bpt_2dprocs_v;
        X3 = X2;
        X4 = EMPTYLIST;
        X5 = EMPTYLIST;
L3136:
        if  ( NEQ( _S2CUINT( X3 ), _S2CUINT( EMPTYLIST ) ) )  goto L3137;
        X1 = X4;
        goto L3148;
L3137:
        if  ( EQ( TSCPTAG( X3 ), PAIRTAG ) )  goto L3140;
        scrt1__24__car_2derror( X3 );
L3140:
        X8 = PAIR_CAR( X3 );
        if  ( EQ( TSCPTAG( X8 ), PAIRTAG ) )  goto L3144;
        scrt1__24__car_2derror( X8 );
L3144:
        X7 = PAIR_CAR( X8 );
        X6 = sc_cons( X7, EMPTYLIST );
        if  ( NEQ( _S2CUINT( X4 ), _S2CUINT( EMPTYLIST ) ) )  goto L3147;
        X7 = PAIR_CDR( X3 );
        X5 = X6;
        X4 = X6;
        X3 = X7;
        GOBACK( L3136 );
L3147:
        X7 = PAIR_CDR( X3 );
        if  ( EQ( TSCPTAG( X5 ), PAIRTAG ) )  goto L3152;
        scdebug_error( c2269, 
                       c2270, CONS( X5, EMPTYLIST ) );
L3152:
        X5 = SETGEN( PAIR_CDR( X5 ), X6 );
        X3 = X7;
        GOBACK( L3136 );
L3148:
        SETGEN( PAIR_CAR( x2473 ), X1 );
        goto L3130;
L3129:
        X2 = PAIR_CAR( x2473 );
        if  ( EQ( TSCPTAG( X2 ), PAIRTAG ) )  goto L3155;
        scrt1__24__cdr_2derror( X2 );
L3155:
        X1 = PAIR_CDR( X2 );
        SETGEN( PAIR_CAR( x2473 ), X1 );
L3130:
        X1 = CONS( EMPTYLIST, EMPTYLIST );
        X2 = CONS( EMPTYLIST, EMPTYLIST );
        X1 = CONS( scrt1_cons_2a( c2148, 
                                  CONS( PAIR_CAR( x2473 ), X2 ) ), 
                   X1 );
        X2 = CONS( EMPTYLIST, EMPTYLIST );
        X3 = CONS( EMPTYLIST, EMPTYLIST );
        X2 = CONS( scrt1_cons_2a( c2536, CONS( c2146, X3 ) ), 
                   X2 );
        POPSTACKTRACE( scrt1_cons_2a( c2144, 
                                      CONS( scrt1_cons_2a( c2145, 
                                                           CONS( scrt1_cons_2a( c2146, 
                                                                                CONS( EMPTYLIST, 
                                                                                      EMPTYLIST ) ), 
                                                                 X2 ) ), 
                                            X1 ) ) );
}

DEFTSCP( scdebug_dounbpt_v );
DEFCSTRING( t3157, "DOUNBPT" );

TSCP  scdebug_dounbpt( n2538 )
        TSCP  n2538;
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( t3157 );
        X1 = scrt1_assoc( n2538, scdebug_bpt_2dprocs_v );
        if  ( TRUE( X1 ) )  goto L3160;
        scdebug_error( c2471, 
                       c2541, CONS( n2538, EMPTYLIST ) );
L3160:
        X2 = scrt2_top_2dlevel_2dvalue( n2538 );
        X3 = scrt1_caddr( X1 );
        if  ( NEQ( _S2CUINT( X2 ), _S2CUINT( X3 ) ) )  goto L3162;
        if  ( EQ( TSCPTAG( X1 ), PAIRTAG ) )  goto L3165;
        scrt1__24__cdr_2derror( X1 );
L3165:
        X3 = PAIR_CDR( X1 );
        if  ( EQ( TSCPTAG( X3 ), PAIRTAG ) )  goto L3168;
        scrt1__24__car_2derror( X3 );
L3168:
        X2 = PAIR_CAR( X3 );
        scrt2_2dvalue_21_c9d2a496( n2538, X2 );
L3162:
        scdebug_bpt_2dprocs_v = scrt1_remove( X1, 
                                              scdebug_bpt_2dprocs_v );
        if  ( NEQ( _S2CUINT( scdebug_bpt_2dprocs_v ), 
                   _S2CUINT( EMPTYLIST ) ) )  goto L3170;
        scdebug_reset_2dbpt(  );
L3170:
        POPSTACKTRACE( n2538 );
}

DEFTSCP( scdebug_procnamex_v );
DEFCSTRING( t3172, "SCDEBUG_PROCNAMEX" );
DEFTSCP( scdebug_expx_v );
DEFCSTRING( t3176, "SCDEBUG_EXPX" );
DEFTSCP( scdebug_dobacktrace_v );
DEFCSTRING( t3180, "SCDEBUG_DOBACKTRACE" );
EXTERNTSCP( sc_emptystring );
EXTERNTSCPP( scrt6_set_2dwrite_2dcircle_21, 
             XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt6_set_2dwrite_2dcircle_21_v );
EXTERNTSCPP( scrt6_set_2dwrite_2dlevel_21, 
             XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt6_set_2dwrite_2dlevel_21_v );
EXTERNTSCPP( scrt6_set_2dwrite_2dlength_21, 
             XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt6_set_2dwrite_2dlength_21_v );
EXTERNTSCPP( scrt2__3d_2dtwo, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt2__3d_2dtwo_v );
EXTERNTSCPP( scrt1_equal_3f, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt1_equal_3f_v );
EXTERNTSCPP( sc_schemepointer, XAL1( TSCP ) );
EXTERNTSCPP( scrt4_c_2dtscp_2dref, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt4_c_2dtscp_2dref_v );
EXTERNTSCPP( sc_c_2dstring_2d_3estring, XAL1( TSCP ) );
EXTERNTSCP( sc_c_2dstring_2d_3estring_v );
EXTERNTSCPP( scrt4_c_2ds2cuint_2dref, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt4_c_2ds2cuint_2dref_v );
EXTERNTSCPP( scrt1_member, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt1_member_v );
EXTERNTSCPP( scrt2__3e_2dtwo, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt2__3e_2dtwo_v );
EXTERNTSCPP( scrt3_string_2dappend, XAL1( TSCP ) );
EXTERNTSCP( scrt3_string_2dappend_v );
EXTERNTSCPP( scrt3_substring, XAL3( TSCP, TSCP, TSCP ) );
EXTERNTSCP( scrt3_substring_v );
EXTERNTSCPP( scrt1_append_2dtwo, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt1_append_2dtwo_v );
EXTERNTSCPP( scrt1_assq, XAL2( TSCP, TSCP ) );
EXTERNTSCP( scrt1_assq_v );
EXTERNTSCPP( scrt1_reverse, XAL1( TSCP ) );
EXTERNTSCP( scrt1_reverse_v );

TSCP  scdebug_dobacktrace( s2575, s2576, l2577, p2578 )
        TSCP  s2575, s2576, l2577, p2578;
{
        TSCP  X10, 
              X9, 
              X8, X7, X6, X5, X4, X3, X2, X1;

        PUSHSTACKTRACE( t3180 );
        s2575 = CONS( s2575, EMPTYLIST );
        l2577 = CONS( l2577, EMPTYLIST );
        X1 = S2CUINT_TSCP( sc_stacktrace );
        X3 = sc_scheme_2dmode(  );
        X2 = BOOLEAN( EQ( _S2CUINT( X3 ), 
                          _S2CUINT( c2698 ) ) );
        X3 = sc_emptystring;
        X4 = EMPTYLIST;
        X5 = c2699;
        X7 = scrt5_open_2doutput_2dstring(  );
        scrt6_set_2dwrite_2dcircle_21( TRUEVALUE, 
                                       CONS( X7, EMPTYLIST ) );
        scrt6_set_2dwrite_2dlevel_21( _TSCP( 40 ), 
                                      CONS( X7, EMPTYLIST ) );
        scrt6_set_2dwrite_2dlength_21( _TSCP( 80 ), 
                                       CONS( X7, EMPTYLIST ) );
        X6 = X7;
L3184:
        X5 = CONS( X5, EMPTYLIST );
        X4 = CONS( X4, EMPTYLIST );
        X3 = CONS( X3, EMPTYLIST );
        if  ( BITAND( BITOR( _S2CINT( X1 ), 
                             _S2CINT( _TSCP( 0 ) ) ), 
                      3 ) )  goto L3185;
        X7 = BOOLEAN( EQ( _S2CUINT( X1 ), 
                          _S2CUINT( _TSCP( 0 ) ) ) );
        goto L3186;
L3185:
        X7 = scrt2__3d_2dtwo( X1, _TSCP( 0 ) );
L3186:
        if  ( TRUE( X7 ) )  goto L3191;
        X9 = PAIR_CAR( l2577 );
        if  ( BITAND( BITOR( _S2CINT( X9 ), 
                             _S2CINT( _TSCP( 0 ) ) ), 
                      3 ) )  goto L3193;
        X8 = BOOLEAN( EQ( _S2CUINT( X9 ), 
                          _S2CUINT( _TSCP( 0 ) ) ) );
        goto L3194;
L3193:
        X8 = scrt2__3d_2dtwo( X9, _TSCP( 0 ) );
L3194:
        if  ( TRUE( X8 ) )  goto L3191;
        if  ( EQ( _S2CUINT( PAIR_CAR( X5 ) ), 
                  _S2CUINT( EMPTYLIST ) ) )  goto L3191;
        if  ( FALSE( PAIR_CAR( s2575 ) ) )  goto L3204;
        X9 = FALSEVALUE;
        goto L3205;
L3204:
        X9 = TRUEVALUE;
L3205:
        if  ( FALSE( X9 ) )  goto L3212;
        if  ( TRUE( scrt1_equal_3f( PAIR_CAR( X3 ), s2576 ) )
            )  goto L3191;
L3212:
        X9 = scrt4_c_2dtscp_2dref( X1, scdebug_procnamex_v );
        X8 = sc_schemepointer( X9 );
        if  ( TRUE( X8 ) )  goto L3218;
        X9 = scrt4_c_2dtscp_2dref( X1, scdebug_procnamex_v );
        if  ( EQ( _S2CUINT( X9 ), _S2CUINT( EMPTYLIST ) ) )  goto L3218;
        X9 = scrt4_c_2ds2cuint_2dref( X1, scdebug_procnamex_v );
        X7 = sc_c_2dstring_2d_3estring( X9 );
        goto L3221;
L3218:
        X7 = scrt4_c_2dtscp_2dref( X1, scdebug_procnamex_v );
L3221:
        SETGEN( PAIR_CAR( X3 ), X7 );
        if  ( FALSE( PAIR_CAR( s2575 ) ) )  goto L3222;
        if  ( FALSE( scrt1_equal_3f( PAIR_CAR( s2575 ), 
                                     PAIR_CAR( X3 ) ) ) )  goto L3227;
        X7 = FALSEVALUE;
        SETGEN( PAIR_CAR( s2575 ), X7 );
        goto L3227;
L3222:
        if  ( NOT( AND( EQ( TSCPTAG( PAIR_CAR( X3 ) ), 
                            EXTENDEDTAG ), 
                        EQ( TSCP_EXTENDEDTAG( PAIR_CAR( X3 ) ), 
                            STRINGTAG ) ) ) )  goto L3226;
        X7 = scrt1_member( PAIR_CAR( X3 ), c2640 );
        if  ( TRUE( X7 ) )  goto L3227;
        if  ( FALSE( p2578 ) )  goto L3232;
        scrt6_display( c2644, CONS( p2578, EMPTYLIST ) );
        scrt6_display( PAIR_CAR( X3 ), 
                       CONS( p2578, EMPTYLIST ) );
        scrt6_display( c2645, CONS( p2578, EMPTYLIST ) );
        scrt6_newline( CONS( p2578, EMPTYLIST ) );
L3232:
        X9 = PAIR_CAR( l2577 );
        if  ( BITAND( BITOR( _S2CINT( X9 ), 
                             _S2CINT( _TSCP( 4 ) ) ), 
                      3 ) )  goto L3236;
        X8 = _TSCP( IDIFFERENCE( _S2CINT( X9 ), 
                                 _S2CINT( _TSCP( 4 ) ) ) );
        goto L3237;
L3236:
        X8 = scrt2__2d_2dtwo( X9, _TSCP( 4 ) );
L3237:
        SETGEN( PAIR_CAR( l2577 ), X8 );
        goto L3227;
L3226:
        if  ( FALSE( p2578 ) )  goto L3239;
        X7 = scrt4_c_2dtscp_2dref( X1, scdebug_expx_v );
        scrt6_write( X7, CONS( X6, EMPTYLIST ) );
        X7 = scrt6_get_2doutput_2dstring( X6 );
        if  ( AND( EQ( TSCPTAG( X7 ), EXTENDEDTAG ), 
                   EQ( TSCP_EXTENDEDTAG( X7 ), STRINGTAG ) ) )  goto L3244;
        scdebug_error( c2666, 
                       c2667, CONS( X7, EMPTYLIST ) );
L3244:
        X8 = C_FIXED( STRING_LENGTH( X7 ) );
        if  ( BITAND( BITOR( _S2CINT( X8 ), 
                             _S2CINT( _TSCP( 260 ) ) ), 
                      3 ) )  goto L3248;
        if  ( GT( _S2CINT( X8 ), _S2CINT( _TSCP( 260 ) ) ) )  goto L3252;
        goto L3253;
L3248:
        if  ( FALSE( scrt2__3e_2dtwo( X8, _TSCP( 260 ) ) ) )  goto L3253;
L3252:
        X9 = CONS( c2669, EMPTYLIST );
        X8 = scrt3_string_2dappend( CONS( scrt3_substring( X7, 
                                                           _TSCP( 0 ), 
                                                           _TSCP( 260 ) ), 
                                          X9 ) );
        scrt6_display( X8, CONS( p2578, EMPTYLIST ) );
        goto L3256;
L3253:
        scrt6_display( X7, CONS( p2578, EMPTYLIST ) );
L3256:
        scrt6_display( c2670, CONS( p2578, EMPTYLIST ) );
        X8 = PAIR_CAR( X5 );
        if  ( EQ( TSCPTAG( X8 ), PAIRTAG ) )  goto L3258;
        scrt1__24__car_2derror( X8 );
L3258:
        X7 = PAIR_CAR( X8 );
        scrt6_display( X7, CONS( p2578, EMPTYLIST ) );
        scrt6_newline( CONS( p2578, EMPTYLIST ) );
L3239:
        if  ( FALSE( X2 ) )  goto L3260;
        X10 = PAIR_CAR( X5 );
        if  ( EQ( TSCPTAG( X10 ), PAIRTAG ) )  goto L3263;
        scrt1__24__car_2derror( X10 );
L3263:
        X9 = PAIR_CAR( X10 );
        X8 = sc_cons( X9, PAIR_CAR( X3 ) );
        X7 = sc_cons( X8, PAIR_CAR( X4 ) );
        SETGEN( PAIR_CAR( X4 ), X7 );
        goto L3261;
L3260:
        X7 = sc_cons( PAIR_CAR( X3 ), PAIR_CAR( X4 ) );
        SETGEN( PAIR_CAR( X4 ), X7 );
L3261:
        X8 = PAIR_CAR( X5 );
        if  ( EQ( TSCPTAG( X8 ), PAIRTAG ) )  goto L3266;
        scrt1__24__cdr_2derror( X8 );
L3266:
        X7 = PAIR_CDR( X8 );
        SETGEN( PAIR_CAR( X5 ), X7 );
        X8 = PAIR_CAR( l2577 );
        if  ( BITAND( BITOR( _S2CINT( X8 ), 
                             _S2CINT( _TSCP( 4 ) ) ), 
                      3 ) )  goto L3269;
        X7 = _TSCP( IDIFFERENCE( _S2CINT( X8 ), 
                                 _S2CINT( _TSCP( 4 ) ) ) );
        goto L3270;
L3269:
        X7 = scrt2__2d_2dtwo( X8, _TSCP( 4 ) );
L3270:
        SETGEN( PAIR_CAR( l2577 ), X7 );
L3227:
        X7 = scrt4_c_2ds2cuint_2dref( X1, _TSCP( 0 ) );
        X5 = PAIR_CAR( X5 );
        X4 = PAIR_CAR( X4 );
        X3 = PAIR_CAR( X3 );
        X1 = X7;
        GOBACK( L3184 );
L3191:
        if  ( FALSE( PAIR_CAR( X4 ) ) )  goto L3271;
        if  ( FALSE( X2 ) )  goto L3273;
        X8 = scrt1_assq( c2693, PAIR_CAR( X4 ) );
        if  ( EQ( TSCPTAG( X8 ), PAIRTAG ) )  goto L3276;
        scrt1__24__cdr_2derror( X8 );
L3276:
        X7 = PAIR_CDR( X8 );
        POPSTACKTRACE( scrt1_append_2dtwo( X7, PAIR_CAR( X4 ) ) );
L3273:
        POPSTACKTRACE( scrt1_reverse( PAIR_CAR( X4 ) ) );
L3271:
        POPSTACKTRACE( PAIR_CAR( X4 ) );
}

DEFTSCP( scdebug_on_2dinterrupt_v );
DEFCSTRING( t3278, "SCDEBUG_ON-INTERRUPT" );
EXTERNTSCP( screp__2areading_2dstdin_2a_v );
EXTERNTSCP( screp_reset_v );

TSCP  scdebug_on_2dinterrupt( s2704 )
        TSCP  s2704;
{
        TSCP  X2, X1;

        PUSHSTACKTRACE( t3278 );
        if  ( FALSE( screp__2areading_2dstdin_2a_v ) )  goto L3280;
        X1 = screp_reset_v;
        X1 = UNKNOWNCALL( X1, 0 );
        VIA( PROCEDURE_CODE( X1 ) )( PROCEDURE_CLOSURE( X1 ) );
L3280:
        X1 = scrt6_le_2dtasks_e4d983f4( FALSEVALUE );
        scrt6_format( scrt5_debug_2doutput_2dport_v, 
                      CONS( c2708, EMPTYLIST ) );
        scdebug_dobacktrace( c2710, 
                             c2445, 
                             _TSCP( 80 ), 
                             scrt5_debug_2doutput_2dport_v );
        X2 = CONS( scdebug_dobacktrace( c2710, 
                                        c2445, 
                                        _TSCP( 80 ), FALSEVALUE ), 
                   EMPTYLIST );
        X2 = CONS( c2444, X2 );
        X2 = CONS( c2709, X2 );
        X2 = CONS( c2443, X2 );
        X2 = CONS( FALSEVALUE, X2 );
        screp_read_2deval_2dprint( CONS( c2441, X2 ) );
        POPSTACKTRACE( scrt6_le_2dtasks_e4d983f4( X1 ) );
}

DEFTSCP( scdebug_error_v );
DEFCSTRING( t3283, "ERROR" );
EXTERNTSCP( scdebug__2aerror_2dhandler_2a_v );
EXTERNTSCPP( sc_abort, XAL0(  ) );
EXTERNTSCP( sc_abort_v );
EXTERNTSCPP( sc_error_2ddisplay, XAL1( TSCP ) );
EXTERNTSCP( sc_error_2ddisplay_v );
EXTERNTSCPP( sc_osexit, XAL1( TSCP ) );
EXTERNTSCP( sc_osexit_v );

TSCP  scdebug_error( s2712, f2713, a2714 )
        TSCP  s2712, f2713, a2714;
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( t3283 );
        if  ( NOT( AND( EQ( TSCPTAG( scdebug__2aerror_2dhandler_2a_v ), 
                            EXTENDEDTAG ), 
                        EQ( TSCP_EXTENDEDTAG( scdebug__2aerror_2dhandler_2a_v ), 
                            PROCEDURETAG ) ) ) )  goto L3285;
        X1 = scdebug__2aerror_2dhandler_2a_v;
        scdebug__2aerror_2dhandler_2a_v = TRUEVALUE;
        X3 = sc_cons( f2713, a2714 );
        X2 = sc_cons( s2712, X3 );
        POPSTACKTRACE( sc_apply_2dtwo( X1, X2 ) );
L3285:
        X1 = sc_scheme_2dmode(  );
        if  ( NEQ( _S2CUINT( X1 ), _S2CUINT( c2464 ) ) )  goto L3288;
        if  ( FALSE( scdebug__2aerror_2dhandler_2a_v ) )  goto L3290;
        scdebug__2aerror_2dhandler_2a_v = FALSEVALUE;
        scrt6_write( c2721, 
                     CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        scrt6_newline( CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        X1 = CONS( _TSCP( 8 ), EMPTYLIST );
        POPSTACKTRACE( screp_jump_2dto_2dscheme2c( CONS( screp__2ascheme2c_2dresult_2a_v, 
                                                         X1 ) ) );
L3290:
        POPSTACKTRACE( sc_abort(  ) );
L3288:
        if  ( FALSE( scdebug__2aerror_2dhandler_2a_v ) )  goto L3293;
        scdebug__2aerror_2dhandler_2a_v = FALSEVALUE;
        sc_error_2ddisplay( c2724 );
        X2 = sc_cons( f2713, a2714 );
        X1 = sc_cons( s2712, X2 );
L3297:
        if  ( NEQ( _S2CUINT( X1 ), _S2CUINT( EMPTYLIST ) ) )  goto L3298;
        sc_error_2ddisplay( _TSCP( 2578 ) );
        POPSTACKTRACE( sc_osexit( _TSCP( 4 ) ) );
L3298:
        sc_error_2ddisplay( _TSCP( 8210 ) );
        if  ( EQ( TSCPTAG( X1 ), PAIRTAG ) )  goto L3303;
        scrt1__24__car_2derror( X1 );
L3303:
        X2 = PAIR_CAR( X1 );
        sc_error_2ddisplay( X2 );
        X1 = PAIR_CDR( X1 );
        GOBACK( L3297 );
L3293:
        sc_error_2ddisplay( c2721 );
        sc_error_2ddisplay( _TSCP( 2578 ) );
        POPSTACKTRACE( sc_osexit( _TSCP( 4 ) ) );
}

DEFTSCP( scdebug__2dhandler_f046c4d9_v );
DEFCSTRING( t3307, "SCDEBUG_EMBEDDED-ERROR-HANDLER" );
EXTERNTSCP( scdebug__2aerror_2denv_2a_v );
EXTERNTSCPP( scdebug__2dhandler_f046c4d9, 
             XAL3( TSCP, TSCP, TSCP ) );
EXTERNTSCP( scdebug__2dhandler_f046c4d9_v );

TSCP  scdebug__2dhandler_f046c4d9( i2749, f2750, a2751 )
        TSCP  i2749, f2750, a2751;
{
        TSCP  X2, X1;

        PUSHSTACKTRACE( t3307 );
        X1 = scrt6_format( c2752, CONS( i2749, EMPTYLIST ) );
        scrt6_display( X1, 
                       CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        X2 = sc_cons( f2750, a2751 );
        X1 = sc_apply_2dtwo( scrt6_format_v, X2 );
        scrt6_display( X1, 
                       CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        scrt6_newline( CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        if  ( NEQ( _S2CUINT( scdebug__2aerror_2denv_2a_v ), 
                   _S2CUINT( FALSEVALUE ) ) )  goto L3309;
        scdebug__2aerror_2denv_2a_v = scdebug_dobacktrace( c2757, 
                                                           c2424, 
                                                           _TSCP( 80 ), 
                                                           scrt5_stderr_2dport_v );
L3309:
        scdebug__2aerror_2dhandler_2a_v = scdebug__2dhandler_f046c4d9_v;
        X1 = CONS( _TSCP( 4 ), EMPTYLIST );
        POPSTACKTRACE( screp_jump_2dto_2dscheme2c( CONS( screp__2ascheme2c_2dresult_2a_v, 
                                                         X1 ) ) );
}

DEFTSCP( scdebug_reset_2derror_v );
DEFCSTRING( t3311, "RESET-ERROR" );

TSCP  scdebug_reset_2derror(  )
{
        PUSHSTACKTRACE( t3311 );
        POPSTACKTRACE( SET( scdebug__2aerror_2denv_2a_v, FALSEVALUE ) );
}

DEFTSCP( scdebug__2aerror_2denv_2a_v );
DEFCSTRING( t3313, "*ERROR-ENV*" );
DEFTSCP( scdebug__2dhandler_eddc0242_v );
DEFCSTRING( t3314, "SCDEBUG_STAND-ALONE-ERROR-HANDLER" );

TSCP  scdebug__2dhandler_eddc0242( i2762, f2763, a2764 )
        TSCP  i2762, f2763, a2764;
{
        TSCP  X2, X1;

        PUSHSTACKTRACE( t3314 );
        X1 = scrt6_format( c2752, CONS( i2762, EMPTYLIST ) );
        scrt6_display( X1, 
                       CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        X2 = sc_cons( f2763, a2764 );
        X1 = sc_apply_2dtwo( scrt6_format_v, X2 );
        scrt6_display( X1, 
                       CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        scrt6_newline( CONS( scrt5_stderr_2dport_v, EMPTYLIST ) );
        scdebug_dobacktrace( c2757, 
                             c2765, 
                             _TSCP( 80 ), scrt5_stderr_2dport_v );
        POPSTACKTRACE( sc_osexit( _TSCP( 4 ) ) );
}

DEFTSCP( scdebug__2aerror_2dhandler_2a_v );
DEFCSTRING( t3316, "*ERROR-HANDLER*" );
DEFTSCP( scdebug__2dhandler_3675c87d_v );
DEFCSTRING( t3319, "RESET-ERROR-HANDLER" );

TSCP  scdebug__2dhandler_3675c87d(  )
{
        TSCP  X2, X1;

        PUSHSTACKTRACE( t3319 );
        X2 = sc_scheme_2dmode(  );
        if  ( NEQ( _S2CUINT( X2 ), _S2CUINT( c2464 ) ) )  goto L3321;
        X1 = scdebug__2dhandler_f046c4d9_v;
        goto L3322;
L3321:
        X1 = scdebug__2dhandler_eddc0242_v;
L3322:
        POPSTACKTRACE( SET( scdebug__2aerror_2dhandler_2a_v, X1 ) );
}

DEFTSCP( scdebug_2derror_2a_ca4047fd_v );
DEFCSTRING( t3323, "*DEBUG-ON-ERROR*" );
DEFTSCP( scdebug__2dhandler_7d8722d5_v );
DEFCSTRING( t3324, "SCDEBUG_BACKTRACE-ERROR-HANDLER" );
EXTERNTSCPP( scdebug__2dhandler_7d8722d5, 
             XAL3( TSCP, TSCP, TSCP ) );
EXTERNTSCP( scdebug__2dhandler_7d8722d5_v );
EXTERNTSCPP( scrt6_char_2dready_3f, XAL1( TSCP ) );
EXTERNTSCP( scrt6_char_2dready_3f_v );
EXTERNTSCP( scrt5_stdin_2dport_v );
EXTERNTSCPP( scrt6_eof_2dobject_3f, XAL1( TSCP ) );
EXTERNTSCP( scrt6_eof_2dobject_3f_v );
EXTERNTSCPP( scrt6_read_2dchar, XAL1( TSCP ) );
EXTERNTSCP( scrt6_read_2dchar_v );

TSCP  scdebug__2dhandler_7d8722d5( i2781, f2782, a2783 )
        TSCP  i2781, f2782, a2783;
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( t3324 );
        X1 = scrt6_format( c2752, CONS( i2781, EMPTYLIST ) );
        scrt6_display( X1, 
                       CONS( scrt5_debug_2doutput_2dport_v, EMPTYLIST ) );
        X2 = sc_cons( f2782, a2783 );
        X1 = sc_apply_2dtwo( scrt6_format_v, X2 );
        scrt6_display( X1, 
                       CONS( scrt5_debug_2doutput_2dport_v, EMPTYLIST ) );
        scrt6_newline( CONS( scrt5_debug_2doutput_2dport_v, EMPTYLIST ) );
        scdebug__2aerror_2dhandler_2a_v = scdebug__2dhandler_7d8722d5_v;
        if  ( FALSE( scdebug_2derror_2a_ca4047fd_v ) )  goto L3326;
        X1 = scrt6_le_2dtasks_e4d983f4( FALSEVALUE );
        X2 = scdebug_dobacktrace( c2757, 
                                  c2445, 
                                  _TSCP( 80 ), 
                                  scrt5_debug_2doutput_2dport_v );
        scdebug_2derror_2a_ca4047fd_v = FALSEVALUE;
L3329:
        if  ( FALSE( scrt6_char_2dready_3f( CONS( scrt5_stdin_2dport_v, 
                                                  EMPTYLIST ) ) )
            )  goto L3333;
        X3 = scrt6_read_2dchar( CONS( scrt5_stdin_2dport_v, EMPTYLIST ) );
        if  ( FALSE( scrt6_eof_2dobject_3f( X3 ) ) )  GOBACK( L3329 );
L3333:
        X3 = CONS( X2, EMPTYLIST );
        X3 = CONS( c2444, X3 );
        X3 = CONS( FALSEVALUE, X3 );
        X3 = CONS( c2441, X3 );
        X3 = CONS( c2709, X3 );
        screp_read_2deval_2dprint( CONS( c2443, X3 ) );
        scrt6_le_2dtasks_e4d983f4( X1 );
        scdebug_2derror_2a_ca4047fd_v = TRUEVALUE;
L3326:
        X1 = screp_reset_v;
        X1 = UNKNOWNCALL( X1, 0 );
        POPSTACKTRACE( VIA( PROCEDURE_CODE( X1 ) )( PROCEDURE_CLOSURE( X1 ) ) );
}

DEFTSCP( scdebug_timeout_v );
DEFCSTRING( t3335, "SCDEBUG_TIMEOUT" );
EXTERNTSCP( scdebug_timeout_2ddebug_v );

TSCP  scdebug_l2797( c3339 )
        TSCP  c3339;
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( "scdebug_l2797 [inside TIMEOUT]" );
        X1 = DISPLAY( 0 );
        DISPLAY( 0 ) = CLOSURE_VAR( c3339, 0 );
        X3 = DISPLAY( 0 );
        X3 = UNKNOWNCALL( X3, 1 );
        X2 = VIA( PROCEDURE_CODE( X3 ) )( TRUEVALUE, 
                                          PROCEDURE_CLOSURE( X3 ) );
        DISPLAY( 0 ) = X1;
        POPSTACKTRACE( X2 );
}

TSCP  scdebug_l2795( r2796, c3337 )
        TSCP  r2796, c3337;
{
        TSCP  X1;
        TSCP  SD0 = DISPLAY( 0 );
        TSCP  SDVAL;

        PUSHSTACKTRACE( "scdebug_l2795 [inside TIMEOUT]" );
        DISPLAY( 0 ) = r2796;
        scdebug_proceed_v = MAKEPROCEDURE( 0, 
                                           0, 
                                           scdebug_l2797, 
                                           MAKECLOSURE( EMPTYLIST, 
                                                        1, 
                                                        DISPLAY( 0 ) ) );
        X1 = CONS( _TSCP( 12 ), EMPTYLIST );
        SDVAL = screp_jump_2dto_2dscheme2c( CONS( screp__2ascheme2c_2dresult_2a_v, 
                                                  X1 ) );
        DISPLAY( 0 ) = SD0;
        POPSTACKTRACE( SDVAL );
}

TSCP  scdebug_l2802( c3346 )
        TSCP  c3346;
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( "scdebug_l2802 [inside TIMEOUT]" );
        X1 = DISPLAY( 0 );
        DISPLAY( 0 ) = CLOSURE_VAR( c3346, 0 );
        X3 = DISPLAY( 0 );
        X3 = UNKNOWNCALL( X3, 1 );
        X2 = VIA( PROCEDURE_CODE( X3 ) )( TRUEVALUE, 
                                          PROCEDURE_CLOSURE( X3 ) );
        DISPLAY( 0 ) = X1;
        POPSTACKTRACE( X2 );
}

TSCP  scdebug_l2800( c2801, c3344 )
        TSCP  c2801, c3344;
{
        TSCP  X1;
        TSCP  SD0 = DISPLAY( 0 );
        TSCP  SDVAL;

        PUSHSTACKTRACE( "scdebug_l2800 [inside TIMEOUT]" );
        DISPLAY( 0 ) = c2801;
        scdebug_proceed_v = MAKEPROCEDURE( 0, 
                                           0, 
                                           scdebug_l2802, 
                                           MAKECLOSURE( EMPTYLIST, 
                                                        1, 
                                                        DISPLAY( 0 ) ) );
        X1 = CONS( _TSCP( 16 ), EMPTYLIST );
        SDVAL = screp_jump_2dto_2dscheme2c( CONS( screp__2ascheme2c_2dresult_2a_v, 
                                                  X1 ) );
        DISPLAY( 0 ) = SD0;
        POPSTACKTRACE( SDVAL );
}

TSCP  scdebug_timeout(  )
{
        TSCP  X3, X2, X1;

        PUSHSTACKTRACE( t3335 );
        scdebug_timeout_2ddebug_v = FALSEVALUE;
        X2 = MAKEPROCEDURE( 1, 0, scdebug_l2795, EMPTYLIST );
        X1 = sc_ntinuation_1af38b9f_v;
        X1 = UNKNOWNCALL( X1, 1 );
        VIA( PROCEDURE_CODE( X1 ) )( X2, PROCEDURE_CLOSURE( X1 ) );
        if  ( FALSE( scdebug_timeout_2ddebug_v ) )  goto L3341;
        X1 = scdebug_dobacktrace( c2803, 
                                  c2424, 
                                  _TSCP( 80 ), scrt5_stderr_2dport_v );
        scdebug__2aargs_2a_v = EMPTYLIST;
        scdebug__2abpt_2denv_2a_v = X1;
        X3 = MAKEPROCEDURE( 1, 0, scdebug_l2800, EMPTYLIST );
        X2 = sc_ntinuation_1af38b9f_v;
        X2 = UNKNOWNCALL( X2, 1 );
        VIA( PROCEDURE_CODE( X2 ) )( X3, PROCEDURE_CLOSURE( X2 ) );
        POPSTACKTRACE( SET( scdebug__2abpt_2denv_2a_v, FALSEVALUE ) );
L3341:
        POPSTACKTRACE( FALSEVALUE );
}

DEFTSCP( scdebug_proceed_3f_v );
DEFCSTRING( t3348, "PROCEED?" );

TSCP  scdebug_proceed_3f(  )
{
        TSCP  X1;

        PUSHSTACKTRACE( t3348 );
        scdebug_timeout_2ddebug_v = TRUEVALUE;
        X1 = scdebug_proceed_v;
        X1 = UNKNOWNCALL( X1, 0 );
        POPSTACKTRACE( VIA( PROCEDURE_CODE( X1 ) )( PROCEDURE_CLOSURE( X1 ) ) );
}

DEFTSCP( scdebug_timeout_2ddebug_v );
DEFCSTRING( t3350, "SCDEBUG_TIMEOUT-DEBUG" );
void scrt3__init();
void scrt4__init();
void screp__init();
void sceval__init();
void scrt5__init();
void scrt6__init();
void scrt2__init();
void scrt1__init();
void scexpand__init();

static void  init_modules( compiler_version )
        char *compiler_version;
{
        scrt3__init();
        scrt4__init();
        screp__init();
        sceval__init();
        scrt5__init();
        scrt6__init();
        scrt2__init();
        scrt1__init();
        scexpand__init();
        MAXDISPLAY( 4 );
}

void  scdebug__init()
{
        TSCP  X2, X1;

        static int  init = 0;
        if  (init)  return;
        init = 1;
        INITHEAP( 0, 0, 0, 0 );
        init_constants();
        init_modules( "(scdebug SCHEME->C COMPILER 15mar93jfb)" );
        sc_segv__handlers();
        INITIALIZEVAR( t2919, 
                       ADR( scdebug_trace_2dlevel_v ), 
                       _TSCP( 0 ) );
        INITIALIZEVAR( t2920, 
                       ADR( scdebug_traced_2dprocs_v ), 
                       EMPTYLIST );
        INITIALIZEVAR( t2921, 
                       ADR( scdebug_bpt_2dprocs_v ), EMPTYLIST );
        INITIALIZEVAR( t2922, 
                       ADR( scdebug__2aargs_2a_v ), EMPTYLIST );
        INITIALIZEVAR( t2923, 
                       ADR( scdebug__2aresult_2a_v ), EMPTYLIST );
        INITIALIZEVAR( t2924, 
                       ADR( scdebug__2abpt_2denv_2a_v ), 
                       FALSEVALUE );
        X1 = MAKEPROCEDURE( 2, 0, scdebug_l2136, EMPTYLIST );
        scexpand_install_2dexpander( c2135, X1 );
        INITIALIZEVAR( t2932, 
                       ADR( scdebug_dotrace_v ), 
                       MAKEPROCEDURE( 1, 
                                      0, 
                                      scdebug_dotrace, EMPTYLIST ) );
        INITIALIZEVAR( t2944, 
                       ADR( scdebug_tracer_v ), 
                       MAKEPROCEDURE( 2, 
                                      0, scdebug_tracer, EMPTYLIST ) );
        X1 = MAKEPROCEDURE( 2, 0, scdebug_l2219, EMPTYLIST );
        scexpand_install_2dexpander( c2218, X1 );
        INITIALIZEVAR( t3000, 
                       ADR( scdebug_dountrace_v ), 
                       MAKEPROCEDURE( 1, 
                                      0, 
                                      scdebug_dountrace, EMPTYLIST ) );
        X1 = MAKEPROCEDURE( 2, 0, scdebug_l2306, EMPTYLIST );
        scexpand_install_2dexpander( c2305, X1 );
        INITIALIZEVAR( t3031, 
                       ADR( scdebug_dobpt_v ), 
                       MAKEPROCEDURE( 1, 
                                      1, scdebug_dobpt, EMPTYLIST ) );
        INITIALIZEVAR( t3044, 
                       ADR( scdebug_reset_2dbpt_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, 
                                      scdebug_reset_2dbpt, EMPTYLIST ) );
        INITIALIZEVAR( t3046, 
                       ADR( scdebug_default_2dproceed_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, 
                                      scdebug_default_2dproceed, 
                                      EMPTYLIST ) );
        INITIALIZEVAR( t3048, 
                       ADR( scdebug_proceed_v ), 
                       scdebug_default_2dproceed_v );
        INITIALIZEVAR( t3049, 
                       ADR( scdebug_bpter_2dprocname_v ), c2365 );
        INITIALIZEVAR( t3050, 
                       ADR( scdebug_bpter_v ), 
                       MAKEPROCEDURE( 3, 
                                      0, scdebug_bpter, EMPTYLIST ) );
        INITIALIZEVAR( t3120, 
                       ADR( scdebug_backtrace_v ), FALSEVALUE );
        INITIALIZEVAR( t3121, 
                       ADR( scdebug_dbacktrace_a8071371_v ), 
                       MAKEPROCEDURE( 0, 
                                      1, 
                                      scdebug_dbacktrace_a8071371, 
                                      EMPTYLIST ) );
        X1 = MAKEPROCEDURE( 2, 0, scdebug_l2472, EMPTYLIST );
        scexpand_install_2dexpander( c2471, X1 );
        INITIALIZEVAR( t3157, 
                       ADR( scdebug_dounbpt_v ), 
                       MAKEPROCEDURE( 1, 
                                      0, 
                                      scdebug_dounbpt, EMPTYLIST ) );
        X1 = C_FIXED( SIZEOF( TSCP ) );
        if  ( BITAND( BITOR( _S2CINT( X1 ), 
                             _S2CINT( _TSCP( 4 ) ) ), 
                      3 ) )  goto L3174;
        X2 = _TSCP( ITIMES( FIXED_C( X1 ), 
                            _S2CINT( _TSCP( 4 ) ) ) );
        goto L3175;
L3174:
        X2 = scrt2__2a_2dtwo( X1, _TSCP( 4 ) );
L3175:
        INITIALIZEVAR( t3172, ADR( scdebug_procnamex_v ), X2 );
        X1 = C_FIXED( SIZEOF( TSCP ) );
        if  ( BITAND( BITOR( _S2CINT( X1 ), 
                             _S2CINT( _TSCP( 8 ) ) ), 
                      3 ) )  goto L3178;
        X2 = _TSCP( ITIMES( FIXED_C( X1 ), 
                            _S2CINT( _TSCP( 8 ) ) ) );
        goto L3179;
L3178:
        X2 = scrt2__2a_2dtwo( X1, _TSCP( 8 ) );
L3179:
        INITIALIZEVAR( t3176, ADR( scdebug_expx_v ), X2 );
        INITIALIZEVAR( t3180, 
                       ADR( scdebug_dobacktrace_v ), 
                       MAKEPROCEDURE( 4, 
                                      0, 
                                      scdebug_dobacktrace, EMPTYLIST ) );
        INITIALIZEVAR( t3278, 
                       ADR( scdebug_on_2dinterrupt_v ), 
                       MAKEPROCEDURE( 1, 
                                      0, 
                                      scdebug_on_2dinterrupt, 
                                      EMPTYLIST ) );
        INITIALIZEVAR( t3283, 
                       ADR( scdebug_error_v ), 
                       MAKEPROCEDURE( 2, 
                                      1, scdebug_error, EMPTYLIST ) );
        INITIALIZEVAR( t3307, 
                       ADR( scdebug__2dhandler_f046c4d9_v ), 
                       MAKEPROCEDURE( 2, 
                                      1, 
                                      scdebug__2dhandler_f046c4d9, 
                                      EMPTYLIST ) );
        INITIALIZEVAR( t3311, 
                       ADR( scdebug_reset_2derror_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, 
                                      scdebug_reset_2derror, EMPTYLIST ) );
        INITIALIZEVAR( t3313, 
                       ADR( scdebug__2aerror_2denv_2a_v ), 
                       FALSEVALUE );
        INITIALIZEVAR( t3314, 
                       ADR( scdebug__2dhandler_eddc0242_v ), 
                       MAKEPROCEDURE( 2, 
                                      1, 
                                      scdebug__2dhandler_eddc0242, 
                                      EMPTYLIST ) );
        X2 = sc_scheme_2dmode(  );
        if  ( NEQ( _S2CUINT( X2 ), _S2CUINT( c2464 ) ) )  goto L3317;
        X1 = scdebug__2dhandler_f046c4d9_v;
        goto L3318;
L3317:
        X1 = scdebug__2dhandler_eddc0242_v;
L3318:
        INITIALIZEVAR( t3316, 
                       ADR( scdebug__2aerror_2dhandler_2a_v ), 
                       X1 );
        INITIALIZEVAR( t3319, 
                       ADR( scdebug__2dhandler_3675c87d_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, 
                                      scdebug__2dhandler_3675c87d, 
                                      EMPTYLIST ) );
        INITIALIZEVAR( t3323, 
                       ADR( scdebug_2derror_2a_ca4047fd_v ), 
                       FALSEVALUE );
        INITIALIZEVAR( t3324, 
                       ADR( scdebug__2dhandler_7d8722d5_v ), 
                       MAKEPROCEDURE( 2, 
                                      1, 
                                      scdebug__2dhandler_7d8722d5, 
                                      EMPTYLIST ) );
        INITIALIZEVAR( t3335, 
                       ADR( scdebug_timeout_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, 
                                      scdebug_timeout, EMPTYLIST ) );
        INITIALIZEVAR( t3348, 
                       ADR( scdebug_proceed_3f_v ), 
                       MAKEPROCEDURE( 0, 
                                      0, 
                                      scdebug_proceed_3f, EMPTYLIST ) );
        INITIALIZEVAR( t3350, 
                       ADR( scdebug_timeout_2ddebug_v ), 
                       TRUEVALUE );
        return;
}
