/* Copyright 2010 Viktor Szakats (vszakats.net/harbour) */

/* Dynamic library call test. */

/* Build with:
      hbmk2 -hbdyn dyn.c -otest_32
      hbmk2 -hbdyn dyn.c -otest_64
 */

#include "hbapi.h"

HB_EXPORT double         TESTD ( double         value ) { printf( "DYN: %lf\n"      , value ); return value; }
HB_EXPORT float          TESTF ( float          value ) { printf( "DYN: %f\n"       , value ); return value; }
HB_EXPORT char           TESTC ( char           value ) { printf( "DYN: %d\n"       , value ); return value; }
HB_EXPORT unsigned char  TESTUC( unsigned char  value ) { printf( "DYN: %d\n"       , value ); return value; }
HB_EXPORT short          TESTS ( short          value ) { printf( "DYN: %hd\n"      , value ); return value; }
HB_EXPORT unsigned short TESTUS( unsigned short value ) { printf( "DYN: %hu\n"      , value ); return value; }
HB_EXPORT int            TESTI ( int            value ) { printf( "DYN: %d\n"       , value ); return value; }
HB_EXPORT unsigned int   TESTUI( unsigned int   value ) { printf( "DYN: %u\n"       , value ); return value; }
HB_EXPORT long           TESTL ( long           value ) { printf( "DYN: %ld\n"      , value ); return value; }
HB_EXPORT unsigned long  TESTUL( unsigned long  value ) { printf( "DYN: %lu\n"      , value ); return value; }
HB_EXPORT HB_LONGLONG    TEST6 ( HB_LONGLONG    value ) { printf( "DYN: %"PFLL"d\n" , value ); return value; }
HB_EXPORT HB_ULONGLONG   TESTU6( HB_ULONGLONG   value ) { printf( "DYN: %"PFLL"u\n" , value ); return value; }
HB_EXPORT char *         TESTST( char *         value ) { printf( "DYN: %s\n"       , value ); return value; }
