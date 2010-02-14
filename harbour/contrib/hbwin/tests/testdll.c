/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    DLL call test.
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 *
 */

/* Build with:
      hbmk2 -hbdyn testdll.c -otest_x86
      hbmk2 -hbdyn testdll.c -otest_x64
 */

#include "hbapi.h"

HB_EXPORT double         TESTD ( double         value ) { printf( "DLL: %lf\n"      , value ); return value; }
HB_EXPORT float          TESTF ( float          value ) { printf( "DLL: %f\n"       , value ); return value; }
HB_EXPORT char           TESTC ( char           value ) { printf( "DLL: %d\n"       , value ); return value; }
HB_EXPORT unsigned char  TESTUC( unsigned char  value ) { printf( "DLL: %d\n"       , value ); return value; }
HB_EXPORT short          TESTS ( short          value ) { printf( "DLL: %hd\n"      , value ); return value; }
HB_EXPORT unsigned short TESTUS( unsigned short value ) { printf( "DLL: %hu\n"      , value ); return value; }
HB_EXPORT int            TESTI ( int            value ) { printf( "DLL: %d\n"       , value ); return value; }
HB_EXPORT unsigned int   TESTUI( unsigned int   value ) { printf( "DLL: %u\n"       , value ); return value; }
HB_EXPORT long           TESTL ( long           value ) { printf( "DLL: %ld\n"      , value ); return value; }
HB_EXPORT unsigned long  TESTUL( unsigned long  value ) { printf( "DLL: %lu\n"      , value ); return value; }
HB_EXPORT HB_LONGLONG    TEST6 ( HB_LONGLONG    value ) { printf( "DLL: %"PFLL"d\n" , value ); return value; }
HB_EXPORT HB_ULONGLONG   TESTU6( HB_ULONGLONG   value ) { printf( "DLL: %"PFLL"u\n" , value ); return value; }
HB_EXPORT char *         TESTST( char *         value ) { printf( "DLL: %s\n"       , value ); return value; }
