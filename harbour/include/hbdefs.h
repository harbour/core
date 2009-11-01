/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for compiler and runtime basic type declarations
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef HB_DEFS_H_
#define HB_DEFS_H_

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "hbsetup.h"
#include "hbver.h"

/* Compatibility. Do not use HB_OS_WIN_32_USED anymore. */
#ifdef HB_LEGACY_LEVEL2
   #if defined( HB_OS_WIN_32_USED ) && ! defined( HB_OS_WIN_USED )
      #define HB_OS_WIN_USED
   #endif
#endif

#if defined( __XCC__ ) || defined( __POCC__ ) || defined( __LCC__ ) || \
    defined( __MINGW32__ ) || defined( __DMC__ ) || \
    ( defined( __BORLANDC__ ) && __BORLANDC__ >= 1410 ) || \
    ( defined( __WATCOMC__ ) && __WATCOMC__ >= 1270 ) || \
    ( ( defined( __GNUC__ ) || defined( __SUNPRO_C ) || defined( __SUNPRO_CC ) ) && \
      ( defined( _ISOC99_SOURCE ) || \
        ( defined( __STDC_VERSION__ ) && __STDC_VERSION__ >= 199901L ) || \
        ( defined( __DJGPP__ ) && \
          ( __DJGPP__ > 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ >= 4 ) ) ) || \
        defined( HB_OS_LINUX ) || defined( HB_OS_DARWIN ) || \
        defined( HB_OS_BSD ) || defined( HB_OS_SUNOS ) || \
        defined( HB_OS_BEOS ) ) )
   #include <stdint.h>
   /* workaround for BCC 5.8 bug */
   #if ( defined( __BORLANDC__ ) && __BORLANDC__ >= 1410 )
      #undef INT32_MIN
      #define INT32_MIN ((int32_t) (-INT32_MAX-1))
      #undef INT64_MIN
      #define INT64_MIN (9223372036854775807i64-1)
      #undef INT64_MAX
      #define INT64_MAX 9223372036854775807i64
   #endif
#endif

/*
#define HB_CLIPPER_INT_ITEMS
#define HB_LONG_LONG_OFF
*/

#if defined( HB_OS_WIN ) || defined( HB_OS_WIN_64 )
   #if defined( HB_OS_WIN_64 )
      #undef HB_LONG_LONG_OFF
      #define HB_STRICT_ALIGNMENT
      #if !defined( HB_OS_WIN )
         #define HB_OS_WIN
      #endif
   #endif

   #if !defined( HB_IO_WIN_OFF )
      #define HB_IO_WIN
   #endif
   #if defined( HB_IO_WIN ) && !defined( HB_OS_WIN_USED )
      /* disabled to avoid problems with windows.h */
      /* #define HB_OS_WIN_USED */
   #endif
#else
   #undef HB_IO_WIN
   #undef HB_OS_WIN_USED
#endif

/* Include windows.h if applicable and requested */
#if defined( HB_OS_WIN_USED ) && defined( HB_OS_WIN )

   #include <windows.h>
   #if defined( __GNUC__ )
      #define HB_DONT_DEFINE_BASIC_TYPES
   #endif

#elif defined( HB_OS_OS2 )

   /* With the exception of WORD, the IBM Visual Age C++ compiler has
      its own definitions of the Harbour types most of which conflict with the
      Harbour #undefs, due to typedef being the prevalent method of
      defining the types in IBMCPP, whereas Harbour assumes that the
      definitions that it is replacing have been defined using
      #define. Therefore, it is necessary to skip the Harbour
      definition section when using the IBMCPP compiiler, include
      the IBMCPP type definitions, and then add the definition for WORD

      NOTE: This only applies to the common types that most C compilers
            define. Any new types, particulary those that start with
            HB_, must be placed AFTER the #endif __IBMCPP__ line!
   */
   /* 28/03/2000 - maurilio.longo@libero.it
      The same holds true when using GCC under OS/2
   */
   #define INCL_TYPES
   #define INCL_DOSEXCEPTIONS    /* DOS exception values */
   #define INCL_ERRORS           /* DOS error values     */
   #define INCL_LONGLONG         /* include native compiler LONGLONG definition */

   #include <os2.h>
   #undef INT
   #undef UINT
   #define HB_DONT_DEFINE_BASIC_TYPES

#elif defined( HB_OS_DOS )

   #include <dos.h>

   #if defined( __WATCOMC__ ) && defined( __386__ ) && !defined( __WINDOWS_386__ )
      #define HB_DOS_INT86 int386
      #define HB_DOS_INT86X int386x
      #define HB_XREGS w
   #elif defined( __RSX32__ )
      #define HB_DOS_INT86 _int86
      #define HB_DOS_INT86X _int86x
      #define HB_XREGS x
   #elif defined( __DJGPP__ )
      #define HB_DOS_INT86 int86
      #define HB_DOS_INT86X int86x
      #define HB_XREGS w
   #else
      #define HB_DOS_INT86 int86
      #define HB_DOS_INT86X int86x
      #define HB_XREGS x
   #endif

#elif defined( HB_OS_DARWIN )

   /* Detect if it is Darwin < 6.x */
   #include <pthread.h>
   #ifndef PTHREAD_MUTEX_RECURSIVE
      #define HB_OS_DARWIN_5
   #endif

#endif

#if ! defined( HB_DONT_DEFINE_BASIC_TYPES )

   #if ! defined( HB_DONT_DEFINE_BOOL )
      #undef BOOL                         /* boolean */
      typedef int BOOL;
   #endif

   #undef UINT                            /* varies with platform */
   typedef unsigned int UINT;

   #undef SCHAR                           /* 1 byte signed */
   typedef signed char SCHAR;

   #undef UCHAR                           /* 1 byte unsigned */
   typedef unsigned char UCHAR;

   #if ! defined( HB_DONT_DEFINE_BYTE )
      #undef BYTE                            /* 1 byte unsigned */
      typedef unsigned char BYTE;
   #endif

   #undef SHORT                           /* 2 bytes signed */
   typedef signed short int SHORT;

   #undef USHORT                          /* 2 bytes unsigned */
   typedef unsigned short int USHORT;

   #if ! defined( HB_DONT_DEFINE_LONG )
      #undef LONG                         /* 4 or 8 bytes signed */
      typedef long LONG;
   #endif

   #undef ULONG                           /* 4 or 8 bytes unsigned */
   typedef unsigned long ULONG;

   #undef FALSE
   #define FALSE  0
   #undef TRUE
   #define TRUE   (!0)

#else  /* HB_DONT_DEFINE_BASIC_TYPES */

   /*
    * if HB_DONT_DEFINE_BASIC_TYPES excluded some types which are not
    * defined in included platform dependent header files then please
    * add necessary definitions here.
    */

   /* SCHAR is needed using GCC on OS/2 */
   #if ! defined( SCHAR )
      typedef signed char SCHAR;          /* 1 byte signed */
   #endif

#endif /* HB_DONT_DEFINE_BASIC_TYPES */

#ifndef HB_LONG_LONG_OFF

   #if ! defined( HB_DONT_DEFINE_BASIC_TYPES ) && ! defined( _WINNT_H )
      #if !defined( LONGLONG )
         #if defined( __GNUC__ ) || defined( __SUNPRO_C ) || defined( __SUNPRO_CC )
            typedef signed long long LONGLONG;
         #else
            typedef __int64 LONGLONG;
         #endif
      #endif
      #if !defined( ULONGLONG )
         #if defined( __GNUC__ ) || defined( __SUNPRO_C ) || defined( __SUNPRO_CC )
            typedef unsigned long long ULONGLONG;
         #else
            typedef unsigned __int64 ULONGLONG;
         #endif
      #endif
   #endif

   #if !defined( ULONGLONG_MAX )
      #if defined( _UI64_MAX )
         #define ULONGLONG_MAX      _UI64_MAX
      #elif defined( ULLONG_MAX )
         #define ULONGLONG_MAX      ULLONG_MAX
      #elif defined( ULONG_LONG_MAX )
         #define ULONGLONG_MAX      ULONG_LONG_MAX
      #else
         #define ULONGLONG_MAX      18446744073709551615ULL
      #endif
   #endif
   #if !defined( LONGLONG_MAX )
      #if defined( _I64_MAX )
         #define LONGLONG_MAX       _I64_MAX
      #elif defined( LLONG_MAX )
         #define LONGLONG_MAX       LLONG_MAX
      #elif defined( LONG_LONG_MAX )
         #define LONGLONG_MAX       LONG_LONG_MAX
      #else
         #define LONGLONG_MAX       9223372036854775807LL
      #endif
   #endif
   #if !defined( LONGLONG_MIN )
      #if defined( _I64_MIN )
         #define LONGLONG_MIN       _I64_MIN
      #elif defined( LLONG_MIN )
         #define LONGLONG_MIN       LLONG_MIN
      #elif defined( LONG_LONG_MIN )
         #define LONGLONG_MIN       LONG_LONG_MIN
      #else
         #define LONGLONG_MIN       (-LONGLONG_MAX - 1LL)
      #endif
   #endif

#endif /* HB_LONG_LONG_OFF */

/*
 * below are some hacks which don't have to be true on some machines
 * please update it if necessary
 */
#if defined( HB_OS_WIN_64 )
#  define HB_ARCH_64BIT
#elif ULONG_MAX > UINT_MAX && UINT_MAX > USHRT_MAX
#  define HB_ARCH_64BIT
#elif ULONG_MAX == UINT_MAX && UINT_MAX > USHRT_MAX
#  define HB_ARCH_32BIT
#elif ULONG_MAX > UINT_MAX && UINT_MAX == USHRT_MAX
#  define HB_ARCH_16BIT
#endif

#if USHRT_MAX == 0xFFFF
   typedef signed short int    HB_I16;
   typedef unsigned short int  HB_U16;
   #define HB_I16_MIN          SHRT_MIN
   #define HB_I16_MAX          SHRT_MAX
   #define HB_U16_MAX          USHRT_MAX
   typedef signed short int    hbI16;
   typedef unsigned short int  hbU16;
   #define hbI16Min            SHRT_MIN
   #define hbI16Max            SHRT_MAX
   #define hbU16Max            USHRT_MAX
#  if !defined( UINT16 )
      typedef USHORT       UINT16;
#  endif
#  if !defined( INT16 )
      typedef SHORT        INT16;
#  endif
#  if !defined( UINT16_MAX )
#     define UINT16_MAX    USHRT_MAX
#  endif
#  if !defined( INT16_MAX )
#     define INT16_MAX     SHRT_MAX
#  endif
#  if !defined( INT16_MIN )
#     define INT16_MIN     SHRT_MIN
#  endif
#else
   typedef short int           HB_I16;
   typedef unsigned short int  HB_U16;
   #define HB_I16_MIN          SHRT_MIN
   #define HB_I16_MAX          SHRT_MAX
   #define HB_U16_MAX          USHRT_MAX
   typedef short int           hbI16;
   typedef unsigned short int  hbU16;
   #define hbI16Min            SHRT_MIN
   #define hbI16Max            SHRT_MAX
   #define hbU16Max            USHRT_MAX
#endif

#if UINT_MAX == 0xFFFFFFFF
   typedef signed int          HB_I32;
   typedef unsigned int        HB_U32;
   #define HB_I32_MIN          INT_MIN
   #define HB_I32_MAX          INT_MAX
   #define HB_U32_MAX          UINT_MAX
   typedef signed int          hbI32;
   typedef unsigned int        hbU32;
   #define hbI32Min            INT_MIN
   #define hbI32Max            INT_MAX
   #define hbU32Max            UINT_MAX
#  if !defined( UINT32 )
      typedef UINT         UINT32;
#  endif
#  if !defined( INT32 )
      typedef signed int   INT32;
#  endif
#  if !defined( UINT32_MAX )
#     define UINT32_MAX    UINT_MAX
#  endif
#  if !defined( INT32_MAX )
#     define INT32_MAX     INT_MAX
#  endif
#  if !defined( INT32_MIN )
#     define INT32_MIN     INT_MIN
#  endif
#elif ULONG_MAX == 0xFFFFFFFF
   typedef signed long         HB_I32;
   typedef unsigned long       HB_U32;
   #define HB_I32_MIN          LONG_MIN
   #define HB_I32_MAX          LONG_MAX
   #define HB_U32_MAX          ULONG_MAX
   typedef signed long         hbI32;
   typedef unsigned long       hbU32;
   #define hbI32Min            LONG_MIN
   #define hbI32Max            LONG_MAX
   #define hbU32Max            ULONG_MAX
#  if !defined( UINT32 )
      typedef ULONG        UINT32;
#  endif
#  if !defined( INT32 )
      typedef LONG         INT32;
#  endif
#  if !defined( UINT32_MAX )
#     define UINT32_MAX    ULONG_MAX
#  endif
#  if !defined( INT32_MAX )
#     define INT32_MAX     LONG_MAX
#  endif
#  if !defined( INT32_MIN )
#     define INT32_MIN     LONG_MIN
#  endif
#endif

#if !defined( UCHAR_MAX )
#  define UCHAR_MAX     0x0FF
#endif
#if !defined( UINT24_MAX )
#  define UINT24_MAX    0x0FFFFFFL
#endif
#if !defined( INT24_MAX )
#  define INT24_MAX     8388607L
#endif
#if !defined( INT24_MIN )
#  define INT24_MIN     -8388608L
#endif

#if defined( HB_ARCH_64BIT ) && !defined( HB_OS_WIN_64 )
   typedef signed long         HB_I64;
   typedef unsigned long       HB_U64;
   #define HB_I64_MIN          LONG_MIN
   #define HB_I64_MAX          LONG_MAX
   #define HB_U64_MAX          ULONG_MAX
   typedef signed long         hbI64;
   typedef unsigned long       hbU64;
   #define hbI64Min            LONG_MIN
   #define hbI64Max            LONG_MAX
   #define hbU64Max            ULONG_MAX
#  if !defined( UINT64 )
     typedef ULONG         UINT64;
#  endif
#  if !defined( INT64 )
     typedef LONG          INT64;
#  endif
#  if !defined( UINT64_MAX )
#    define UINT64_MAX    ULONG_MAX
#  endif
#  if !defined( INT64_MAX )
#    define INT64_MAX     LONG_MAX
#  endif
#  if !defined( INT64_MIN )
#    define INT64_MIN     LONG_MIN
#  endif
#elif !defined( HB_LONG_LONG_OFF )
   typedef LONGLONG            HB_I64;
   typedef ULONGLONG           HB_U64;
   #define HB_I64_MIN          LONGLONG_MIN
   #define HB_I64_MAX          LONGLONG_MAX
   #define HB_U64_MAX          ULONGLONG_MAX
   typedef LONGLONG            hbI64;
   typedef ULONGLONG           hbU64;
   #define hbI64Min            LONGLONG_MIN
   #define hbI64Max            LONGLONG_MAX
   #define hbU64Max            ULONGLONG_MAX
#  if !defined( UINT64 )
     typedef ULONGLONG     UINT64;
#  endif
#  if !defined( INT64 )
     typedef LONGLONG      INT64;
#  endif
#  if !defined( UINT64_MAX )
#    define UINT64_MAX     ULONGLONG_MAX
#  endif
#  if !defined( INT64_MAX )
#    define INT64_MAX      LONGLONG_MAX
#  endif
#  if !defined( INT64_MIN )
#    define INT64_MIN      LONGLONG_MIN
#  endif
#endif

#ifndef HB_LONG_DOUBLE_OFF
   typedef long double  HB_MAXDBL;
#else
   typedef double       HB_MAXDBL;
#endif

#if defined( HB_CLIPPER_INT_ITEMS )
#  define HB_INT_MAX             SHRT_MAX
#  define HB_INT_MIN             SHRT_MIN
#  define HB_LONG_MAX            LONG_MAX
#  define HB_LONG_MIN            LONG_MIN
#  define HB_ULONG_MAX           ULONG_MAX
   typedef LONG                  HB_LONG;
   typedef ULONG                 HB_ULONG;
#  define PFHL                   "l"
#elif !defined( HB_LONG_LONG_OFF ) && ULONG_MAX == UINT_MAX
#  define HB_INT_MAX             INT_MAX
#  define HB_INT_MIN             INT_MIN
#  define HB_LONG_MAX            LONGLONG_MAX
#  define HB_LONG_MIN            LONGLONG_MIN
#  define HB_ULONG_MAX           ULONGLONG_MAX
   typedef LONGLONG              HB_LONG;
   typedef ULONGLONG             HB_ULONG;
#else
#  define HB_INT_MAX             INT_MAX
#  define HB_INT_MIN             INT_MIN
#  define HB_LONG_MAX            LONG_MAX
#  define HB_LONG_MIN            LONG_MIN
#  define HB_ULONG_MAX           ULONG_MAX
   typedef LONG                  HB_LONG;
   typedef ULONG                 HB_ULONG;
#  define PFHL                   "l"
#endif

#define HB_DBL_LIM_INT(d)     ( HB_INT_MIN <= (d) && (d) <= HB_INT_MAX )
#define HB_DBL_LIM_LONG(d)    ( (HB_MAXDBL) HB_LONG_MIN <= (HB_MAXDBL) (d) && (HB_MAXDBL) (d) <= (HB_MAXDBL) HB_LONG_MAX )
#define HB_LIM_INT(l)         ( HB_INT_MIN <= (l) && (l) <= HB_INT_MAX )
#define HB_LIM_LONG(l)        ( HB_LONG_MIN <= (l) && (l) <= HB_LONG_MAX )

#define HB_DBL_LIM_INT8(d)    ( -128 <= (d) && (d) <= 127 )
#define HB_DBL_LIM_INT16(d)   ( INT16_MIN <= (d) && (d) <= INT16_MAX )
#define HB_DBL_LIM_INT24(d)   ( INT24_MIN <= (d) && (d) <= INT24_MAX )
#define HB_DBL_LIM_INT32(d)   ( INT32_MIN <= (d) && (d) <= INT32_MAX )
#define HB_DBL_LIM_INT64(d)   ( (HB_MAXDBL) INT64_MIN <= (HB_MAXDBL) (d) && (HB_MAXDBL) (d) <= (HB_MAXDBL) INT64_MAX )
#define HB_LIM_INT8(l)        ( -128 <= (l) && (l) <= 127 )
#define HB_LIM_INT16(l)       ( INT16_MIN <= (l) && (l) <= INT16_MAX )
#define HB_LIM_INT24(l)       ( INT24_MIN <= (l) && (l) <= INT24_MAX )
#define HB_LIM_INT32(l)       ( INT32_MIN <= (l) && (l) <= INT32_MAX )
#define HB_LIM_INT64(l)       ( INT64_MIN <= (l) && (l) <= INT64_MAX )

/*
 * It's a hack for compilers which don't support LL suffix for LONGLONG
 * numeric constant. This suffix is necessary for some compilers -
 * without it they cut the number to LONG
 */
#if defined( __BORLANDC__ )
#  if __BORLANDC__ >= 1328
#     define HB_LL( num )           num##i64
#     define HB_ULL( num )          num##ui64
#  else
#     define HB_LL( num )           num
#     define HB_ULL( num )          num
#  endif
#elif defined( _MSC_VER )
#  define HB_LL( num )           num
#  define HB_ULL( num )          num
#else
#  define HB_LL( num )           num##LL
#  define HB_ULL( num )          num##ULL
#endif


/* HB_*_EXPLENGTH() macros are used by HVM to set the size of
 * math operations, HB_*_LENGTH() macros are used when new
 * item is created. [druzus]
 */
/* NOTE: the positive number limit 999999999 in HB_INT_LENGTH()
 *       (HB_LONG_LENGTH() on 16-bit platforms) below is not
 *       compatible with other limits. Clipper have such limit
 *       but IMHO it's result of some typo or wrong compiler
 *       warnings cleanup when someone removed one digit from
 *       upper limit instead of removing the whole limit.
 *       It's also possible that it comes from DBASE and was
 *       intentionally replicated. I think we should keep it
 *       only in strict compatibility mode. [druzus]
 */
#if HB_INT_MIN < -999999999
#  define HB_INT_LENGTH( i )        ( ( (i) < -999999999 || (i) > 999999999 ) ? 20 : 10 )
#else
#  define HB_INT_LENGTH( i )        10
#  define HB_INT_EXPLENGTH( i )     10
#  if HB_LONG_MIN < -999999999
#     define HB_LONG_LENGTH( i )    ( ( (i) < -999999999 || (i) > 999999999 ) ? 20 : 10 )
#  endif
#endif

#if !defined( HB_LONG_LONG_OFF )
#  if HB_LONG_MAX > HB_LL( 9999999999 )
#     define HB_LONG_LENGTH( l )    ( ( (l) < -999999999 || (l) > HB_LL( 9999999999 ) ) ? 20 : 10 )
#  endif
#  if HB_INT_MAX > HB_LL( 9999999999 )
#     define HB_INT_EXPLENGTH( i )  HB_LONG_LENGTH( i )
#  endif
#endif

#if !defined( HB_LONG_LENGTH )
#  define HB_LONG_LENGTH( l )       ( ( (l) < -999999999 ) ? 20 : 10 )
#endif
#if !defined( HB_INT_EXPLENGTH )
#  define HB_INT_EXPLENGTH( i )     ( ( (i) < -999999999 ) ? 20 : 10 )
#endif
#if !defined( HB_LONG_EXPLENGTH )
#  define HB_LONG_EXPLENGTH( l ) HB_LONG_LENGTH( l )
#endif

/* HB_DBL_LENGTH() is used by VAL() for strings longer then 10 characters
 * (counted to '.') and to set the size of math operations and new
 * double item - it's CA-Cl*pper compatible range. For doubles we do
 * not have separated limit for result of math operations. [druzus]
 */
#define HB_DBL_LENGTH( d ) ( ( (d) > 9999999999.0 || (d) < -999999999.0 ) ? 20 : 10 )

/* uncomment this if you need strict Clipper compatibility */
/* #define PCODE_LONG_LIM(l)     HB_LIM_INT32( l ) */

/* #define PCODE_LONG_LIM(l)     HB_LIM_LONG( l ) */

/* type of HB_ITEM */
/* typedef USHORT HB_TYPE; */
typedef UINT32 HB_TYPE;

/* type of reference counter */
typedef unsigned long HB_COUNTER;
#if ULONG_MAX <= UINT32_MAX
#  define HB_COUNTER_SIZE     4
#else
#  define HB_COUNTER_SIZE     8
#endif

/* type for memory pointer diff */
#if defined( HB_OS_WIN_64 )
   typedef LONGLONG HB_PTRDIFF;
   typedef ULONGLONG HB_PTRUINT;
#else
   typedef long HB_PTRDIFF;
   typedef unsigned long HB_PTRUINT;
#endif

#if defined( HB_LONG_LONG_OFF ) || ULONG_MAX == ULONGLONG_MAX
   typedef LONG HB_FOFFSET;
   /* we can add hack with double as work around what should
      effectively give 52bit file size limit */
#else
   typedef LONGLONG HB_FOFFSET;
#endif

#if defined( HB_IO_WIN )
#if 1
   typedef HB_PTRDIFF HB_FHANDLE;
#else
   typedef void * HB_FHANDLE;
#endif
   typedef HB_PTRDIFF HB_NHANDLE;
#  define hb_numToHandle( h )   ( ( HB_FHANDLE ) ( HB_NHANDLE ) ( h ) )
#else
   typedef int HB_FHANDLE;
   typedef int HB_NHANDLE;
#  define hb_numToHandle( h )   ( ( int ) ( h ) )
#endif

/* maximum length of double number in decimal representation:
   log10(2^1024) ~ 308.25 */
#define HB_MAX_DOUBLE_LENGTH 320

/* This value is used to hack the double FL value in round/int
   operation - similar thing is done by CL5.3 - I do not know
   only the exact factor value but it should be close to this one.
   When HB_CLP_STRICT is set this macro is not used.
*/
#define HB_DBLFL_PREC_FACTOR 1.0000000000000002;

/* try to detect byte order if not explicitly set */
#if !defined( HB_PDP_ENDIAN ) && !defined( HB_BIG_ENDIAN ) && \
    !defined( HB_LITTLE_ENDIAN )

   /* I intentionaly move the first two #if/#elif to the begining
      to avoid compiler error when this macro will be defined as
      empty statement in next conditions, F.e. SunOS
    */
#  if ( defined( __LITTLE_ENDIAN__ ) && ! defined( __BIG_ENDIAN__ ) ) || \
      ( defined( __LITTLE_ENDIAN ) && ! defined( __BIG_ENDIAN ) ) || \
      ( defined( _LITTLE_ENDIAN ) && ! defined( _BIG_ENDIAN ) ) || \
      ( defined( LITTLE_ENDIAN ) && ! defined( BIG_ENDIAN ) )

#     define HB_LITTLE_ENDIAN

#  elif ( ! defined( __LITTLE_ENDIAN__ ) && defined( __BIG_ENDIAN__ ) ) || \
        ( ! defined( __LITTLE_ENDIAN ) && defined( __BIG_ENDIAN ) ) || \
        ( ! defined( _LITTLE_ENDIAN ) && defined( _BIG_ENDIAN ) ) || \
        ( ! defined( LITTLE_ENDIAN ) && defined( BIG_ENDIAN ) )

#     define HB_BIG_ENDIAN

#  elif ( defined( __BYTE_ORDER ) && defined( __LITTLE_ENDIAN ) && __BYTE_ORDER == __LITTLE_ENDIAN ) || \
        ( defined( _BYTE_ORDER ) && defined( _LITTLE_ENDIAN ) && _BYTE_ORDER == _LITTLE_ENDIAN ) || \
        ( defined( BYTE_ORDER ) && defined( LITTLE_ENDIAN ) && BYTE_ORDER == LITTLE_ENDIAN )

#     define HB_LITTLE_ENDIAN

#  elif ( defined( __BYTE_ORDER ) && defined( __BIG_ENDIAN ) && __BYTE_ORDER == __BIG_ENDIAN ) || \
        ( defined( _BYTE_ORDER ) && defined( _BIG_ENDIAN ) && _BYTE_ORDER == _BIG_ENDIAN ) || \
        ( defined( BYTE_ORDER ) && defined( BIG_ENDIAN ) && BYTE_ORDER == BIG_ENDIAN )

#     define HB_BIG_ENDIAN

#  elif ( defined( __BYTE_ORDER ) && defined( __PDP_ENDIAN ) && __BYTE_ORDER == __PDP_ENDIAN ) || \
        ( defined( _BYTE_ORDER ) && defined( _PDP_ENDIAN ) && _BYTE_ORDER == _PDP_ENDIAN ) || \
        ( defined( BYTE_ORDER ) && defined( PDP_ENDIAN ) && BYTE_ORDER == PDP_ENDIAN )

#     define HB_PDP_ENDIAN

#  else /* We cannot detect byte order, we will have to guess */

#     if defined( HB_OS_DARWIN ) || defined( HB_OS_SUNOS ) || defined( HB_OS_HPUX )
#        define HB_BIG_ENDIAN
#     else
#        define HB_LITTLE_ENDIAN
#     endif

#  endif

#endif


/* New Harbour types (Planning stage) */

/*
  ANSI C types:
     void,
     [ [un]signed ] char, [ [un]signed ] short, [ [un]signed ] int,
     [ [un]signed ] long, double
*/

/* TODO: Remove dependence on old types */

/* Harbour overloaded types: */

/* http://www.opengroup.org/onlinepubs/009695399/basedefs/stdint.h.html
   http://www.opengroup.org/onlinepubs/009695399/basedefs/stdbool.h.html */

#define HB_FALSE 0
#define HB_TRUE  (!0)

typedef int                 HB_BOOL;
typedef char                HB_CHAR;
typedef signed char         HB_SCHAR;
typedef unsigned char       HB_UCHAR;
typedef short               HB_SHORT;
typedef unsigned short      HB_USHORT;
typedef int                 HB_INT;
typedef unsigned int        HB_UINT;
#if !defined( HB_LONG_LONG_OFF )
   typedef LONGLONG            HB_LONGLONG;
   typedef ULONGLONG           HB_ULONGLONG;
#endif
typedef double              HB_DOUBLE;
typedef long                HB_SIZE;
typedef void *              HB_PTRVAL;         /* TOFIX */
/* typedef                     HB_POINTER;    */    /* TOFIX */

/* Harbour strict bit types: */
typedef signed char         HB_I8;
typedef unsigned char       HB_U8;

/* ---------------- */

#define hbFALSE  0
#define hbTRUE   (!0)

typedef int                 hbBool;
typedef char                hbChar;           /* TOFIX */
typedef signed char         hbSChar;
typedef unsigned char       hbUChar;
typedef short               hbShort;
typedef unsigned short      hbUShort;
typedef int                 hbInt;
typedef unsigned int        hbUInt;
typedef long                hbLong;
typedef unsigned long       hbULong;
#if !defined( HB_LONG_LONG_OFF )
   typedef LONGLONG            hbLongLong;
   typedef ULONGLONG           hbULongLong;
#endif
typedef double              hbDouble;
/* typedef                     hbIntMax;     */    /* TOFIX */
/* typedef                     hbDoubleMax;  */    /* TOFIX */
typedef unsigned short      hbCounter;
typedef long                hbSize;
typedef HB_PTRDIFF          hbPtrDiff;
typedef void *              hbPtrVal;         /* TOFIX */
/* typedef                     hbPointer;    */    /* TOFIX */
/* typedef                     hbWChar;      */    /* TOFIX */

/* Harbour strict bit types: */
typedef signed char         hbI8;
typedef unsigned char       hbU8;

/* Types which depends on internal HVM/compilation settings: */
/* NOTE: hbMaxVMInt - maximal integer which can be stored in HVM item
                      (HB_IT_LONG). It's currently HB_LONG, usually will be
                      the same as hbIntMax unless for some reasons it will
                      not be reduced, f.e. compiler may support 128bit
                      integers as hbIntMax but we may don't use it for
                      HB_IT_LONG due to performance reduction. */
typedef long                hbVMIntMax;       /* TOFIX */

#define HB_MAX( a, b )          ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )
#define HB_MIN( a, b )          ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )

#define HB_LOBYTE( w )          ( ( BYTE ) ( w ) )
#define HB_HIBYTE( w )          ( ( BYTE ) ( ( ( w ) >>  8 ) & 0xFF ) )
#define HB_ULBYTE( w )          ( ( BYTE ) ( ( ( w ) >> 16 ) & 0xFF ) )
#define HB_UHBYTE( w )          ( ( BYTE ) ( ( ( w ) >> 24 ) & 0xFF ) )
#define HB_LOWORD( l )          ( ( UINT16 ) ( l ) )
#define HB_HIWORD( l )          ( ( UINT16 ) ( ( ( l ) >> 16 ) & 0xFFFF ) )
#define HB_MKSHORT( lo, hi )    ( ( SHORT ) ( ( ( INT16 ) ( hi ) ) << 8 ) | ( lo ) )
#define HB_MKUSHORT( lo, hi )   ( ( USHORT ) ( ( ( UINT16 ) ( hi ) ) << 8 ) | ( lo ) )
#define HB_MKLONG( b1, b2, b3, b4 )  ( ( LONG ) \
                                       ( ( ( ( INT32 ) ( b4 ) ) << 24 ) | \
                                         ( ( ( INT32 ) ( b3 ) ) << 16 ) | \
                                         ( ( ( INT32 ) ( b2 ) ) <<  8 ) | \
                                         ( ( ( INT32 ) ( b1 ) ) ) ) )
#define HB_MKULONG( b1, b2, b3, b4 ) ( ( ULONG ) \
                                       ( ( ( ( UINT32 ) ( b4 ) ) << 24 ) | \
                                         ( ( ( UINT32 ) ( b3 ) ) << 16 ) | \
                                         ( ( ( UINT32 ) ( b2 ) ) <<  8 ) | \
                                         ( ( ( UINT32 ) ( b1 ) ) ) ) )

#define HB_SWAP_UINT16( w )     ( ( UINT16 ) ( ( ( ( UINT16 ) ( w ) & 0xFF00 ) >> 8 ) | \
                                               ( ( ( UINT16 ) ( w ) & 0x00FF ) << 8 ) ) )
#define HB_SWAP_UINT32( w )     ( ( UINT32 ) ( ( ( ( UINT32 ) ( w ) & 0x000000FF ) << 24 ) | \
                                               ( ( ( UINT32 ) ( w ) & 0x0000FF00 ) <<  8 ) | \
                                               ( ( ( UINT32 ) ( w ) & 0x00FF0000 ) >>  8 ) | \
                                               ( ( ( UINT32 ) ( w ) & 0xFF000000 ) >> 24 ) ) )


#ifndef PFLL
#  if defined( __BORLANDC__ ) || defined( _MSC_VER ) || defined( __MINGW32__ )
#     define PFLL    "I64"
#  else
#     define PFLL    "ll"
#  endif
#endif
#ifndef PFHL
#  define PFHL    PFLL
#endif


#define HB_SWAP_UINT64( w )      ( ( UINT64 ) ( ( ( ( UINT64 ) ( w ) & HB_LL( 0x00000000000000FF ) ) << 56 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x000000000000FF00 ) ) << 40 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x0000000000FF0000 ) ) << 24 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x00000000FF000000 ) ) <<  8 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x000000FF00000000 ) ) >>  8 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x0000FF0000000000 ) ) >> 24 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x00FF000000000000 ) ) >> 40 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0xFF00000000000000 ) ) >> 56 ) ) )

/*
 * on some machines it's not safe to directly access pointers stored
 * at byte buffer they have to be stored at odd (or other alignment)
 * addresses.
 * For example SPARC which needs 4 byte alignment for pointers
 * and 8 byte alignment for doubles and structures (when GCC is used)
 * IMHO need HB_ARCH_<arch> macro yet - the same OS can be used with
 * different architectures - SPARC + LINUX, ALPHA + LINUX
 */
#if !defined( HB_STRICT_ALIGNMENT )
#  if defined( HB_CPU_SPARC ) || defined( HB_CPU_SPARC_64 ) || \
      defined( HB_CPU_ALPHA ) || \
      defined( HB_CPU_PARISC ) || \
      defined( HB_CPU_MIPS ) || \
      defined( HB_CPU_PPC ) || defined( HB_CPU_PPC_64 ) || \
      defined( HB_CPU_ARM )
#     define HB_STRICT_ALIGNMENT
#  endif
#endif

#if defined( HB_STRICT_ALIGNMENT )
#  if !defined( HB_ALLOC_ALIGNMENT ) || ( HB_ALLOC_ALIGNMENT + 1 == 1 )
#     define HB_ALLOC_ALIGNMENT     8
#  endif
#endif

#if defined( HB_ALLOC_ALIGNMENT ) && HB_COUNTER_SIZE < HB_ALLOC_ALIGNMENT + 0
#  define HB_COUNTER_OFFSET   HB_ALLOC_ALIGNMENT
#else
#  define HB_COUNTER_OFFSET   HB_COUNTER_SIZE
#endif

#define HB_COUNTER_PTR( p )         ((HB_COUNTER*) ((BYTE *) (p)-HB_COUNTER_OFFSET))

#if defined( HB_PDP_ENDIAN )
   #error PDP-Endian support unimplemented. If you have such machine do it yourself.
#endif

/*
 * These macros are necessary for architectures which need
 * strict alignment for pointers.
 */
#if defined( __GNUC__ )
#  define   HB_PUT_PTR( p, v )     _hb_put_ptr( ( BYTE * ) ( p ), v )
#  define   HB_GET_PTR( p )        _hb_get_ptr( ( BYTE * ) ( p ) )
#elif !defined( HB_STRICT_ALIGNMENT )
#  define   HB_PUT_PTR( p, v )      do { *( void ** ) ( p ) = ( void * ) ( v ); } while ( 0 )
#  define   HB_GET_PTR( p )         ( *( void ** ) ( p ) )
#else
#  if defined( HB_BIG_ENDIAN )
#     if defined( HB_ARCH_64BIT )
#        define   HB_PUT_PTR( p, v )   HB_PUT_BE_UINT64( p, ( UINT64 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_BE_UINT64( p ) )
#     else
#        define   HB_PUT_PTR( p, v )   HB_PUT_BE_UINT32( p, ( UINT32 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_BE_UINT32( p ) )
#     endif
#  else
#     if defined( HB_ARCH_64BIT )
#        define   HB_PUT_PTR( p, v )   HB_PUT_LE_UINT64( p, ( UINT64 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_LE_UINT64( p ) )
#     else
#        define   HB_PUT_PTR( p, v )   HB_PUT_LE_UINT32( p, ( UINT32 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_LE_UINT32( p ) )
#     endif
#  endif
#endif
#if defined( HB_BIG_ENDIAN )
#  define   HB_PUT_UINT32( p, v )   HB_PUT_BE_UINT32( p, ( UINT32 ) ( v ) )
#  define   HB_GET_UINT32( p )      HB_GET_BE_UINT32( p )
#else
#  define   HB_PUT_UINT32( p, v )   HB_PUT_LE_UINT32( p, ( UINT32 ) ( v ) )
#  define   HB_GET_UINT32( p )      HB_GET_LE_UINT32( p )
#endif

/* Macros to store/retrieve integer and double values at/from byte address */
#if defined( __GNUC__ )

#  if ( __GNUC__ > 4 || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 3 ) ) && \
      ! defined( __ICC )
#     define HB_BUILTIN_BSWAP 1
#  else
#     define HB_BUILTIN_BSWAP 0
#  endif

   typedef union
   {
      void *   val;
#  if defined( HB_ARCH_64BIT )
      BYTE     buf[8];
#  else
      BYTE     buf[4];
#  endif
   } HB_PTRCAST, * PHB_PTRCAST;

   typedef union
   {
      UINT16   val;
      BYTE     buf[2];
   } HB_U16CAST, * PHB_U16CAST;

   typedef union
   {
      UINT32   val;
      BYTE     buf[4];
   } HB_U32CAST, * PHB_U32CAST;

#  if !defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT )
   typedef union
   {
      UINT64   val;
      BYTE     buf[8];
   } HB_U64CAST, * PHB_U64CAST;
#  endif

   typedef union
   {
      double   val;
      BYTE     buf[8];
#  if !defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT )
      UINT64   i64;
#  endif
   } HB_DBLCAST, * PHB_DBLCAST;

   static __inline__ void * _hb_get_ptr( const BYTE * buf )
   {
      HB_PTRCAST u;
      memcpy( u.buf, buf, sizeof( void * ) );
      return u.val;
   }

   static __inline__ void _hb_put_ptr( BYTE * buf, void * val )
   {
      HB_PTRCAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( void * ) );
   }

   static __inline__ UINT16 _hb_get_std_uint16( const BYTE * buf )
   {
      HB_U16CAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static __inline__ void _hb_put_std_uint16( BYTE * buf, UINT16 val )
   {
      HB_U16CAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static __inline__ UINT16 _hb_get_rev_uint16( const BYTE * buf )
   {
      HB_U16CAST u;
      u.buf[ 0 ] = buf[ 1 ];
      u.buf[ 1 ] = buf[ 0 ];
      return u.val;
   }

   static __inline__ void _hb_put_rev_uint16( BYTE * buf, UINT16 val )
   {
      HB_U16CAST u;
      u.val = val;
      buf[ 0 ] = u.buf[ 1 ];
      buf[ 1 ] = u.buf[ 0 ];
   }

   static __inline__ UINT32 _hb_get_std_uint32( const BYTE * buf )
   {
      HB_U32CAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static __inline__ void _hb_put_std_uint32( BYTE * buf, UINT32 val )
   {
      HB_U32CAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static __inline__ UINT32 _hb_get_rev_uint32( const BYTE * buf )
   {
      HB_U32CAST u;
#  if HB_BUILTIN_BSWAP
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return __builtin_bswap32( u.val );
#  else
      u.buf[ 0 ] = buf[ 3 ];
      u.buf[ 1 ] = buf[ 2 ];
      u.buf[ 2 ] = buf[ 1 ];
      u.buf[ 3 ] = buf[ 0 ];
      return u.val;
#  endif
   }

   static __inline__ void _hb_put_rev_uint32( BYTE * buf, UINT32 val )
   {
      HB_U32CAST u;
#  if HB_BUILTIN_BSWAP
      u.val = __builtin_bswap32( val );
      memcpy( buf, u.buf, sizeof( u.buf ) );
#  else
      u.val = val;
      buf[ 0 ] = u.buf[ 3 ];
      buf[ 1 ] = u.buf[ 2 ];
      buf[ 2 ] = u.buf[ 1 ];
      buf[ 3 ] = u.buf[ 0 ];
#  endif
   }

#  if !defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT )
      static __inline__ UINT64 _hb_get_std_uint64( const BYTE * buf )
      {
         HB_U64CAST u;
         memcpy( u.buf, buf, sizeof( u.buf ) );
         return u.val;
      }

      static __inline__ void _hb_put_std_uint64( BYTE * buf, UINT64 val )
      {
         HB_U64CAST u;
         u.val = val;
         memcpy( buf, u.buf, sizeof( u.buf ) );
      }

      static __inline__ UINT64 _hb_get_rev_uint64( const BYTE * buf )
      {
         HB_U64CAST u;
#  if HB_BUILTIN_BSWAP
         memcpy( u.buf, buf, sizeof( u.buf ) );
         return __builtin_bswap64( u.val );
#     else
         u.buf[ 0 ] = buf[ 7 ];
         u.buf[ 1 ] = buf[ 6 ];
         u.buf[ 2 ] = buf[ 5 ];
         u.buf[ 3 ] = buf[ 4 ];
         u.buf[ 4 ] = buf[ 3 ];
         u.buf[ 5 ] = buf[ 2 ];
         u.buf[ 6 ] = buf[ 1 ];
         u.buf[ 7 ] = buf[ 0 ];
         return u.val;
#     endif
      }

      static __inline__ void _hb_put_rev_uint64( BYTE * buf, UINT64 val )
      {
         HB_U64CAST u;
#  if HB_BUILTIN_BSWAP
         u.val = __builtin_bswap64( val );
         memcpy( buf, u.buf, sizeof( u.buf ) );
#     else
         u.val = val;
         buf[ 0 ] = u.buf[ 7 ];
         buf[ 1 ] = u.buf[ 6 ];
         buf[ 2 ] = u.buf[ 5 ];
         buf[ 3 ] = u.buf[ 4 ];
         buf[ 4 ] = u.buf[ 3 ];
         buf[ 5 ] = u.buf[ 2 ];
         buf[ 6 ] = u.buf[ 1 ];
         buf[ 7 ] = u.buf[ 0 ];
#     endif
      }
#  endif

   static __inline__ double _hb_get_std_double( const BYTE * buf )
   {
      HB_DBLCAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static __inline__ void _hb_put_std_double( BYTE * buf, double val )
   {
      HB_DBLCAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static __inline__ double _hb_get_rev_double( const BYTE * buf )
   {
      HB_DBLCAST u;
#  if ( !defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT ) ) && \
      HB_BUILTIN_BSWAP
      memcpy( u.buf, buf, sizeof( u.buf ) );
      u.i64 = __builtin_bswap64( u.i64 );
      return u.val;
#  else
      u.buf[ 0 ] = buf[ 7 ];
      u.buf[ 1 ] = buf[ 6 ];
      u.buf[ 2 ] = buf[ 5 ];
      u.buf[ 3 ] = buf[ 4 ];
      u.buf[ 4 ] = buf[ 3 ];
      u.buf[ 5 ] = buf[ 2 ];
      u.buf[ 6 ] = buf[ 1 ];
      u.buf[ 7 ] = buf[ 0 ];
      return u.val;
#  endif
   }

   static __inline__ void _hb_put_rev_double( BYTE * buf, double val )
   {
      HB_DBLCAST u;
#  if ( !defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT ) ) && \
      HB_BUILTIN_BSWAP
      u.val = val;
      u.i64 = __builtin_bswap64( u.i64 );
      memcpy( buf, u.buf, sizeof( u.buf ) );
#  else
      u.val = val;
      buf[ 0 ] = u.buf[ 7 ];
      buf[ 1 ] = u.buf[ 6 ];
      buf[ 2 ] = u.buf[ 5 ];
      buf[ 3 ] = u.buf[ 4 ];
      buf[ 4 ] = u.buf[ 3 ];
      buf[ 5 ] = u.buf[ 2 ];
      buf[ 6 ] = u.buf[ 1 ];
      buf[ 7 ] = u.buf[ 0 ];
#  endif
   }

#  define HB_GET_STD_DOUBLE( p )       _hb_get_std_double( ( const BYTE * ) ( p ) )
#  define HB_GET_REV_DOUBLE( p )       _hb_get_rev_double( ( const BYTE * ) ( p ) )
#  define HB_PUT_STD_DOUBLE( p, d )    _hb_put_std_double( ( BYTE * ) ( p ), d )
#  define HB_PUT_REV_DOUBLE( p, d )    _hb_put_rev_double( ( BYTE * ) ( p ), d )

#  if defined( HB_BIG_ENDIAN )

#     define HB_GET_BE_UINT16( p )        _hb_get_std_uint16( ( BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT16( p, w )     _hb_put_std_uint16( ( BYTE * ) ( p ), w )
#     define HB_GET_BE_UINT32( p )        _hb_get_std_uint32( ( BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT32( p, l )     _hb_put_std_uint32( ( BYTE * ) ( p ), l )
#     define HB_GET_BE_UINT64( p )        _hb_get_std_uint64( ( BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT64( p, q )     _hb_put_std_uint64( ( BYTE * ) ( p ), q )

#     define HB_GET_LE_UINT16( p )        _hb_get_rev_uint16( ( BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT16( p, w )     _hb_put_rev_uint16( ( BYTE * ) ( p ), w )
#     define HB_GET_LE_UINT32( p )        _hb_get_rev_uint32( ( BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT32( p, l )     _hb_put_rev_uint32( ( BYTE * ) ( p ), l )
#     define HB_GET_LE_UINT64( p )        _hb_get_rev_uint64( ( BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT64( p, q )     _hb_put_rev_uint64( ( BYTE * ) ( p ), q )

#  else /* HB_LITTLE_ENDIAN */

#     define HB_GET_BE_UINT16( p )        _hb_get_rev_uint16( ( BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT16( p, w )     _hb_put_rev_uint16( ( BYTE * ) ( p ), w )
#     define HB_GET_BE_UINT32( p )        _hb_get_rev_uint32( ( BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT32( p, l )     _hb_put_rev_uint32( ( BYTE * ) ( p ), l )
#     define HB_GET_BE_UINT64( p )        _hb_get_rev_uint64( ( BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT64( p, q )     _hb_put_rev_uint64( ( BYTE * ) ( p ), q )

#     define HB_GET_LE_UINT16( p )        _hb_get_std_uint16( ( BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT16( p, w )     _hb_put_std_uint16( ( BYTE * ) ( p ), w )
#     define HB_GET_LE_UINT32( p )        _hb_get_std_uint32( ( BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT32( p, l )     _hb_put_std_uint32( ( BYTE * ) ( p ), l )
#     define HB_GET_LE_UINT64( p )        _hb_get_std_uint64( ( BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT64( p, q )     _hb_put_std_uint64( ( BYTE * ) ( p ), q )

#  endif

#else /* ! __GNUC__ */

#  define HB_GET_STD_DOUBLE( p )    hb_get_std_double( ( BYTE * ) ( p ) )
#  define HB_GET_REV_DOUBLE( p )    hb_get_rev_double( ( BYTE * ) ( p ) )
#  define HB_PUT_REV_DOUBLE( p, d ) \
         do { \
            union { \
               double dbl; \
               BYTE buffer[ 8 ]; \
            } u; \
            u.dbl = ( double ) ( d ); \
            (( BYTE * )( p ))[ 7 ] = u.buffer[ 0 ]; \
            (( BYTE * )( p ))[ 6 ] = u.buffer[ 1 ]; \
            (( BYTE * )( p ))[ 5 ] = u.buffer[ 2 ]; \
            (( BYTE * )( p ))[ 4 ] = u.buffer[ 3 ]; \
            (( BYTE * )( p ))[ 3 ] = u.buffer[ 4 ]; \
            (( BYTE * )( p ))[ 2 ] = u.buffer[ 5 ]; \
            (( BYTE * )( p ))[ 1 ] = u.buffer[ 6 ]; \
            (( BYTE * )( p ))[ 0 ] = u.buffer[ 7 ]; \
         } while ( 0 )
#  define HB_PUT_STD_DOUBLE( p, d ) \
         do { \
            union { \
               double dbl; \
               BYTE buffer[ 8 ]; \
            } u; \
            u.dbl = ( double ) ( d ); \
            (( BYTE * )( p ))[ 0 ] = u.buffer[ 0 ]; \
            (( BYTE * )( p ))[ 1 ] = u.buffer[ 1 ]; \
            (( BYTE * )( p ))[ 2 ] = u.buffer[ 2 ]; \
            (( BYTE * )( p ))[ 3 ] = u.buffer[ 3 ]; \
            (( BYTE * )( p ))[ 4 ] = u.buffer[ 4 ]; \
            (( BYTE * )( p ))[ 5 ] = u.buffer[ 5 ]; \
            (( BYTE * )( p ))[ 6 ] = u.buffer[ 6 ]; \
            (( BYTE * )( p ))[ 7 ] = u.buffer[ 7 ]; \
         } while ( 0 )

#  if !defined( HB_STRICT_ALIGNMENT ) && defined( HB_LITTLE_ENDIAN )

   #define HB_GET_LE_UINT16( p )    ( *( UINT16 * )( p ) )
   #define HB_PUT_LE_UINT16( p, w ) ( *( UINT16 * )( p ) = ( UINT16 ) ( w ) )
   #define HB_GET_LE_UINT32( p )    ( *( UINT32 * )( p ) )
   #define HB_PUT_LE_UINT32( p, l ) ( *( UINT32 * )( p ) = ( UINT32 ) ( l ) )
   #define HB_GET_LE_UINT64( p )    ( *( UINT64 * )( p ) )
   #define HB_PUT_LE_UINT64( p, q ) ( *( UINT64 * )( p ) = ( UINT64 ) ( q ) )

#  else

   #define HB_GET_LE_UINT16( p )    ( ( UINT16 ) \
                                      ( ( ( UINT16 ) (( BYTE * )( p ))[0] ) | \
                                        ( ( UINT16 ) (( BYTE * )( p ))[1] <<  8 ) ) )
   #define HB_GET_LE_UINT32( p )    ( ( UINT32 ) \
                                      ( ( ( UINT32 ) (( BYTE * )( p ))[0] ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[2] << 16 ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[3] << 24 ) ) )
   #define HB_GET_LE_UINT64( p )    ( ( UINT64 ) \
                                      ( ( ( UINT64 ) (( BYTE * )( p ))[0] ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[2] << 16 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[3] << 24 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[4] << 32 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[5] << 40 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[6] << 48 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[7] << 56 ) ) )

   #define HB_PUT_LE_UINT16( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( w ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (w) >>  8 ); \
                                       } while ( 0 )
   #define HB_PUT_LE_UINT32( p, l )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( l ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (l) >>  8 ); \
                                         (( BYTE * )( p ))[2] = ( BYTE )( (l) >> 16 ); \
                                         (( BYTE * )( p ))[3] = ( BYTE )( (l) >> 24 ); \
                                       } while ( 0 )
   #define HB_PUT_LE_UINT64( p, q )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( q ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (q) >>  8 ); \
                                         (( BYTE * )( p ))[2] = ( BYTE )( (q) >> 16 ); \
                                         (( BYTE * )( p ))[3] = ( BYTE )( (q) >> 24 ); \
                                         (( BYTE * )( p ))[4] = ( BYTE )( (q) >> 32 ); \
                                         (( BYTE * )( p ))[5] = ( BYTE )( (q) >> 40 ); \
                                         (( BYTE * )( p ))[6] = ( BYTE )( (q) >> 48 ); \
                                         (( BYTE * )( p ))[7] = ( BYTE )( (q) >> 56 ); \
                                       } while ( 0 )
#  endif

#  if !defined( HB_STRICT_ALIGNMENT ) && defined( HB_BIG_ENDIAN )

   #define HB_GET_BE_UINT16( p )    ( *( UINT16 * )( p ) )
   #define HB_PUT_BE_UINT16( p, w ) ( *( UINT16 * )( p ) = ( UINT16 ) ( w ) )
   #define HB_GET_BE_UINT32( p )    ( *( UINT32 * )( p ) )
   #define HB_PUT_BE_UINT32( p, l ) ( *( UINT32 * )( p ) = ( UINT32 ) ( l ) )
   #define HB_GET_BE_UINT64( p )    ( *( UINT64 * )( p ) )
   #define HB_PUT_BE_UINT64( p, q ) ( *( UINT64 * )( p ) = ( UINT64 ) ( q ) )

#  else

   #define HB_GET_BE_UINT16( p )    ( ( UINT16 ) \
                                      ( ( ( UINT16 ) (( BYTE * )( p ))[0] << 8 ) | \
                                        ( ( UINT16 ) (( BYTE * )( p ))[1] ) ) )
   #define HB_GET_BE_UINT32( p )    ( ( UINT32 ) \
                                      ( ( ( UINT32 ) (( BYTE * )( p ))[0] << 24 ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[1] << 16 ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[2] <<  8 ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[3] ) ) )
   #define HB_GET_BE_UINT64( p )    ( ( UINT64 ) \
                                      ( ( ( UINT64 ) (( BYTE * )( p ))[0] << 56 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[1] << 48 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[2] << 40 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[3] << 32 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[4] << 24 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[5] << 16 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[6] <<  8 ) | \
                                        ( ( UINT64 ) (( BYTE * )( p ))[7] ) ) )

   #define HB_PUT_BE_UINT16( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( (w) >>  8 ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( w ); \
                                       } while ( 0 )
   #define HB_PUT_BE_UINT32( p, l )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( (l) >> 24 ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (l) >> 16 ); \
                                         (( BYTE * )( p ))[2] = ( BYTE )( (l) >>  8 ); \
                                         (( BYTE * )( p ))[3] = ( BYTE )( l ); \
                                       } while ( 0 )
   #define HB_PUT_BE_UINT64( p, q )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( (q) >> 56 ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (q) >> 48 ); \
                                         (( BYTE * )( p ))[2] = ( BYTE )( (q) >> 40 ); \
                                         (( BYTE * )( p ))[3] = ( BYTE )( (q) >> 32 ); \
                                         (( BYTE * )( p ))[4] = ( BYTE )( (q) >> 24 ); \
                                         (( BYTE * )( p ))[5] = ( BYTE )( (q) >> 16 ); \
                                         (( BYTE * )( p ))[6] = ( BYTE )( (q) >>  8 ); \
                                         (( BYTE * )( p ))[7] = ( BYTE )( q ); \
                                       } while ( 0 )
#  endif

#endif /* ! __GNUC__ */

/*
 * HB_FORCE_IEEE754_DOUBLE will can be used on platforms which use differ
 * double format and we want to force storing double number as IEEE754
 * double value for sharing binary data (f.e. PCODE in .hrb files or CDX
 * indexes or DBFs with "B" fields.
 */
#if defined( HB_FORCE_IEEE754_DOUBLE )

#  define HB_GET_LE_DOUBLE( p )     hb_get_ieee754( ( BYTE * ) ( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  hb_put_ieee754( ( BYTE * ) ( p ), ( d ) )
#  define HB_DBL2ORD( d, o )        hb_put_ord_ieee754( ( o ), *( d ) )
#  define HB_ORD2DBL( o, d )  do { \
                                 *d = hb_get_ord_ieee754( ( BYTE * ) ( o ) ); \
                              } while( 0 )

#elif defined( HB_BIG_ENDIAN )

#  define HB_GET_LE_DOUBLE( p )     HB_GET_REV_DOUBLE( ( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  HB_PUT_REV_DOUBLE( ( p ), ( d ) )

#elif defined( HB_STRICT_ALIGNMENT ) || defined( __GNUC__ )

#  define HB_GET_LE_DOUBLE( p )     HB_GET_STD_DOUBLE( ( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  HB_PUT_STD_DOUBLE( ( p ), ( d ) )

#else

#  define HB_GET_LE_DOUBLE( p )     ( *( double * )( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  ( *( double * )( p ) = ( double ) ( d ) )

#endif

#if ! defined( HB_FORCE_IEEE754_DOUBLE )
#  if defined( HB_BIG_ENDIAN )

   #define HB_ORD2DBL( o, d )       do { \
      if ( ( ( BYTE * ) ( o ) )[ 0 ] & 0x80 ) { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( BYTE * ) ( o ) )[ 0 ]; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( BYTE * ) ( o ) )[ 1 ]; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( BYTE * ) ( o ) )[ 2 ]; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( BYTE * ) ( o ) )[ 3 ]; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( BYTE * ) ( o ) )[ 4 ]; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( BYTE * ) ( o ) )[ 5 ]; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( BYTE * ) ( o ) )[ 6 ]; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( BYTE * ) ( o ) )[ 7 ] ^ ( BYTE ) 0x80; \
      } else { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( BYTE * ) ( o ) )[ 0 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( BYTE * ) ( o ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( BYTE * ) ( o ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( BYTE * ) ( o ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( BYTE * ) ( o ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( BYTE * ) ( o ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( BYTE * ) ( o ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( BYTE * ) ( o ) )[ 7 ] ^ ( BYTE ) 0xFF; \
      } } while ( 0 )

   #define HB_DBL2ORD( d, o )       do { \
      if ( *( double * )( d ) >= 0.0 ) { \
         if( *( double * )( d ) == -0.0 ) *( double * )( d ) = 0.0; \
         ( ( BYTE * ) ( o ) )[ 0 ] = ( ( BYTE * ) ( d ) )[ 0 ] ^ ( BYTE ) 0x80; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( BYTE * ) ( d ) )[ 1 ]; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( BYTE * ) ( d ) )[ 2 ]; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( BYTE * ) ( d ) )[ 3 ]; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( BYTE * ) ( d ) )[ 4 ]; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( BYTE * ) ( d ) )[ 5 ]; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( BYTE * ) ( d ) )[ 6 ]; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( BYTE * ) ( d ) )[ 7 ]; \
      } else { \
         ( ( BYTE * ) ( o ) )[ 0 ] = ( ( BYTE * ) ( d ) )[ 0 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( BYTE * ) ( d ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( BYTE * ) ( d ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( BYTE * ) ( d ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( BYTE * ) ( d ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( BYTE * ) ( d ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( BYTE * ) ( d ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( BYTE * ) ( d ) )[ 7 ] ^ ( BYTE ) 0xFF; \
      } } while ( 0 )

#  else /* HB_LITTLE_ENDIAN */

   #define HB_ORD2DBL( o, d )       do { \
      if ( ( ( BYTE * ) ( o ) )[ 0 ] & 0x80 ) { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( BYTE * ) ( o ) )[ 7 ]; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( BYTE * ) ( o ) )[ 6 ]; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( BYTE * ) ( o ) )[ 5 ]; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( BYTE * ) ( o ) )[ 4 ]; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( BYTE * ) ( o ) )[ 3 ]; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( BYTE * ) ( o ) )[ 2 ]; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( BYTE * ) ( o ) )[ 1 ]; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( BYTE * ) ( o ) )[ 0 ] ^ ( BYTE ) 0x80; \
      } else { \
         ( ( BYTE * ) ( d ) )[ 0 ] = ( ( BYTE * ) ( o ) )[ 7 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 1 ] = ( ( BYTE * ) ( o ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 2 ] = ( ( BYTE * ) ( o ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 3 ] = ( ( BYTE * ) ( o ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 4 ] = ( ( BYTE * ) ( o ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 5 ] = ( ( BYTE * ) ( o ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 6 ] = ( ( BYTE * ) ( o ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( d ) )[ 7 ] = ( ( BYTE * ) ( o ) )[ 0 ] ^ ( BYTE ) 0xFF; \
      } } while ( 0 )

   #define HB_DBL2ORD( d, o )       do { \
      if ( *( double * )( d ) >= 0.0 ) { \
         if( *( double * )( d ) == -0.0 ) *( double * )( d ) = 0.0; \
         ( ( BYTE * ) ( o ) )[ 0 ] = ( ( BYTE * ) ( d ) )[ 7 ] ^ ( BYTE ) 0x80; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( BYTE * ) ( d ) )[ 6 ]; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( BYTE * ) ( d ) )[ 5 ]; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( BYTE * ) ( d ) )[ 4 ]; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( BYTE * ) ( d ) )[ 3 ]; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( BYTE * ) ( d ) )[ 2 ]; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( BYTE * ) ( d ) )[ 1 ]; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( BYTE * ) ( d ) )[ 0 ]; \
      } else { \
         ( ( BYTE * ) ( o ) )[ 0 ] = ( ( BYTE * ) ( d ) )[ 7 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 1 ] = ( ( BYTE * ) ( d ) )[ 6 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 2 ] = ( ( BYTE * ) ( d ) )[ 5 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 3 ] = ( ( BYTE * ) ( d ) )[ 4 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 4 ] = ( ( BYTE * ) ( d ) )[ 3 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 5 ] = ( ( BYTE * ) ( d ) )[ 2 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 6 ] = ( ( BYTE * ) ( d ) )[ 1 ] ^ ( BYTE ) 0xFF; \
         ( ( BYTE * ) ( o ) )[ 7 ] = ( ( BYTE * ) ( d ) )[ 0 ] ^ ( BYTE ) 0xFF; \
      } } while ( 0 )
#  endif

#endif /* ! defined( HB_FORCE_IEEE754_DOUBLE ) */


/* Now the rest of endian macros */

/*
 * 24 bit integers are not directly supported by any processor we used so far
 * so we always have to build them from BYTEs and cannot use C casting
 */
#define HB_GET_LE_INT24( p )        ( ( INT32 ) \
                                      ( ( ( INT32 ) (( BYTE * )( p ))[0] ) | \
                                        ( ( INT32 ) (( BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( INT32 ) (( BYTE * )( p ))[2] << 16 ) | \
                                        ( ( INT32 ) ((( BYTE * )( p ))[2] & 0x80 ? 0xFF : 0x00 ) << 24 ) ) )
#define HB_GET_LE_UINT24( p )       ( ( UINT32 ) \
                                      ( ( ( UINT32 ) (( BYTE * )( p ))[0] ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[2] << 16 ) ) )
#define HB_PUT_LE_UINT24( p, u )    do { \
                                       (( BYTE * )( p ))[0] = ( BYTE )( u ); \
                                       (( BYTE * )( p ))[1] = ( BYTE )( (u) >>  8 ); \
                                       (( BYTE * )( p ))[2] = ( BYTE )( (u) >> 16 ); \
                                    } while ( 0 )
#define HB_GET_BE_INT24( p )        ( ( INT32 ) \
                                      ( ( ( INT32 ) (( BYTE * )( p ))[2] ) | \
                                        ( ( INT32 ) (( BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( INT32 ) (( BYTE * )( p ))[0] << 16 ) | \
                                        ( ( INT32 ) ((( BYTE * )( p ))[0] & 0x80 ? 0xFF : 0x00 ) << 24 ) ) )
#define HB_GET_BE_UINT24( p )       ( ( UINT32 ) \
                                      ( ( ( UINT32 ) (( BYTE * )( p ))[2] ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( UINT32 ) (( BYTE * )( p ))[0] << 16 ) ) )
#define HB_PUT_BE_UINT24( p, u )    do { \
                                       (( BYTE * )( p ))[2] = ( BYTE )( u ); \
                                       (( BYTE * )( p ))[1] = ( BYTE )( (u) >>  8 ); \
                                       (( BYTE * )( p ))[0] = ( BYTE )( (u) >> 16 ); \
                                    } while ( 0 )


#define HB_GET_LE_INT16( p )        (( INT16 ) HB_GET_LE_UINT16( p ))
#define HB_GET_LE_INT32( p )        (( INT32 ) HB_GET_LE_UINT32( p ))
#define HB_GET_LE_INT64( p )        (( INT64 ) HB_GET_LE_UINT64( p ))

#define HB_PCODE_MKSHORT( p )       (( SHORT )     HB_GET_LE_INT16( p ))
#define HB_PCODE_MKUSHORT( p )      (( USHORT )    HB_GET_LE_UINT16( p ))
#define HB_PCODE_MKLONG( p )        (( LONG )      HB_GET_LE_INT32( p ))
#define HB_PCODE_MKULONG( p )       (( ULONG )     HB_GET_LE_UINT32( p ))
#define HB_PCODE_MKLONGLONG( p )    (( LONGLONG )  HB_GET_LE_INT64( p ))
#define HB_PCODE_MKULONGLONG( p )   (( ULONGLONG ) HB_GET_LE_UINT64( p ))
#define HB_PCODE_MKDOUBLE( p )      (( double )    HB_GET_LE_DOUBLE( p ))
#define HB_PCODE_MKINT24( p )       (( LONG )      HB_GET_LE_INT24( p ))
#define HB_PCODE_MKUINT24( p )      (( ULONG )     HB_GET_LE_UINT24( p ))

/*
 * Below are hacked version of INT64 macros which operates on double
 * when INT64 is not supported - they are necessary for PCODE and
 * database access
 */
#if defined( HB_LONG_LONG_OFF ) && !defined( HB_ARCH_64BIT )
   #undef HB_GET_LE_INT64
   #undef HB_GET_LE_UINT64
   #undef HB_PUT_LE_UINT64
   #undef HB_PCODE_MKLONGLONG
   #undef HB_PCODE_MKULONGLONG
   #undef HB_DBL_LIM_INT64
   #define UINT64_MAXDBL               ( (( double ) UINT32_MAX + 1.0) * \
                                         (( double ) UINT32_MAX + 1.0) - 1.0 )
   #define HB_GET_LE_INT64( p )        hb_get_le_int64( ( BYTE * ) ( p ) )
   #define HB_GET_LE_UINT64( p )       hb_get_le_uint64( ( BYTE * ) ( p ) )
   #define HB_PUT_LE_UINT64( p, d )    hb_put_le_uint64( ( BYTE * ) ( p ), \
                                                         ( double ) ( d ) )
   #define HB_PCODE_MKLONGLONG( p )    (( double ) HB_GET_LE_INT64( p ))
   #define HB_PCODE_MKULONGLONG( p )   (( double ) HB_GET_LE_UINT64( p ))
   #define HB_DBL_LIM_INT64(d)         ( (HB_MAXDBL) -UINT64_MAXDBL / 2 - 1 <= \
                                         (HB_MAXDBL) (d) && (HB_MAXDBL) (d) <= \
                                         (HB_MAXDBL) UINT64_MAXDBL / 2 )
#endif

#define HB_MACRO2STRING( macro )    HB_MACRO2STRING_( macro )
#define HB_MACRO2STRING_( macro )   #macro

#define HB_SIZEOFARRAY( var )       ( sizeof( var ) / sizeof( *var ) )


#if defined( __POCC__ ) || defined( __XCC__ )
   #define HB_SYMBOL_UNUSED( symbol )  do if( symbol ) {;} while( 0 )
#else
   #define HB_SYMBOL_UNUSED( symbol )  ( void ) symbol
#endif

/* ***********************************************************************
 * The name of starting procedure
 * Note: You have to define it in case when Harbour cannot find the proper
 * starting procedure (due to unknown order of static data initialization)
 */
#define HB_START_PROCEDURE "MAIN"
#if defined( __WATCOMC__ ) || defined( __DMC__ ) || \
    ( defined( __GNUC__ ) && !defined( __DJGPP__ ) && !defined( HB_OS_OS2_GCC ) )
   #define HB_START_PROC_STRICT
#endif

#if defined( __WATCOMC__ ) || defined( __DMC__ ) || \
    defined( _MSC_VER ) || defined( __POCC__ )
   #define HB_DLL_ENTRY_POINT    DllMain
#else
   #define HB_DLL_ENTRY_POINT    DllEntryPoint
#endif

#if defined( HB_DYNLIB )
   #if defined( __RSXNT__ )
      /* RSXNT does not support any type of export keyword.
         Exported (i.e., public) names can be obtained via
         the emxexp utility and the output can be used for
         input to a module definition file. See emxdev.doc
         in the RSXNT doc/ directory for more information. */
      #define HB_EXPORT

   #elif defined( __GNUC__ ) && defined( HB_OS_WIN )
      #define HB_EXPORT __attribute__ (( dllexport ))

   #elif defined( __GNUC__ ) && defined( HB_OS_LINUX ) && __GNUC__ >= 3
      #define HB_EXPORT __attribute__ ((visibility ("default")))

   #elif defined( __BORLANDC__ )
      #define HB_EXPORT __declspec( dllexport )

   #elif defined( __WATCOMC__ )
      #define HB_EXPORT __declspec( dllexport )

   #elif defined( ASANLM ) || defined( ASANT )
      #define HB_EXPORT

   #elif defined( HB_OS_WIN )
      #define HB_EXPORT _declspec( dllexport )

   #else
      #define HB_EXPORT

   #endif
#else
   #define HB_EXPORT
#endif

#if defined( __RSXNT__ )
   /* RSXNT does not support any type of export keyword.
      Exported (i.e., public) names can be obtained via
      the emxexp utility and the output can be used for
      input to a module definition file. See emxdev.doc
      in the RSXNT doc/ directory for more information. */
   #define HB_IMPORT

#elif defined( __GNUC__ ) && defined( HB_OS_WIN )
   #define HB_IMPORT __attribute__ (( dllimport ))

#elif defined( __BORLANDC__ )
   #define HB_IMPORT __declspec( dllimport )

#elif defined( __WATCOMC__ )
   #define HB_IMPORT __declspec( dllimport )

#elif defined( ASANLM ) || defined( ASANT )
   #define HB_IMPORT

#elif defined( HB_OS_WIN )
   #define HB_IMPORT _declspec( dllimport )

#else
   #define HB_IMPORT

#endif

#if defined( HB_OS_WIN )
   #include "hbwince.h"
#endif

/* Function declaration macros */

/* NOTE: The prefix is "HB_FUN_" currently, this is needed to
         avoid collision with any other declared symbol.
         Note that "HB_" is not enough, since the Harbour internals
         are also prefixed with HB_. [vszakats] */

#define HB_FUNCNAME( funcname )        HB_FUN_##funcname
#define HB_INIT_FUNCNAME( funcname )   HB_FUN_init_##funcname
#define HB_EXIT_FUNCNAME( funcname )   HB_FUN_exit_##funcname
#define HB_INITSTATICS_FUNCNAME()      hb_INITSTATICS

#if defined( __cplusplus ) && !defined( HB_FUNC_USE_DECORATION )
   #define HB_EXTERN_C_ HB_EXTERN_C
   #define HB_EXTERN_
#else
   #define HB_EXTERN_C_
   #define HB_EXTERN_   extern
#endif

#define HB_FUNC_EXEC( funcname )   HB_FUN_##funcname();
#define HB_FUNC( funcname )        HB_EXTERN_C_ HB_EXPORT HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_EXTERN( funcname ) HB_EXTERN_C_ HB_EXTERN_ HARBOUR HB_EXPORT HB_FUN_##funcname ( void )
#define HB_FUNC_STATIC( funcname ) static HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_INIT( funcname )   static HARBOUR HB_FUN_init_##funcname ( void )
#define HB_FUNC_EXIT( funcname )   static HARBOUR HB_FUN_exit_##funcname ( void )
#define HB_FUNC_INITSTATICS( )     static HARBOUR hb_INITSTATICS( void )
#define HB_FUNC_INITLINES( )       static HARBOUR hb_INITLINES( void )

#if defined( HB_FUNC_CALLCONV )
   #define HARBOUR void HB_FUNC_CALLCONV
#else
   #define HARBOUR void
#endif

HB_EXTERN_C_ typedef HARBOUR ( * PHB_FUNC )( void );
HB_EXTERN_C_ typedef PHB_FUNC HB_FUNC_PTR;

typedef SHORT HB_SYMBOLSCOPE;   /* stores symbol's scope */

typedef BYTE HB_ATTR;
typedef int HB_COLOR;

/* Some common character constants */

#define HB_CHAR_NUL             '\0'    /*   0 - NUL */
#define HB_CHAR_EOS             HB_CHAR_NUL
#define HB_CHAR_BEL             '\a'    /*   7 - Bell */
#define HB_CHAR_BS              '\b'    /*   8 - Backspace */
#define HB_CHAR_HT              '\t'    /*   9 - Tab horizontal */
#define HB_CHAR_LF              '\n'    /*  10 - Linefeed */
#define HB_CHAR_VT              '\v'    /*  11 - Tab vertical */
#define HB_CHAR_FF              '\f'    /*  12 - Formfeed */
#define HB_CHAR_CR              '\r'    /*  13 - Carriage return */
#define HB_CHAR_EOF             '\x1A'  /*  26 - End of file marker */

/* Harbour specific character constants */

#define HB_CHAR_HARD1           ( ( char ) HB_CHAR_CR )
#define HB_CHAR_HARD2           ( ( char ) HB_CHAR_LF )

#define HB_CHAR_SOFT1           ( ( char ) 141 )
#define HB_CHAR_SOFT2           ( ( char ) HB_CHAR_LF )

#define HB_ISUPPER( c )         ( ( c ) >= 'A' && ( c ) <= 'Z' )
#define HB_ISLOWER( c )         ( ( c ) >= 'a' && ( c ) <= 'z' )
#define HB_TOUPPER( c )         ( ( c ) >= 'a' && ( c ) <= 'z' ? ( c ) - ( 'a' - 'A' ) : ( c ) )
#define HB_TOLOWER( c )         ( ( c ) >= 'A' && ( c ) <= 'Z' ? ( c ) + ( 'a' - 'A' ) : ( c ) )
#define HB_ISDIGIT( c )         ( ( c ) >= '0' && ( c ) <= '9' )
#define HB_ISALPHA( c )         ( HB_ISUPPER( c ) || HB_ISLOWER( c ) )
#define HB_ISALNUM( c )         ( HB_ISALPHA( c ) || HB_ISDIGIT( c ) )
#define HB_ISXDIGIT( c )        ( HB_ISDIGIT(c) || \
                                  ( (c) >= 'A' && (c) <= 'F' ) || \
                                  ( (c) >= 'a' && (c) <= 'f' ) )
#define HB_ISSPACE( c )         ( ( c ) == ' ' || \
                                  ( c ) == HB_CHAR_HT || \
                                  ( c ) == HB_CHAR_LF || \
                                  ( c ) == HB_CHAR_CR )
#define HB_ISFIRSTIDCHAR( c )   ( HB_ISALPHA( c ) || ( c ) == '_' )
#define HB_ISNEXTIDCHAR( c )    ( HB_ISFIRSTIDCHAR(c) || HB_ISDIGIT( c ) )

#include "hbtrace.h"

#endif /* HB_DEFS_H_ */
