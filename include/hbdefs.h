/*
 * Harbour Project source code:
 * Header file for compiler and runtime basic type declarations
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#if defined( __XCC__ ) || defined( __POCC__ ) || defined( __LCC__ ) || \
    defined( __MINGW32__ ) || defined( __DMC__ ) || defined( __TINYC__ ) || \
    ( defined( _MSC_VER ) && _MSC_VER >= 1600 ) || \
    ( defined( __BORLANDC__ ) && __BORLANDC__ >= 1410 ) || \
    ( defined( __WATCOMC__ ) && __WATCOMC__ >= 1270 ) || \
    ( ( defined( __GNUC__ ) || defined( __SUNPRO_C ) || defined( __SUNPRO_CC ) ) && \
      ( defined( _ISOC99_SOURCE ) || defined( _STDC_C99 ) || \
        ( defined( __STDC_VERSION__ ) && __STDC_VERSION__ >= 199901L ) || \
        ( defined( __DJGPP__ ) && \
          ( __DJGPP__ > 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ >= 4 ) ) ) || \
        defined( HB_OS_LINUX ) || defined( HB_OS_DARWIN ) || \
        defined( HB_OS_BSD ) || defined( HB_OS_SUNOS ) || \
        defined( HB_OS_BEOS ) || defined( HB_OS_QNX ) || \
        defined( HB_OS_VXWORKS ) || defined( HB_OS_MINIX ) ) )
   #include <stdint.h>
   /* NOTE: Hack to avoid collision between stdint.h and unistd.h. [vszakats] */
#  if defined( HB_OS_VXWORKS ) && defined( _INTPTR ) && ! defined( _INTPTR_T )
#     define _INTPTR_T
#  endif
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

#if defined( HB_OS_WIN )
   #if defined( HB_OS_WIN_64 )
      #undef HB_LONG_LONG_OFF
   #endif
#endif

#if defined( HB_OS_DOS )

   #if defined( __WATCOMC__ ) && defined( __386__ ) && ! defined( __WINDOWS_386__ )
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

/* Native Harbour types */

#ifndef HB_LONG_LONG_OFF

   #if defined( HB_OS_WIN ) && ! defined( __GNUC__ )
      typedef __int64            HB_LONGLONG;
      typedef unsigned __int64   HB_ULONGLONG;
   #else
      typedef signed long long   HB_LONGLONG;
      typedef unsigned long long HB_ULONGLONG;
   #endif

   #if ! defined( ULONGLONG_MAX )
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
   #if ! defined( LONGLONG_MAX )
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
   #if ! defined( LONGLONG_MIN )
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

/* Basic types */
#define HB_FALSE 0
#define HB_TRUE  ( ! 0 )

typedef int                 HB_BOOL;
typedef signed char         HB_SCHAR;
typedef unsigned char       HB_UCHAR;
typedef short               HB_SHORT;
typedef unsigned short      HB_USHORT;
typedef long                HB_LONG;           /* WARNING: These types have a new size in Harbour 2.1.x and upper. */
typedef unsigned long       HB_ULONG;          /* WARNING: These types have a new size in Harbour 2.1.x and upper. */
typedef int                 HB_INT;
typedef unsigned int        HB_UINT;

/* Harbour size type */
#if defined( HB_OS_WIN_64 )
#  if defined( HB_SIZE_SIGNED )
      typedef HB_LONGLONG         HB_SIZE;
#  else
      typedef HB_ULONGLONG        HB_SIZE;        /* TODO: Currently 'unsigned', to be changed 'signed' */
#  endif
   typedef HB_LONGLONG         HB_ISIZ;           /* TODO: Change to HB_SIZE, after HB_SIZE has been converted to signed type. TEMPORARY type. */
   typedef HB_ULONGLONG        HB_USIZ;           /* TEMPORARY type. Do not use it. */
#else
#  if defined( HB_SIZE_SIGNED )
      typedef HB_LONG             HB_SIZE;
#  else
      typedef HB_ULONG            HB_SIZE;        /* TODO: Currently 'unsigned', to be changed 'signed' */
#  endif
   typedef HB_LONG             HB_ISIZ;           /* TODO: Change to HB_SIZE, after HB_SIZE has been converted to signed type. TEMPORARY type. */
   typedef HB_ULONG            HB_USIZ;           /* TEMPORARY type. Do not use it. */
#endif

/* Harbour abstract types */
#define HB_AREANO           HB_USHORT
#define HB_FIELDNO          HB_USHORT
#define HB_PARAMNO          HB_USHORT

/* Convenience */
typedef HB_UCHAR            HB_BYTE;

/* Guaranteed 8-bit types */
typedef HB_SCHAR            HB_I8;
typedef HB_UCHAR            HB_U8;

/* Guaranteed 16-bit types */
#if USHRT_MAX == 0xFFFF
   typedef signed short int    HB_I16;
   typedef unsigned short int  HB_U16;
   #define HB_I16_MIN          SHRT_MIN
   #define HB_I16_MAX          SHRT_MAX
   #define HB_U16_MAX          USHRT_MAX
#  if ! defined( UINT16_MAX )
#     define UINT16_MAX    USHRT_MAX
#  endif
#  if ! defined( INT16_MAX )
#     define INT16_MAX     SHRT_MAX
#  endif
#  if ! defined( INT16_MIN )
#     define INT16_MIN     SHRT_MIN
#  endif
#else
   typedef short int           HB_I16;
   typedef unsigned short int  HB_U16;
   #define HB_I16_MIN          SHRT_MIN
   #define HB_I16_MAX          SHRT_MAX
   #define HB_U16_MAX          USHRT_MAX
#endif

/* Guaranteed 32-bit types */
#if UINT_MAX == 0xFFFFFFFF
   typedef signed int          HB_I32;
   typedef unsigned int        HB_U32;
   #define HB_I32_MIN          INT_MIN
   #define HB_I32_MAX          INT_MAX
   #define HB_U32_MAX          UINT_MAX
#  if ! defined( UINT32_MAX )
#     define UINT32_MAX    UINT_MAX
#  endif
#  if ! defined( INT32_MAX )
#     define INT32_MAX     INT_MAX
#  endif
#  if ! defined( INT32_MIN )
#     define INT32_MIN     INT_MIN
#  endif
#elif ULONG_MAX == 0xFFFFFFFF
   typedef signed long         HB_I32;
   typedef unsigned long       HB_U32;
   #define HB_I32_MIN          LONG_MIN
   #define HB_I32_MAX          LONG_MAX
   #define HB_U32_MAX          ULONG_MAX
#  if ! defined( UINT32_MAX )
#     define UINT32_MAX    ULONG_MAX
#  endif
#  if ! defined( INT32_MAX )
#     define INT32_MAX     LONG_MAX
#  endif
#  if ! defined( INT32_MIN )
#     define INT32_MIN     LONG_MIN
#  endif
#endif

#if ! defined( UCHAR_MAX )
#  define UCHAR_MAX     0x0FF
#endif
#if ! defined( UINT24_MAX )
#  define UINT24_MAX    0x0FFFFFFL
#endif
#if ! defined( INT24_MAX )
#  define INT24_MAX     8388607L
#endif
#if ! defined( INT24_MIN )
#  define INT24_MIN     -8388608L
#endif

/* Guaranteed 64-bit types */
#if defined( HB_ARCH_64BIT ) && ! defined( HB_OS_WIN_64 )
   typedef signed long         HB_I64;
   typedef unsigned long       HB_U64;
   #define HB_I64_MIN          LONG_MIN
   #define HB_I64_MAX          LONG_MAX
   #define HB_U64_MAX          ULONG_MAX
   #define HB_PF64             "l"
#  if ! defined( UINT64_MAX )
#     define UINT64_MAX    ULONG_MAX
#  endif
#  if ! defined( INT64_MAX )
#     define INT64_MAX     LONG_MAX
#  endif
#  if ! defined( INT64_MIN )
#     define INT64_MIN     LONG_MIN
#  endif
#elif ! defined( HB_LONG_LONG_OFF )
   typedef HB_LONGLONG         HB_I64;
   typedef HB_ULONGLONG        HB_U64;
   #define HB_I64_MIN          LONGLONG_MIN
   #define HB_I64_MAX          LONGLONG_MAX
   #define HB_U64_MAX          ULONGLONG_MAX
#  if ! defined( UINT64_MAX )
#     define UINT64_MAX     ULONGLONG_MAX
#  endif
#  if ! defined( INT64_MAX )
#     define INT64_MAX      LONGLONG_MAX
#  endif
#  if ! defined( INT64_MIN )
#     define INT64_MIN      LONGLONG_MIN
#  endif
#endif

/* Legacy Windows/Clipper-style types */

#if defined( HB_LEGACY_LEVEL4 ) && defined( HB_LEGACY_TYPES_ON )

   #if ! defined( HB_DONT_DEFINE_BOOL )
      #undef BOOL                         /* boolean */
      typedef HB_BOOL BOOL;
   #endif

   #undef FALSE
   #define FALSE  0
   #undef TRUE
   #define TRUE   ( ! 0 )

   #undef UINT                            /* varies with platform */
   typedef HB_UINT UINT;

   #undef SCHAR                           /* 1 byte signed */
   typedef HB_SCHAR SCHAR;

   #undef UCHAR                           /* 1 byte unsigned */
   typedef HB_UCHAR UCHAR;

   #if ! defined( HB_DONT_DEFINE_BYTE )
      #undef BYTE                            /* 1 byte unsigned */
      typedef HB_UCHAR BYTE;
   #endif

   #undef SHORT                           /* 2 bytes signed */
   typedef HB_SHORT SHORT;

   #undef USHORT                          /* 2 bytes unsigned */
   typedef HB_USHORT USHORT;

   #if ! defined( HB_DONT_DEFINE_LONG )
      #undef LONG                         /* 4 or 8 bytes signed */
      typedef HB_LONG LONG;
   #endif

   #undef ULONG                           /* 4 or 8 bytes unsigned */
   typedef HB_ULONG ULONG;

   #if ! defined( _WINNT_H )
      #if ! defined( HB_LONG_LONG_OFF )
         #if ! defined( LONGLONG )
            typedef HB_LONGLONG LONGLONG;
         #endif
         #if ! defined( ULONGLONG )
            typedef HB_ULONGLONG ULONGLONG;
         #endif
      #endif
   #endif

   #if ! defined( UINT16 )
       typedef HB_U16        UINT16;
   #endif
   #if ! defined( INT16 )
       typedef HB_I16        INT16;
   #endif
   #if ! defined( UINT32 )
       typedef HB_U32        UINT32;
   #endif
   #if ! defined( INT32 )
       typedef HB_I32        INT32;
   #endif
   #if ! defined( HB_LONG_LONG_OFF )
      #if ! defined( UINT64 )
          typedef HB_U64        UINT64;
      #endif
      #if ! defined( INT64 )
          typedef HB_I64        INT64;
      #endif
   #endif

#endif

#ifndef HB_LONG_DOUBLE_OFF
   typedef long double  HB_MAXDBL;
#else
   typedef double       HB_MAXDBL;
#endif

#if defined( HB_CLIPPER_INT_ITEMS )
#  define HB_VMINT_MAX           SHRT_MAX
#  define HB_VMINT_MIN           SHRT_MIN
#  define HB_VMUINT_MAX          USHRT_MAX
#  define HB_VMLONG_MAX          LONG_MAX
#  define HB_VMLONG_MIN          LONG_MIN
#  define HB_VMULONG_MAX         ULONG_MAX
   typedef long                  HB_MAXINT;
   typedef unsigned long         HB_MAXUINT;
#  define PFHL                   "l"
#elif ! defined( HB_LONG_LONG_OFF ) && ULONG_MAX == UINT_MAX
#  define HB_VMINT_MAX           INT_MAX
#  define HB_VMINT_MIN           INT_MIN
#  define HB_VMUINT_MAX          UINT_MAX
#  define HB_VMLONG_MAX          LONGLONG_MAX
#  define HB_VMLONG_MIN          LONGLONG_MIN
#  define HB_VMULONG_MAX         ULONGLONG_MAX
   typedef HB_LONGLONG           HB_MAXINT;
   typedef HB_ULONGLONG          HB_MAXUINT;
#else
#  define HB_VMINT_MAX           INT_MAX
#  define HB_VMINT_MIN           INT_MIN
#  define HB_VMUINT_MAX          UINT_MAX
#  define HB_VMLONG_MAX          LONG_MAX
#  define HB_VMLONG_MIN          LONG_MIN
#  define HB_VMULONG_MAX         ULONG_MAX
   typedef long                  HB_MAXINT;
   typedef unsigned long         HB_MAXUINT;
#  define PFHL                   "l"
#endif

typedef HB_MAXINT    HB_VMMAXINT;
typedef HB_MAXUINT   HB_VMMAXUINT;

#define HB_DBL_LIM_INT(d)     ( HB_VMINT_MIN <= (d) && (d) <= HB_VMINT_MAX )
#define HB_DBL_LIM_LONG(d)    ( (HB_MAXDBL) HB_VMLONG_MIN <= (HB_MAXDBL) (d) && (HB_MAXDBL) (d) <= (HB_MAXDBL) HB_VMLONG_MAX )
#define HB_LIM_INT(l)         ( HB_VMINT_MIN <= (l) && (l) <= HB_VMINT_MAX )
#define HB_LIM_LONG(l)        ( HB_VMLONG_MIN <= (l) && (l) <= HB_VMLONG_MAX )

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
 * without it they cut the number to HB_LONG
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
#if HB_VMINT_MIN < -999999999
#  define HB_INT_LENGTH( i )        ( ( (i) < -999999999 || (i) > 999999999 ) ? 20 : 10 )
#else
#  define HB_INT_LENGTH( i )        10
#  define HB_INT_EXPLENGTH( i )     10
#  if HB_VMLONG_MIN < -999999999
#     define HB_LONG_LENGTH( i )    ( ( (i) < -999999999 || (i) > 999999999 ) ? 20 : 10 )
#  endif
#endif

#if ! defined( HB_LONG_LONG_OFF )
#  if HB_VMLONG_MAX > HB_LL( 9999999999 )
#     define HB_LONG_LENGTH( l )    ( ( (l) < -999999999 || (l) > HB_LL( 9999999999 ) ) ? 20 : 10 )
#  endif
#  if HB_VMINT_MAX > HB_LL( 9999999999 )
#     define HB_INT_EXPLENGTH( i )  HB_LONG_LENGTH( i )
#  endif
#endif

#if ! defined( HB_LONG_LENGTH )
#  define HB_LONG_LENGTH( l )       ( ( (l) < -999999999 ) ? 20 : 10 )
#endif
#if ! defined( HB_INT_EXPLENGTH )
#  define HB_INT_EXPLENGTH( i )     ( ( (i) < -999999999 ) ? 20 : 10 )
#endif
#if ! defined( HB_LONG_EXPLENGTH )
#  define HB_LONG_EXPLENGTH( l ) HB_LONG_LENGTH( l )
#endif

/* HB_DBL_LENGTH() is used by Val() for strings longer then 10 characters
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
typedef HB_U32 HB_TYPE;

/* type of reference counter */
typedef unsigned long HB_COUNTER;
#if ULONG_MAX <= UINT32_MAX
#  define HB_COUNTER_SIZE     4
#else
#  define HB_COUNTER_SIZE     8
#endif

typedef HB_U32 HB_FATTR;

/* type for memory pointer diff */
#if defined( HB_OS_WIN_64 )
   typedef HB_LONGLONG HB_PTRDIFF;
   typedef HB_ULONGLONG HB_PTRUINT;
#else
   typedef long HB_PTRDIFF;
   typedef unsigned long HB_PTRUINT;
#endif

#if defined( HB_LONG_LONG_OFF ) || ULONG_MAX == ULONGLONG_MAX
   typedef HB_LONG HB_FOFFSET;
   /* we can add hack with double as work around what should
      effectively give 52bit file size limit */
#else
   typedef HB_LONGLONG HB_FOFFSET;
#endif

#if defined( HB_OS_WIN )
   typedef HB_PTRDIFF HB_FHANDLE;
   typedef HB_PTRDIFF HB_NHANDLE;
#  define hb_numToHandle( h )   ( ( HB_FHANDLE ) ( HB_NHANDLE ) ( h ) )
#else
   typedef int HB_FHANDLE;
   typedef int HB_NHANDLE;
#  define hb_numToHandle( h )   ( ( int ) ( h ) )
#endif

/* maximum index size */
#if defined( HB_OS_WIN_64 )
#  if defined( HB_SIZE_SIGNED )
#     define HB_SIZE_MAX    LONGLONG_MAX
#  else
#     define HB_SIZE_MAX    ULONGLONG_MAX
#  endif
#else
#  if defined( HB_SIZE_SIGNED )
#     define HB_SIZE_MAX    LONG_MAX
#  else
#     define HB_SIZE_MAX    ULONG_MAX
#  endif
#endif

#if defined( HB_OS_WIN )
   typedef wchar_t         HB_WCHAR;
#else
   typedef unsigned short  HB_WCHAR;
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
#if ! defined( HB_PDP_ENDIAN ) && ! defined( HB_BIG_ENDIAN ) && \
    ! defined( HB_LITTLE_ENDIAN )

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

#define HB_MAX( a, b )          ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )
#define HB_MIN( a, b )          ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )

#define HB_LOBYTE( w )          ( ( HB_BYTE ) ( w ) )
#define HB_HIBYTE( w )          ( ( HB_BYTE ) ( ( ( w ) >>  8 ) & 0xFF ) )
#define HB_ULBYTE( w )          ( ( HB_BYTE ) ( ( ( w ) >> 16 ) & 0xFF ) )
#define HB_UHBYTE( w )          ( ( HB_BYTE ) ( ( ( w ) >> 24 ) & 0xFF ) )
#define HB_LOWORD( l )          ( ( HB_U16 ) ( l ) )
#define HB_HIWORD( l )          ( ( HB_U16 ) ( ( ( l ) >> 16 ) & 0xFFFF ) )
#define HB_MKSHORT( lo, hi )    ( ( HB_SHORT ) ( ( ( HB_I16 ) ( hi ) ) << 8 ) | ( lo ) )
#define HB_MKUSHORT( lo, hi )   ( ( HB_USHORT ) ( ( ( HB_U16 ) ( hi ) ) << 8 ) | ( lo ) )
#define HB_MKLONG( b1, b2, b3, b4 )  ( ( HB_LONG ) \
                                       ( ( ( ( HB_I32 ) ( b4 ) ) << 24 ) | \
                                         ( ( ( HB_I32 ) ( b3 ) ) << 16 ) | \
                                         ( ( ( HB_I32 ) ( b2 ) ) <<  8 ) | \
                                         ( ( ( HB_I32 ) ( b1 ) ) ) ) )
#define HB_MKULONG( b1, b2, b3, b4 ) ( ( HB_ULONG ) \
                                       ( ( ( ( HB_U32 ) ( b4 ) ) << 24 ) | \
                                         ( ( ( HB_U32 ) ( b3 ) ) << 16 ) | \
                                         ( ( ( HB_U32 ) ( b2 ) ) <<  8 ) | \
                                         ( ( ( HB_U32 ) ( b1 ) ) ) ) )

#define HB_SWAP_UINT16( w )     ( ( HB_U16 ) ( ( ( ( HB_U16 ) ( w ) & 0xFF00 ) >> 8 ) | \
                                               ( ( ( HB_U16 ) ( w ) & 0x00FF ) << 8 ) ) )
#define HB_SWAP_UINT32( w )     ( ( HB_U32 ) ( ( ( ( HB_U32 ) ( w ) & 0x000000FF ) << 24 ) | \
                                               ( ( ( HB_U32 ) ( w ) & 0x0000FF00 ) <<  8 ) | \
                                               ( ( ( HB_U32 ) ( w ) & 0x00FF0000 ) >>  8 ) | \
                                               ( ( ( HB_U32 ) ( w ) & 0xFF000000 ) >> 24 ) ) )


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

#ifndef HB_PF64
#  define HB_PF64 PFLL
#endif

#if defined( HB_OS_WIN_64 )
#  define HB_PFS  PFLL
#else
#  define HB_PFS  "l"
#endif

#define HB_SWAP_UINT64( w )      ( ( HB_U64 ) ( ( ( ( HB_U64 ) ( w ) & HB_LL( 0x00000000000000FF ) ) << 56 ) | \
                                                ( ( ( HB_U64 ) ( w ) & HB_LL( 0x000000000000FF00 ) ) << 40 ) | \
                                                ( ( ( HB_U64 ) ( w ) & HB_LL( 0x0000000000FF0000 ) ) << 24 ) | \
                                                ( ( ( HB_U64 ) ( w ) & HB_LL( 0x00000000FF000000 ) ) <<  8 ) | \
                                                ( ( ( HB_U64 ) ( w ) & HB_LL( 0x000000FF00000000 ) ) >>  8 ) | \
                                                ( ( ( HB_U64 ) ( w ) & HB_LL( 0x0000FF0000000000 ) ) >> 24 ) | \
                                                ( ( ( HB_U64 ) ( w ) & HB_LL( 0x00FF000000000000 ) ) >> 40 ) | \
                                                ( ( ( HB_U64 ) ( w ) & HB_LL( 0xFF00000000000000 ) ) >> 56 ) ) )

/*
 * on some machines it's not safe to directly access pointers stored
 * at byte buffer they have to be stored at odd (or other alignment)
 * addresses.
 * For example SPARC which needs 4 byte alignment for pointers
 * and 8 byte alignment for doubles and structures (when GCC is used)
 * IMHO need HB_ARCH_<arch> macro yet - the same OS can be used with
 * different architectures - SPARC + LINUX, ALPHA + LINUX
 */
#if ! defined( HB_STRICT_ALIGNMENT )
#  if ! defined( HB_CPU_X86 ) && \
      ! defined( HB_CPU_X86_64 )
#     define HB_STRICT_ALIGNMENT
#  endif
#endif

#if defined( HB_STRICT_ALIGNMENT )
#  if ! defined( HB_ALLOC_ALIGNMENT ) || ( HB_ALLOC_ALIGNMENT + 1 == 1 )
#     define HB_ALLOC_ALIGNMENT     8
#  endif
#endif

#if defined( HB_ALLOC_ALIGNMENT ) && HB_COUNTER_SIZE < HB_ALLOC_ALIGNMENT + 0
#  define HB_COUNTER_OFFSET   HB_ALLOC_ALIGNMENT
#else
#  define HB_COUNTER_OFFSET   HB_COUNTER_SIZE
#endif

#define HB_COUNTER_PTR( p )         ((HB_COUNTER*) ((HB_BYTE *) (p)-HB_COUNTER_OFFSET))

#if defined( HB_PDP_ENDIAN )
   #error PDP-Endian support unimplemented. If you have such machine do it yourself.
#endif

/*
 * These macros are necessary for architectures which need
 * strict alignment for pointers.
 */
#if defined( __GNUC__ )
#  define   HB_PUT_PTR( p, v )     _hb_put_ptr( ( HB_BYTE * ) ( p ), v )
#  define   HB_GET_PTR( p )        _hb_get_ptr( ( const HB_BYTE * ) ( p ) )
#elif ! defined( HB_STRICT_ALIGNMENT )
#  define   HB_PUT_PTR( p, v )      do { *( void ** ) ( p ) = ( void * ) ( v ); } while( 0 )
#  define   HB_GET_PTR( p )         ( *( void ** ) ( p ) )
#else
#  if defined( HB_BIG_ENDIAN )
#     if defined( HB_ARCH_64BIT )
#        define   HB_PUT_PTR( p, v )   HB_PUT_BE_UINT64( p, ( HB_U64 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_BE_UINT64( p ) )
#     else
#        define   HB_PUT_PTR( p, v )   HB_PUT_BE_UINT32( p, ( HB_U32 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_BE_UINT32( p ) )
#     endif
#  else
#     if defined( HB_ARCH_64BIT )
#        define   HB_PUT_PTR( p, v )   HB_PUT_LE_UINT64( p, ( HB_U64 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_LE_UINT64( p ) )
#     else
#        define   HB_PUT_PTR( p, v )   HB_PUT_LE_UINT32( p, ( HB_U32 ) ( v ) )
#        define   HB_GET_PTR( p )      ( ( void * ) HB_GET_LE_UINT32( p ) )
#     endif
#  endif
#endif
#if defined( HB_BIG_ENDIAN )
#  define   HB_PUT_UINT32( p, v )   HB_PUT_BE_UINT32( p, ( HB_U32 ) ( v ) )
#  define   HB_GET_UINT32( p )      HB_GET_BE_UINT32( p )
#else
#  define   HB_PUT_UINT32( p, v )   HB_PUT_LE_UINT32( p, ( HB_U32 ) ( v ) )
#  define   HB_GET_UINT32( p )      HB_GET_LE_UINT32( p )
#endif

/* Macros to store/retrieve integer and double values at/from byte address */
#if defined( __GNUC__ )

#  if ( __GNUC__ > 4 || ( __GNUC__ == 4 && __GNUC_MINOR__ >= 3 ) ) && \
      ! defined( __ICC ) && ! defined( __OPENCC__ ) && ! defined( __PCC__ )
#     define HB_BUILTIN_BSWAP 1
#  else
#     define HB_BUILTIN_BSWAP 0
#  endif

   typedef union
   {
      void *   val;
#  if defined( HB_ARCH_64BIT )
      HB_BYTE  buf[ 8 ];
#  else
      HB_BYTE  buf[ 4 ];
#  endif
   } HB_PTRCAST, * PHB_PTRCAST;

   typedef union
   {
      HB_U16   val;
      HB_BYTE  buf[ 2 ];
   } HB_U16CAST, * PHB_U16CAST;

   typedef union
   {
      HB_U32   val;
      HB_BYTE  buf[ 4 ];
   } HB_U32CAST, * PHB_U32CAST;

#  if ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT )
   typedef union
   {
      HB_U64   val;
      HB_BYTE  buf[ 8 ];
   } HB_U64CAST, * PHB_U64CAST;
#  endif

   typedef union
   {
      double   val;
      HB_BYTE  buf[ 8 ];
#  if ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT )
      HB_U64   i64;
#  endif
   } HB_DBLCAST, * PHB_DBLCAST;

   static __inline__ void * _hb_get_ptr( const HB_BYTE * buf )
   {
      HB_PTRCAST u;
      memcpy( u.buf, buf, sizeof( void * ) );
      return u.val;
   }

   static __inline__ void _hb_put_ptr( HB_BYTE * buf, void * val )
   {
      HB_PTRCAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( void * ) );
   }

   static __inline__ HB_U16 _hb_get_std_uint16( const HB_BYTE * buf )
   {
      HB_U16CAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static __inline__ void _hb_put_std_uint16( HB_BYTE * buf, HB_U16 val )
   {
      HB_U16CAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static __inline__ HB_U16 _hb_get_rev_uint16( const HB_BYTE * buf )
   {
      HB_U16CAST u;
      u.buf[ 0 ] = buf[ 1 ];
      u.buf[ 1 ] = buf[ 0 ];
      return u.val;
   }

   static __inline__ void _hb_put_rev_uint16( HB_BYTE * buf, HB_U16 val )
   {
      HB_U16CAST u;
      u.val = val;
      buf[ 0 ] = u.buf[ 1 ];
      buf[ 1 ] = u.buf[ 0 ];
   }

   static __inline__ HB_U32 _hb_get_std_uint32( const HB_BYTE * buf )
   {
      HB_U32CAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static __inline__ void _hb_put_std_uint32( HB_BYTE * buf, HB_U32 val )
   {
      HB_U32CAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static __inline__ HB_U32 _hb_get_rev_uint32( const HB_BYTE * buf )
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

   static __inline__ void _hb_put_rev_uint32( HB_BYTE * buf, HB_U32 val )
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

#  if ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT )
      static __inline__ HB_U64 _hb_get_std_uint64( const HB_BYTE * buf )
      {
         HB_U64CAST u;
         memcpy( u.buf, buf, sizeof( u.buf ) );
         return u.val;
      }

      static __inline__ void _hb_put_std_uint64( HB_BYTE * buf, HB_U64 val )
      {
         HB_U64CAST u;
         u.val = val;
         memcpy( buf, u.buf, sizeof( u.buf ) );
      }

      static __inline__ HB_U64 _hb_get_rev_uint64( const HB_BYTE * buf )
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

      static __inline__ void _hb_put_rev_uint64( HB_BYTE * buf, HB_U64 val )
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

   static __inline__ double _hb_get_std_double( const HB_BYTE * buf )
   {
      HB_DBLCAST u;
      memcpy( u.buf, buf, sizeof( u.buf ) );
      return u.val;
   }

   static __inline__ void _hb_put_std_double( HB_BYTE * buf, double val )
   {
      HB_DBLCAST u;
      u.val = val;
      memcpy( buf, u.buf, sizeof( u.buf ) );
   }

   static __inline__ double _hb_get_rev_double( const HB_BYTE * buf )
   {
      HB_DBLCAST u;
#  if ( ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT ) ) && \
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

   static __inline__ void _hb_put_rev_double( HB_BYTE * buf, double val )
   {
      HB_DBLCAST u;
#  if ( ! defined( HB_LONG_LONG_OFF ) || defined( HB_ARCH_64BIT ) ) && \
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

#  define HB_GET_STD_DOUBLE( p )       _hb_get_std_double( ( const HB_BYTE * ) ( p ) )
#  define HB_GET_REV_DOUBLE( p )       _hb_get_rev_double( ( const HB_BYTE * ) ( p ) )
#  define HB_PUT_STD_DOUBLE( p, d )    _hb_put_std_double( ( HB_BYTE * ) ( p ), d )
#  define HB_PUT_REV_DOUBLE( p, d )    _hb_put_rev_double( ( HB_BYTE * ) ( p ), d )

#  if defined( HB_BIG_ENDIAN )

#     define HB_GET_BE_UINT16( p )        _hb_get_std_uint16( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT16( p, w )     _hb_put_std_uint16( ( HB_BYTE * ) ( p ), w )
#     define HB_GET_BE_UINT32( p )        _hb_get_std_uint32( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT32( p, l )     _hb_put_std_uint32( ( HB_BYTE * ) ( p ), l )
#     define HB_GET_BE_UINT64( p )        _hb_get_std_uint64( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT64( p, q )     _hb_put_std_uint64( ( HB_BYTE * ) ( p ), q )

#     define HB_GET_LE_UINT16( p )        _hb_get_rev_uint16( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT16( p, w )     _hb_put_rev_uint16( ( HB_BYTE * ) ( p ), w )
#     define HB_GET_LE_UINT32( p )        _hb_get_rev_uint32( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT32( p, l )     _hb_put_rev_uint32( ( HB_BYTE * ) ( p ), l )
#     define HB_GET_LE_UINT64( p )        _hb_get_rev_uint64( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT64( p, q )     _hb_put_rev_uint64( ( HB_BYTE * ) ( p ), q )

#  else /* HB_LITTLE_ENDIAN */

#     define HB_GET_BE_UINT16( p )        _hb_get_rev_uint16( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT16( p, w )     _hb_put_rev_uint16( ( HB_BYTE * ) ( p ), w )
#     define HB_GET_BE_UINT32( p )        _hb_get_rev_uint32( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT32( p, l )     _hb_put_rev_uint32( ( HB_BYTE * ) ( p ), l )
#     define HB_GET_BE_UINT64( p )        _hb_get_rev_uint64( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_BE_UINT64( p, q )     _hb_put_rev_uint64( ( HB_BYTE * ) ( p ), q )

#     define HB_GET_LE_UINT16( p )        _hb_get_std_uint16( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT16( p, w )     _hb_put_std_uint16( ( HB_BYTE * ) ( p ), w )
#     define HB_GET_LE_UINT32( p )        _hb_get_std_uint32( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT32( p, l )     _hb_put_std_uint32( ( HB_BYTE * ) ( p ), l )
#     define HB_GET_LE_UINT64( p )        _hb_get_std_uint64( ( const HB_BYTE * ) ( p ) )
#     define HB_PUT_LE_UINT64( p, q )     _hb_put_std_uint64( ( HB_BYTE * ) ( p ), q )

#  endif

#else /* ! __GNUC__ */

#  define HB_GET_STD_DOUBLE( p )    hb_get_std_double( ( const HB_BYTE * ) ( p ) )
#  define HB_GET_REV_DOUBLE( p )    hb_get_rev_double( ( const HB_BYTE * ) ( p ) )
#  define HB_PUT_REV_DOUBLE( p, d ) \
         do { \
            union { \
               double dbl; \
               HB_BYTE buffer[ 8 ]; \
            } u; \
            u.dbl = ( double ) ( d ); \
            (( HB_BYTE * )( p ))[ 7 ] = u.buffer[ 0 ]; \
            (( HB_BYTE * )( p ))[ 6 ] = u.buffer[ 1 ]; \
            (( HB_BYTE * )( p ))[ 5 ] = u.buffer[ 2 ]; \
            (( HB_BYTE * )( p ))[ 4 ] = u.buffer[ 3 ]; \
            (( HB_BYTE * )( p ))[ 3 ] = u.buffer[ 4 ]; \
            (( HB_BYTE * )( p ))[ 2 ] = u.buffer[ 5 ]; \
            (( HB_BYTE * )( p ))[ 1 ] = u.buffer[ 6 ]; \
            (( HB_BYTE * )( p ))[ 0 ] = u.buffer[ 7 ]; \
         } while( 0 )
#  define HB_PUT_STD_DOUBLE( p, d ) \
         do { \
            union { \
               double dbl; \
               HB_BYTE buffer[ 8 ]; \
            } u; \
            u.dbl = ( double ) ( d ); \
            (( HB_BYTE * )( p ))[ 0 ] = u.buffer[ 0 ]; \
            (( HB_BYTE * )( p ))[ 1 ] = u.buffer[ 1 ]; \
            (( HB_BYTE * )( p ))[ 2 ] = u.buffer[ 2 ]; \
            (( HB_BYTE * )( p ))[ 3 ] = u.buffer[ 3 ]; \
            (( HB_BYTE * )( p ))[ 4 ] = u.buffer[ 4 ]; \
            (( HB_BYTE * )( p ))[ 5 ] = u.buffer[ 5 ]; \
            (( HB_BYTE * )( p ))[ 6 ] = u.buffer[ 6 ]; \
            (( HB_BYTE * )( p ))[ 7 ] = u.buffer[ 7 ]; \
         } while( 0 )

#  if ! defined( HB_STRICT_ALIGNMENT ) && defined( HB_LITTLE_ENDIAN )

   #define HB_GET_LE_UINT16( p )    ( *( const HB_U16 * )( p ) )
   #define HB_PUT_LE_UINT16( p, w ) ( *( HB_U16 * )( p ) = ( HB_U16 ) ( w ) )
   #define HB_GET_LE_UINT32( p )    ( *( const HB_U32 * )( p ) )
   #define HB_PUT_LE_UINT32( p, l ) ( *( HB_U32 * )( p ) = ( HB_U32 ) ( l ) )
   #define HB_GET_LE_UINT64( p )    ( *( const HB_U64 * )( p ) )
   #define HB_PUT_LE_UINT64( p, q ) ( *( HB_U64 * )( p ) = ( HB_U64 ) ( q ) )

#  else

   #define HB_GET_LE_UINT16( p )    ( ( HB_U16 ) \
                                      ( ( ( HB_U16 ) (( const HB_BYTE * )( p ))[0] ) | \
                                        ( ( HB_U16 ) (( const HB_BYTE * )( p ))[1] <<  8 ) ) )
   #define HB_GET_LE_UINT32( p )    ( ( HB_U32 ) \
                                      ( ( ( HB_U32 ) (( const HB_BYTE * )( p ))[0] ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[2] << 16 ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[3] << 24 ) ) )
   #define HB_GET_LE_UINT64( p )    ( ( HB_U64 ) \
                                      ( ( ( HB_U64 ) (( const HB_BYTE * )( p ))[0] ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[2] << 16 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[3] << 24 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[4] << 32 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[5] << 40 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[6] << 48 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[7] << 56 ) ) )

   #define HB_PUT_LE_UINT16( p, w )    do { \
                                         (( HB_BYTE * )( p ))[0] = ( HB_BYTE )( w ); \
                                         (( HB_BYTE * )( p ))[1] = ( HB_BYTE )( (w) >>  8 ); \
                                       } while( 0 )
   #define HB_PUT_LE_UINT32( p, l )    do { \
                                         (( HB_BYTE * )( p ))[0] = ( HB_BYTE )( l ); \
                                         (( HB_BYTE * )( p ))[1] = ( HB_BYTE )( (l) >>  8 ); \
                                         (( HB_BYTE * )( p ))[2] = ( HB_BYTE )( (l) >> 16 ); \
                                         (( HB_BYTE * )( p ))[3] = ( HB_BYTE )( (l) >> 24 ); \
                                       } while( 0 )
   #define HB_PUT_LE_UINT64( p, q )    do { \
                                         (( HB_BYTE * )( p ))[0] = ( HB_BYTE )( q ); \
                                         (( HB_BYTE * )( p ))[1] = ( HB_BYTE )( (q) >>  8 ); \
                                         (( HB_BYTE * )( p ))[2] = ( HB_BYTE )( (q) >> 16 ); \
                                         (( HB_BYTE * )( p ))[3] = ( HB_BYTE )( (q) >> 24 ); \
                                         (( HB_BYTE * )( p ))[4] = ( HB_BYTE )( (q) >> 32 ); \
                                         (( HB_BYTE * )( p ))[5] = ( HB_BYTE )( (q) >> 40 ); \
                                         (( HB_BYTE * )( p ))[6] = ( HB_BYTE )( (q) >> 48 ); \
                                         (( HB_BYTE * )( p ))[7] = ( HB_BYTE )( (q) >> 56 ); \
                                       } while( 0 )
#  endif

#  if ! defined( HB_STRICT_ALIGNMENT ) && defined( HB_BIG_ENDIAN )

   #define HB_GET_BE_UINT16( p )    ( *( const HB_U16 * )( p ) )
   #define HB_PUT_BE_UINT16( p, w ) ( *( HB_U16 * )( p ) = ( HB_U16 ) ( w ) )
   #define HB_GET_BE_UINT32( p )    ( *( const HB_U32 * )( p ) )
   #define HB_PUT_BE_UINT32( p, l ) ( *( HB_U32 * )( p ) = ( HB_U32 ) ( l ) )
   #define HB_GET_BE_UINT64( p )    ( *( const HB_U64 * )( p ) )
   #define HB_PUT_BE_UINT64( p, q ) ( *( HB_U64 * )( p ) = ( HB_U64 ) ( q ) )

#  else

   #define HB_GET_BE_UINT16( p )    ( ( HB_U16 ) \
                                      ( ( ( HB_U16 ) (( const HB_BYTE * )( p ))[0] << 8 ) | \
                                        ( ( HB_U16 ) (( const HB_BYTE * )( p ))[1] ) ) )
   #define HB_GET_BE_UINT32( p )    ( ( HB_U32 ) \
                                      ( ( ( HB_U32 ) (( const HB_BYTE * )( p ))[0] << 24 ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[1] << 16 ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[2] <<  8 ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[3] ) ) )
   #define HB_GET_BE_UINT64( p )    ( ( HB_U64 ) \
                                      ( ( ( HB_U64 ) (( const HB_BYTE * )( p ))[0] << 56 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[1] << 48 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[2] << 40 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[3] << 32 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[4] << 24 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[5] << 16 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[6] <<  8 ) | \
                                        ( ( HB_U64 ) (( const HB_BYTE * )( p ))[7] ) ) )

   #define HB_PUT_BE_UINT16( p, w )    do { \
                                         (( HB_BYTE * )( p ))[0] = ( HB_BYTE )( (w) >>  8 ); \
                                         (( HB_BYTE * )( p ))[1] = ( HB_BYTE )( w ); \
                                       } while( 0 )
   #define HB_PUT_BE_UINT32( p, l )    do { \
                                         (( HB_BYTE * )( p ))[0] = ( HB_BYTE )( (l) >> 24 ); \
                                         (( HB_BYTE * )( p ))[1] = ( HB_BYTE )( (l) >> 16 ); \
                                         (( HB_BYTE * )( p ))[2] = ( HB_BYTE )( (l) >>  8 ); \
                                         (( HB_BYTE * )( p ))[3] = ( HB_BYTE )( l ); \
                                       } while( 0 )
   #define HB_PUT_BE_UINT64( p, q )    do { \
                                         (( HB_BYTE * )( p ))[0] = ( HB_BYTE )( (q) >> 56 ); \
                                         (( HB_BYTE * )( p ))[1] = ( HB_BYTE )( (q) >> 48 ); \
                                         (( HB_BYTE * )( p ))[2] = ( HB_BYTE )( (q) >> 40 ); \
                                         (( HB_BYTE * )( p ))[3] = ( HB_BYTE )( (q) >> 32 ); \
                                         (( HB_BYTE * )( p ))[4] = ( HB_BYTE )( (q) >> 24 ); \
                                         (( HB_BYTE * )( p ))[5] = ( HB_BYTE )( (q) >> 16 ); \
                                         (( HB_BYTE * )( p ))[6] = ( HB_BYTE )( (q) >>  8 ); \
                                         (( HB_BYTE * )( p ))[7] = ( HB_BYTE )( q ); \
                                       } while( 0 )
#  endif

#endif /* ! __GNUC__ */

/*
 * HB_FORCE_IEEE754_DOUBLE will can be used on platforms which use different
 * double format and we want to force storing double number as IEEE754
 * double value for sharing binary data (f.e. PCODE in .hrb files or CDX
 * indexes or DBFs with "B" fields.
 */
#if defined( HB_FORCE_IEEE754_DOUBLE )

#  define HB_GET_LE_DOUBLE( p )     hb_get_ieee754( ( const HB_BYTE * ) ( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  hb_put_ieee754( ( HB_BYTE * ) ( p ), ( d ) )
#  define HB_DBL2ORD( d, o )        hb_put_ord_ieee754( ( o ), *( d ) )
#  define HB_ORD2DBL( o, d )  do { \
                                 *( d ) = hb_get_ord_ieee754( ( const HB_BYTE * ) ( o ) ); \
                              } while( 0 )

#elif defined( HB_BIG_ENDIAN )

#  define HB_GET_LE_DOUBLE( p )     HB_GET_REV_DOUBLE( ( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  HB_PUT_REV_DOUBLE( ( p ), ( d ) )

#elif defined( HB_STRICT_ALIGNMENT ) || defined( __GNUC__ )

#  define HB_GET_LE_DOUBLE( p )     HB_GET_STD_DOUBLE( ( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  HB_PUT_STD_DOUBLE( ( p ), ( d ) )

#else

#  define HB_GET_LE_DOUBLE( p )     ( *( const double * )( p ) )
#  define HB_PUT_LE_DOUBLE( p, d )  ( *( double * )( p ) = ( double ) ( d ) )

#endif

#if ! defined( HB_FORCE_IEEE754_DOUBLE )
#  if defined( HB_BIG_ENDIAN )

   #define HB_ORD2DBL( o, d )       do { \
      if ( ( ( const HB_BYTE * ) ( o ) )[ 0 ] & 0x80 ) { \
         ( ( HB_BYTE * ) ( d ) )[ 0 ] = ( ( const HB_BYTE * ) ( o ) )[ 0 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 1 ] = ( ( const HB_BYTE * ) ( o ) )[ 1 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 2 ] = ( ( const HB_BYTE * ) ( o ) )[ 2 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 3 ] = ( ( const HB_BYTE * ) ( o ) )[ 3 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 4 ] = ( ( const HB_BYTE * ) ( o ) )[ 4 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 5 ] = ( ( const HB_BYTE * ) ( o ) )[ 5 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 6 ] = ( ( const HB_BYTE * ) ( o ) )[ 6 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 7 ] = ( ( const HB_BYTE * ) ( o ) )[ 7 ] ^ ( HB_BYTE ) 0x80; \
      } else { \
         ( ( HB_BYTE * ) ( d ) )[ 0 ] = ( ( const HB_BYTE * ) ( o ) )[ 0 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 1 ] = ( ( const HB_BYTE * ) ( o ) )[ 1 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 2 ] = ( ( const HB_BYTE * ) ( o ) )[ 2 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 3 ] = ( ( const HB_BYTE * ) ( o ) )[ 3 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 4 ] = ( ( const HB_BYTE * ) ( o ) )[ 4 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 5 ] = ( ( const HB_BYTE * ) ( o ) )[ 5 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 6 ] = ( ( const HB_BYTE * ) ( o ) )[ 6 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 7 ] = ( ( const HB_BYTE * ) ( o ) )[ 7 ] ^ ( HB_BYTE ) 0xFF; \
      } } while( 0 )

   #define HB_DBL2ORD( d, o )       do { \
      if ( *( d ) >= 0.0 ) { \
         if( *( d ) == -0.0 ) *( d ) = 0.0; \
         ( ( HB_BYTE * ) ( o ) )[ 0 ] = ( ( const HB_BYTE * ) ( d ) )[ 0 ] ^ ( HB_BYTE ) 0x80; \
         ( ( HB_BYTE * ) ( o ) )[ 1 ] = ( ( const HB_BYTE * ) ( d ) )[ 1 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 2 ] = ( ( const HB_BYTE * ) ( d ) )[ 2 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 3 ] = ( ( const HB_BYTE * ) ( d ) )[ 3 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 4 ] = ( ( const HB_BYTE * ) ( d ) )[ 4 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 5 ] = ( ( const HB_BYTE * ) ( d ) )[ 5 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 6 ] = ( ( const HB_BYTE * ) ( d ) )[ 6 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 7 ] = ( ( const HB_BYTE * ) ( d ) )[ 7 ]; \
      } else { \
         ( ( HB_BYTE * ) ( o ) )[ 0 ] = ( ( const HB_BYTE * ) ( d ) )[ 0 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 1 ] = ( ( const HB_BYTE * ) ( d ) )[ 1 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 2 ] = ( ( const HB_BYTE * ) ( d ) )[ 2 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 3 ] = ( ( const HB_BYTE * ) ( d ) )[ 3 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 4 ] = ( ( const HB_BYTE * ) ( d ) )[ 4 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 5 ] = ( ( const HB_BYTE * ) ( d ) )[ 5 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 6 ] = ( ( const HB_BYTE * ) ( d ) )[ 6 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 7 ] = ( ( const HB_BYTE * ) ( d ) )[ 7 ] ^ ( HB_BYTE ) 0xFF; \
      } } while( 0 )

#  else /* HB_LITTLE_ENDIAN */

   #define HB_ORD2DBL( o, d )       do { \
      if ( ( ( const HB_BYTE * ) ( o ) )[ 0 ] & 0x80 ) { \
         ( ( HB_BYTE * ) ( d ) )[ 0 ] = ( ( const HB_BYTE * ) ( o ) )[ 7 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 1 ] = ( ( const HB_BYTE * ) ( o ) )[ 6 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 2 ] = ( ( const HB_BYTE * ) ( o ) )[ 5 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 3 ] = ( ( const HB_BYTE * ) ( o ) )[ 4 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 4 ] = ( ( const HB_BYTE * ) ( o ) )[ 3 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 5 ] = ( ( const HB_BYTE * ) ( o ) )[ 2 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 6 ] = ( ( const HB_BYTE * ) ( o ) )[ 1 ]; \
         ( ( HB_BYTE * ) ( d ) )[ 7 ] = ( ( const HB_BYTE * ) ( o ) )[ 0 ] ^ ( HB_BYTE ) 0x80; \
      } else { \
         ( ( HB_BYTE * ) ( d ) )[ 0 ] = ( ( const HB_BYTE * ) ( o ) )[ 7 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 1 ] = ( ( const HB_BYTE * ) ( o ) )[ 6 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 2 ] = ( ( const HB_BYTE * ) ( o ) )[ 5 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 3 ] = ( ( const HB_BYTE * ) ( o ) )[ 4 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 4 ] = ( ( const HB_BYTE * ) ( o ) )[ 3 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 5 ] = ( ( const HB_BYTE * ) ( o ) )[ 2 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 6 ] = ( ( const HB_BYTE * ) ( o ) )[ 1 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( d ) )[ 7 ] = ( ( const HB_BYTE * ) ( o ) )[ 0 ] ^ ( HB_BYTE ) 0xFF; \
      } } while( 0 )

   #define HB_DBL2ORD( d, o )       do { \
      if ( *( d ) >= 0.0 ) { \
         if( *( d ) == -0.0 ) *( d ) = 0.0; \
         ( ( HB_BYTE * ) ( o ) )[ 0 ] = ( ( const HB_BYTE * ) ( d ) )[ 7 ] ^ ( HB_BYTE ) 0x80; \
         ( ( HB_BYTE * ) ( o ) )[ 1 ] = ( ( const HB_BYTE * ) ( d ) )[ 6 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 2 ] = ( ( const HB_BYTE * ) ( d ) )[ 5 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 3 ] = ( ( const HB_BYTE * ) ( d ) )[ 4 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 4 ] = ( ( const HB_BYTE * ) ( d ) )[ 3 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 5 ] = ( ( const HB_BYTE * ) ( d ) )[ 2 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 6 ] = ( ( const HB_BYTE * ) ( d ) )[ 1 ]; \
         ( ( HB_BYTE * ) ( o ) )[ 7 ] = ( ( const HB_BYTE * ) ( d ) )[ 0 ]; \
      } else { \
         ( ( HB_BYTE * ) ( o ) )[ 0 ] = ( ( const HB_BYTE * ) ( d ) )[ 7 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 1 ] = ( ( const HB_BYTE * ) ( d ) )[ 6 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 2 ] = ( ( const HB_BYTE * ) ( d ) )[ 5 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 3 ] = ( ( const HB_BYTE * ) ( d ) )[ 4 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 4 ] = ( ( const HB_BYTE * ) ( d ) )[ 3 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 5 ] = ( ( const HB_BYTE * ) ( d ) )[ 2 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 6 ] = ( ( const HB_BYTE * ) ( d ) )[ 1 ] ^ ( HB_BYTE ) 0xFF; \
         ( ( HB_BYTE * ) ( o ) )[ 7 ] = ( ( const HB_BYTE * ) ( d ) )[ 0 ] ^ ( HB_BYTE ) 0xFF; \
      } } while( 0 )
#  endif

#endif /* ! defined( HB_FORCE_IEEE754_DOUBLE ) */


/* Now the rest of endian macros */

/*
 * 24 bit integers are not directly supported by any processor we used so far
 * so we always have to build them from HB_BYTEs and cannot use C casting
 */
#define HB_GET_LE_INT24( p )        ( ( HB_I32 ) \
                                      ( ( ( HB_I32 ) (( const HB_BYTE * )( p ))[0] ) | \
                                        ( ( HB_I32 ) (( const HB_BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( HB_I32 ) (( const HB_BYTE * )( p ))[2] << 16 ) | \
                                        ( ( HB_I32 ) ((( const HB_BYTE * )( p ))[2] & 0x80 ? 0xFF : 0x00 ) << 24 ) ) )
#define HB_GET_LE_UINT24( p )       ( ( HB_U32 ) \
                                      ( ( ( HB_U32 ) (( const HB_BYTE * )( p ))[0] ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[2] << 16 ) ) )
#define HB_PUT_LE_UINT24( p, u )    do { \
                                       (( HB_BYTE * )( p ))[0] = ( HB_BYTE )( u ); \
                                       (( HB_BYTE * )( p ))[1] = ( HB_BYTE )( (u) >>  8 ); \
                                       (( HB_BYTE * )( p ))[2] = ( HB_BYTE )( (u) >> 16 ); \
                                    } while( 0 )
#define HB_GET_BE_INT24( p )        ( ( HB_I32 ) \
                                      ( ( ( HB_I32 ) (( const HB_BYTE * )( p ))[2] ) | \
                                        ( ( HB_I32 ) (( const HB_BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( HB_I32 ) (( const HB_BYTE * )( p ))[0] << 16 ) | \
                                        ( ( HB_I32 ) ((( const HB_BYTE * )( p ))[0] & 0x80 ? 0xFF : 0x00 ) << 24 ) ) )
#define HB_GET_BE_UINT24( p )       ( ( HB_U32 ) \
                                      ( ( ( HB_U32 ) (( const HB_BYTE * )( p ))[2] ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[1] <<  8 ) | \
                                        ( ( HB_U32 ) (( const HB_BYTE * )( p ))[0] << 16 ) ) )
#define HB_PUT_BE_UINT24( p, u )    do { \
                                       (( HB_BYTE * )( p ))[2] = ( HB_BYTE )( u ); \
                                       (( HB_BYTE * )( p ))[1] = ( HB_BYTE )( (u) >>  8 ); \
                                       (( HB_BYTE * )( p ))[0] = ( HB_BYTE )( (u) >> 16 ); \
                                    } while( 0 )


#define HB_GET_LE_INT16( p )        (( HB_I16 ) HB_GET_LE_UINT16( p ))
#define HB_GET_LE_INT32( p )        (( HB_I32 ) HB_GET_LE_UINT32( p ))
#define HB_GET_LE_INT64( p )        (( HB_I64 ) HB_GET_LE_UINT64( p ))

#define HB_PCODE_MKSHORT( p )       (( HB_SHORT )     HB_GET_LE_INT16( p ))
#define HB_PCODE_MKUSHORT( p )      (( HB_USHORT )    HB_GET_LE_UINT16( p ))
#define HB_PCODE_MKLONG( p )        (( HB_LONG )      HB_GET_LE_INT32( p ))
#define HB_PCODE_MKULONG( p )       (( HB_ULONG )     HB_GET_LE_UINT32( p ))
#define HB_PCODE_MKLONGLONG( p )    (( HB_LONGLONG )  HB_GET_LE_INT64( p ))
#define HB_PCODE_MKULONGLONG( p )   (( HB_ULONGLONG ) HB_GET_LE_UINT64( p ))
#define HB_PCODE_MKDOUBLE( p )      (( double )       HB_GET_LE_DOUBLE( p ))
#define HB_PCODE_MKINT24( p )       (( HB_LONG )      HB_GET_LE_INT24( p ))
#define HB_PCODE_MKUINT24( p )      (( HB_ULONG )     HB_GET_LE_UINT24( p ))

/*
 * Below are hacked version of INT64 macros which operates on double
 * when INT64 is not supported - they are necessary for PCODE and
 * database access
 */
#if defined( HB_LONG_LONG_OFF ) && ! defined( HB_ARCH_64BIT )
   #undef HB_GET_LE_INT64
   #undef HB_GET_LE_UINT64
   #undef HB_PUT_LE_UINT64
   #undef HB_PCODE_MKLONGLONG
   #undef HB_PCODE_MKULONGLONG
   #undef HB_DBL_LIM_INT64
   #define UINT64_MAXDBL               ( ( ( double ) UINT32_MAX + 1.0 ) * \
                                         ( ( double ) UINT32_MAX + 1.0 ) - 1.0 )
   #define HB_GET_LE_INT64( p )        hb_get_le_int64( ( const HB_BYTE * ) ( p ) )
   #define HB_GET_LE_UINT64( p )       hb_get_le_uint64( ( const HB_BYTE * ) ( p ) )
   #define HB_PUT_LE_UINT64( p, d )    hb_put_le_uint64( ( HB_BYTE * ) ( p ), \
                                                         ( double ) ( d ) )
   #define HB_PCODE_MKLONGLONG( p )    ( ( double ) HB_GET_LE_INT64( p ) )
   #define HB_PCODE_MKULONGLONG( p )   ( ( double ) HB_GET_LE_UINT64( p ) )
   #define HB_DBL_LIM_INT64( d )       ( ( HB_MAXDBL ) -UINT64_MAXDBL / 2 - 1 <= \
                                         ( HB_MAXDBL ) ( d ) && ( HB_MAXDBL ) ( d ) <= \
                                         ( HB_MAXDBL ) UINT64_MAXDBL / 2 )
#endif

#define HB_MACRO2STRING( macro )    HB_MACRO2STRING_( macro )
#define HB_MACRO2STRING_( macro )   #macro

#define HB_MACRONAME_JOIN( m1, m2 )       HB_MACRONAME_JOIN_( m1, m2 )
#define HB_MACRONAME_JOIN_( m1, m2 )      m1 ## m2

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
    ( defined( __GNUC__ ) && ! defined( __DJGPP__ ) && ! defined( HB_OS_OS2_GCC ) )
   #define HB_START_PROC_STRICT
#endif

#if defined( __WATCOMC__ ) || defined( __DMC__ ) || \
    defined( _MSC_VER ) || defined( __POCC__ )
   #define HB_DLL_ENTRY_POINT    DllMain
#else
   #define HB_DLL_ENTRY_POINT    DllEntryPoint
#endif

#define HB_EXTERN extern

#if defined( __RSXNT__ )
   /* RSXNT does not support any type of export keyword.
      Exported (i.e., public) names can be obtained via
      the emxexp utility and the output can be used for
      input to a module definition file. See emxdev.doc
      in the RSXNT doc/ directory for more information. */
   #define HB_EXPORT_ATTR

#elif defined( __GNUC__ ) && defined( HB_OS_WIN )
   #define HB_EXPORT_ATTR     __attribute__ (( dllexport ))

#elif defined( __GNUC__ ) && defined( HB_OS_LINUX ) && __GNUC__ >= 3
   #define HB_EXPORT_ATTR     __attribute__ ((visibility ("default")))

#elif defined( __BORLANDC__ )
   #define HB_EXPORT_ATTR     __declspec( dllexport )

#elif defined( __WATCOMC__ )
   #define HB_EXPORT_ATTR     __declspec( dllexport )

#elif defined( ASANLM ) || defined( ASANT )
   #define HB_EXPORT_ATTR

#elif defined( HB_OS_WIN )
   #define HB_EXPORT_ATTR     _declspec( dllexport )

#else
   #define HB_EXPORT_ATTR

#endif

#if defined( HB_DYNLIB )
   #define HB_EXPORT    HB_EXPORT_ATTR
#else
   #define HB_EXPORT
#endif

#define HB_EXPORT_INT HB_EXPORT

#if defined( __RSXNT__ )
   /* RSXNT does not support any type of export keyword.
      Exported (i.e., public) names can be obtained via
      the emxexp utility and the output can be used for
      input to a module definition file. See emxdev.doc
      in the RSXNT doc/ directory for more information. */
   #define HB_IMPORT_ATTR

#elif defined( __GNUC__ ) && defined( HB_OS_WIN )
   #define HB_IMPORT_ATTR     __attribute__ (( dllimport ))

#elif defined( __BORLANDC__ )
   #define HB_IMPORT_ATTR     __declspec( dllimport )

#elif defined( __WATCOMC__ )
   #define HB_IMPORT_ATTR     __declspec( dllimport )

#elif defined( ASANLM ) || defined( ASANT )
   #define HB_IMPORT_ATTR

#elif defined( HB_OS_WIN )
   #define HB_IMPORT_ATTR     _declspec( dllimport )

#else
   #define HB_IMPORT_ATTR

#endif

#define HB_IMPORT    HB_IMPORT_ATTR

#if defined( HB_OS_WIN )

   /* Features provided for Windows builds only */

   HB_EXTERN_BEGIN
      extern HB_EXPORT int       hb_wctomblen( const wchar_t * szText );
      extern HB_EXPORT wchar_t * hb_mbtowc( const char * srcA );
      extern HB_EXPORT char *    hb_wctomb( const wchar_t * srcW );
      extern HB_EXPORT wchar_t * hb_mbntowc( const char * srcA, HB_SIZE nLen );
      extern HB_EXPORT char *    hb_wcntomb( const wchar_t * srcW, HB_SIZE nLen );
      extern HB_EXPORT void      hb_wcntombcpy( char * dstA, const wchar_t * srcW, HB_SIZE nLen );
      extern HB_EXPORT void      hb_mbntowccpy( wchar_t * dstW, const char * srcA, HB_SIZE nLen );
   HB_EXTERN_END

#endif

#if defined( HB_OS_WIN ) && defined( HB_LEGACY_LEVEL4 )
   #if defined( UNICODE )
      #define HB_TCHAR_COPYTO( d, s, l )     hb_mbntowccpy( d, s, l )
      #define HB_TCHAR_COPYFROM( d, s, l )   hb_wcntombcpy( d, s, l )
      #define HB_TCHAR_CONVTO( s )           hb_mbtowc( s )
      #define HB_TCHAR_CONVFROM( s )         hb_wctomb( s )
      #define HB_TCHAR_FREE( s )             hb_xfree( s )
   #else
      #define HB_TCHAR_COPYTO( d, s, l )     hb_strncpy( d, s, l )
      #define HB_TCHAR_COPYFROM( d, s, l )   hb_strncpy( d, s, l )
      #define HB_TCHAR_CONVTO( s )           ( ( char * )( s ) )
      #define HB_TCHAR_CONVFROM( s )         ( ( char * )( s ) )
      #define HB_TCHAR_FREE( s )             HB_SYMBOL_UNUSED( s )
   #endif
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

#if defined( __cplusplus ) && ! defined( HB_FUNC_USE_DECORATION )
   #define HB_EXTERN_C_ HB_EXTERN_C
   #define HB_EXTERN_
#else
   #define HB_EXTERN_C_
   #define HB_EXTERN_   extern
#endif

#define HB_FUNC_EXEC( funcname )   HB_FUN_##funcname()
#define HB_FUNC( funcname )        HB_EXTERN_C_ HB_EXPORT HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_EXTERN( funcname ) HB_EXTERN_C_ HB_EXTERN_ HARBOUR HB_EXPORT HB_FUN_##funcname ( void )
#define HB_FUNC_STATIC( funcname ) static HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_INIT( funcname )   static HARBOUR HB_FUN_init_##funcname ( void )
#define HB_FUNC_EXIT( funcname )   static HARBOUR HB_FUN_exit_##funcname ( void )
#define HB_FUNC_INITSTATICS()      static HARBOUR hb_INITSTATICS( void )
#define HB_FUNC_INITLINES()        static HARBOUR hb_INITLINES( void )
#define HB_FUNC_TRANSLATE( w, o )  HB_FUNC_EXTERN( o ); HB_FUNC( w ) { HB_FUNC_EXEC( o ); }


#if defined( HB_FUNC_CALLCONV )
   #define HARBOUR void HB_FUNC_CALLCONV
#else
   #define HARBOUR void
#endif

HB_EXTERN_BEGIN
   typedef HARBOUR ( * PHB_FUNC )( void );
HB_EXTERN_END

typedef HB_SHORT HB_SYMBOLSCOPE;   /* stores symbol's scope */

typedef unsigned char HB_ATTR;
typedef int           HB_COLOR;

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

#define HB_CHAR_HARD1           HB_CHAR_CR
#define HB_CHAR_HARD2           HB_CHAR_LF

#define HB_CHAR_SOFT1           '\x8D'  /* 141 */
#define HB_CHAR_SOFT2           HB_CHAR_LF

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
