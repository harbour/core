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

#include "hbsetup.h"
#include "hbtrace.h"

/*
#define HB_CLIPPER_INT_ITEMS
#define HB_LONG_LONG_OFF
*/

#if defined( _WIN64 )
#  undef HB_LONG_LONG_OFF
#  define HB_STRICT_ALIGNMENT
#endif

#if defined( HB_OS_WIN_32 )
   #if !defined( HB_WIN32_IO_OFF )
      #define HB_WIN32_IO
   #endif
   #if defined( HB_WIN32_IO ) && !defined( HB_OS_WIN_32_USED )
      /* disabled to avoid problems with windows.h */
      /* #define HB_OS_WIN_32_USED */
   #endif
#else
   #undef HB_WIN32_IO
   #undef HB_OS_WIN_32_USED
#endif

/* Include windows.h if applicable and requested */
#if defined( HB_OS_WIN_32_USED ) && defined( HB_OS_WIN_32 )

   #define WIN32_LEAN_AND_MEAN
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

   #include <os2.h>
   #undef INT
   #undef UINT
   #define HB_DONT_DEFINE_BASIC_TYPES

#elif defined( HB_OS_DOS )

   #include <dos.h>

   #if defined(__WATCOMC__) && defined(__386__) && !defined(__WINDOWS_386__)
      #define HB_DOS_INT86 int386
      #define HB_DOS_INT86X int386x
      #define HB_XREGS w
   #elif defined(__RSX32__)
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

#endif

#if ! defined( HB_DONT_DEFINE_BASIC_TYPES )

   #undef BOOL                            /* boolean */
   typedef int BOOL;

   #undef UINT                            /* varies with platform */
   typedef unsigned int UINT;

   #undef BYTE                            /* 1 byte unsigned */
   typedef unsigned char BYTE;

   #undef SHORT                           /* 2 bytes signed */
   typedef short int SHORT;

   #undef USHORT                          /* 2 bytes unsigned */
   typedef unsigned short int USHORT;

   #undef LONG                            /* 4 or 8 bytes signed */
   typedef long LONG;

   #undef ULONG                           /* 4 or 8 bytes unsigned */
   typedef unsigned long ULONG;

   #undef FALSE
   #define FALSE  0
   #undef TRUE
   #define TRUE   (!0)

#ifndef HB_LONG_LONG_OFF
   #if ! defined(_WINNT_H)
      #if !defined(LONGLONG)
         #if defined(__GNUC__)
            typedef long long LONGLONG;
         #else
           typedef __int64 LONGLONG;
         #endif
      #endif
      #if !defined(ULONGLONG)
         #if defined(__GNUC__)
            typedef unsigned long long ULONGLONG;
         #else
            typedef unsigned __int64 ULONGLONG;
         #endif
      #endif
   #endif

   #ifdef __GNUC__
      #if defined(ULLONG_MAX)
         #define ULONGLONG_MAX      ULLONG_MAX
      #elif defined(ULONG_LONG_MAX)
         #define ULONGLONG_MAX      ULONG_LONG_MAX
      #else
         #define ULONGLONG_MAX      18446744073709551615ULL
      #endif
      #if defined(LLONG_MAX)
         #define LONGLONG_MAX       LLONG_MAX
      #elif defined(LONG_LONG_MAX)
         #define LONGLONG_MAX       LONG_LONG_MAX
      #else
         #define LONGLONG_MAX       9223372036854775807LL
      #endif
      #if defined(LLONG_MIN)
         #define LONGLONG_MIN       LLONG_MIN
      #elif defined(LONG_LONG_MIN)
         #define LONGLONG_MIN       LONG_LONG_MIN
      #else
         #define LONGLONG_MIN       (-LONGLONG_MAX - 1LL)
      #endif
   #else
      #if !defined(LONGLONG_MIN)
         #define LONGLONG_MIN       _I64_MIN
      #endif
      #if !defined(LONGLONG_MAX)
         #define LONGLONG_MAX       _I64_MAX
      #endif
      #if !defined(ULONGLONG_MAX)
         #define ULONGLONG_MAX      _UI64_MAX
      #endif
   #endif
#endif /* HB_LONG_LONG_OFF */

#endif /* HB_DONT_DEFINE_BASIC_TYPES */

/*
 * below are some hacks which don't have to be true on some machines
 * please update it if necessary
 */
#if ( ULONG_MAX > UINT_MAX && UINT_MAX > USHRT_MAX ) || defined( _WIN64 )
#  define HB_ARCH_64BIT
#elif ULONG_MAX == UINT_MAX && UINT_MAX > USHRT_MAX
#  define HB_ARCH_32BIT
#elif ULONG_MAX > UINT_MAX && UINT_MAX == USHRT_MAX
#  define HB_ARCH_16BIT
#endif

#if USHRT_MAX == 0xffff
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
#endif

#if UINT_MAX == 0xffffffff
#  if !defined( UINT32 )
      typedef UINT         UINT32;
#  endif
#  if !defined( INT32 )
      typedef int          INT32;
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
#elif ULONG_MAX == 0xffffffff
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

#if defined( HB_ARCH_64BIT ) && !defined( _WIN64 )
#   if !defined( UINT64 )
      typedef ULONG        UINT64;
#   endif
#   if !defined( INT64 )
      typedef LONG         INT64;
#   endif
#   if !defined( UINT64_MAX )
#     define UINT64_MAX    ULONG_MAX
#   endif
#   if !defined( INT64_MAX )
#     define INT64_MAX     LONG_MAX
#   endif
#   if !defined( INT64_MIN )
#     define INT64_MIN     LONG_MIN
#   endif
#elif !defined( HB_LONG_LONG_OFF )
#   if !defined( UINT64 )
      typedef ULONGLONG    UINT64;
#   endif
#   if !defined( INT64 )
      typedef LONGLONG     INT64;
#   endif
#   if !defined( UINT64_MAX )
#     define UINT64_MAX    ULONGLONG_MAX
#   endif
#   if !defined( INT64_MAX )
#     define INT64_MAX     LONGLONG_MAX
#   endif
#   if !defined( INT64_MIN )
#     define INT64_MIN     LONGLONG_MIN
#   endif
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
#define HB_DBL_LIM_INT24(d)   ( -8388608.0 <= (d) && (d) <= 8388607.0 )
#define HB_DBL_LIM_INT32(d)   ( INT32_MIN <= (d) && (d) <= INT32_MAX )
#define HB_DBL_LIM_INT64(d)   ( (HB_MAXDBL) INT64_MIN <= (HB_MAXDBL) (d) && (HB_MAXDBL) (d) <= (HB_MAXDBL) INT64_MAX )
#define HB_LIM_INT8(l)        ( -128 <= (l) && (l) <= 127 )
#define HB_LIM_INT16(l)       ( INT16_MIN <= (l) && (l) <= INT16_MAX )
#define HB_LIM_INT24(l)       ( -8388608L <= (l) && (l) <= 8388607L )
#define HB_LIM_INT32(l)       ( INT32_MIN <= (l) && (l) <= INT32_MAX )
#define HB_LIM_INT64(l)       ( INT64_MIN <= (l) && (l) <= INT64_MAX )

#if HB_LONG_MAX > 10000000000
#  define HB_LONG_LENGTH( l ) ( ( (l) <= -1000000000 || (l) >= HB_LL( 10000000000 ) ) ? 20 : 10 )
#else
#  define HB_LONG_LENGTH( l ) ( ( (l) <= -1000000000 ) ? 20 : 10 )
#endif
#if HB_INT_MIN <= -1000000000
#  define HB_INT_LENGTH( i )  ( ( (i) <= -1000000000 ) ? 20 : 10 )
#else
#  define HB_INT_LENGTH( i )  10
#endif
/* NOTE: Yes, -999999999.0 is right instead of -1000000000.0 [vszakats] */
/* This comment is from hb_vmNeg() - if it's true only in this case then
   the limit should be changed and this function fixed */
#define HB_DBL_LENGTH( d ) ( ( (d) >= 10000000000.0 || (d) <= -999999999.0 ) ? 20 : 10 )

/* uncomment this if you need strict Clipper compatibility */
/* #define PCODE_LONG_LIM(l)     HB_LIM_INT32( l ) */

/* #define PCODE_LONG_LIM(l)     HB_LIM_LONG( l ) */

/* type of HB_ITEM */
/* typedef USHORT HB_TYPE; */
typedef UINT32 HB_TYPE;

/* type of reference counter */
typedef unsigned long HB_COUNTER;

/* type for memory pointer diff */
#if defined( _WIN64 )
   typedef LONGLONG HB_PTRDIFF;
#else
   typedef long HB_PTRDIFF;
#endif

#ifdef HB_LONG_LONG_OFF
    typedef LONG HB_FOFFSET;
    /* we can add hack with double as work around what should
       effectively give 52bit file size limit */
#else
    typedef LONGLONG HB_FOFFSET;
#endif

/* maximum length of double number in decimal representation:
   log10(2^1024) ~ 308.25 */
#define HB_MAX_DOUBLE_LENGTH 320

/* This value is used to hack the double FL value in round/int
   operation - similar thing is done by CL5.3 - I do not know
   only the exact factor value but it should be close to this one.
   When HB_C52_STRICT is set this macro is not used.
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


#define HB_MAX( a, b )          ( ( ( a ) > ( b ) ) ? ( a ) : ( b ) )
#define HB_MIN( a, b )          ( ( ( a ) < ( b ) ) ? ( a ) : ( b ) )

#define HB_LOBYTE( w )          ( ( BYTE ) ( w ) )
#define HB_HIBYTE( w )          ( ( BYTE ) ( ( ( w ) >> 8 ) & 0xFF ) )
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

/*
 * It's a hack for MSC which doesn't support LL suffix for LONGLONG
 * numeric constant. This suffix is necessary for some compilers -
 * without it they cut the number to LONG
 */
#if defined( _MSC_VER ) || defined( __BORLANDC__ )
#  define HB_LL( num )           ((LONGLONG)num)
#else
#  define HB_LL( num )           num##LL
#endif

#ifndef PFLL
#  if defined( __BORLANDC__ ) || defined( _MSC_VER )
#     define PFLL    "I64"
#  elif defined( __LCC__ )
#     define PFLL    "ll"
#  else
#     define PFLL    "L"
#  endif
#endif
#ifndef PFHL
#  define PFHL    PFLL
#endif


#define HB_SWAP_UINT64( w )      ( ( UINT64 ) ( ( ( ( UINT64 ) ( w ) & HB_LL( 0x00000000000000FF ) ) << 56 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x000000000000FF00 ) ) << 40 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x0000000000FF0000 ) ) >> 24 ) | \
                                                ( ( ( UINT64 ) ( w ) & HB_LL( 0x00000000FF000000 ) ) >>  8 ) | \
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
#if defined( HB_OS_SUNOS ) || defined( HB_OS_HPUX )
#  if !defined( HB_STRICT_ALIGNMENT )
#     define HB_STRICT_ALIGNMENT
#  endif
#  if !defined( HB_ALLOC_ALIGNMENT ) || ( HB_ALLOC_ALIGNMENT + 1 == 1 )
#     define HB_ALLOC_ALIGNMENT     8
#  endif
#endif

/*
 * These macros are necessary for architectures which need
 * strict alignment for pointers.
 */
#if defined( HB_BIG_ENDIAN )
#  if defined( HB_ARCH_64BIT )
#     define   HB_PUT_LONG( p, v )  HB_PUT_BE_UINT64( p, ( UINT64 ) ( v ) )
#     define   HB_GET_LONG( p )     HB_GET_BE_UINT64( p )
#  else
#     define   HB_PUT_LONG( p, v )  HB_PUT_BE_UINT32( p, ( UINT32 ) ( v ) )
#     define   HB_GET_LONG( p )     HB_GET_BE_UINT32( p )
#  endif
#else
#  if defined( HB_ARCH_64BIT )
#     define   HB_PUT_LONG( p, v )  HB_PUT_LE_UINT64( p, ( UINT64 ) ( v ) )
#     define   HB_GET_LONG( p )     HB_GET_LE_UINT64( p )
#  else
#     define   HB_PUT_LONG( p, v )  HB_PUT_LE_UINT32( p, ( UINT32 ) ( v ) )
#     define   HB_GET_LONG( p )     HB_GET_LE_UINT32( p )
#  endif
#endif

#if !defined( HB_STRICT_ALIGNMENT )
#  define   HB_PUT_PTR( p, v )      do { *( void ** ) ( p ) = ( void * ) ( v ); } while ( 0 )
#  define   HB_GET_PTR( p )         ( *( void ** ) ( p ) )
#else
#  define   HB_PUT_PTR( p, v )      HB_PUT_LONG( p, v )
#  define   HB_GET_PTR( p )         ( ( void * ) HB_GET_LONG( p ) )
#endif


/* Now the rest of endian macros */
#if defined( HB_STRICT_ALIGNMENT ) || !defined( HB_LITTLE_ENDIAN )

   #define HB_GET_LE_UINT16( p )    ( ( UINT16 ) \
                                      ( ( UINT16 ) (( BYTE * )( p ))[0] | \
                                        ( UINT16 ) (( BYTE * )( p ))[1] <<  8 ) )
   #define HB_GET_LE_UINT32( p )    ( ( UINT32 ) \
                                      ( ( UINT32 ) (( BYTE * )( p ))[0] | \
                                        ( UINT32 ) (( BYTE * )( p ))[1] <<  8 | \
                                        ( UINT32 ) (( BYTE * )( p ))[2] << 16 | \
                                        ( UINT32 ) (( BYTE * )( p ))[3] << 24 ) )
   #define HB_GET_LE_UINT64( p )    ( ( UINT64 ) \
                                      ( ( UINT64 ) (( BYTE * )( p ))[0] | \
                                        ( UINT64 ) (( BYTE * )( p ))[1] <<  8 | \
                                        ( UINT64 ) (( BYTE * )( p ))[2] << 16 | \
                                        ( UINT64 ) (( BYTE * )( p ))[3] << 24 | \
                                        ( UINT64 ) (( BYTE * )( p ))[4] << 32 | \
                                        ( UINT64 ) (( BYTE * )( p ))[5] << 40 | \
                                        ( UINT64 ) (( BYTE * )( p ))[6] << 48 | \
                                        ( UINT64 ) (( BYTE * )( p ))[7] << 56 ) )

   #define HB_PUT_LE_UINT16( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( w ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (w) >>  8 ); \
                                       } while ( 0 )
   #define HB_PUT_LE_UINT32( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( w ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (w) >>  8 ); \
                                         (( BYTE * )( p ))[2] = ( BYTE )( (w) >> 16 ); \
                                         (( BYTE * )( p ))[3] = ( BYTE )( (w) >> 24 ); \
                                       } while ( 0 )
   #define HB_PUT_LE_UINT64( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( w ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (w) >>  8 ); \
                                         (( BYTE * )( p ))[2] = ( BYTE )( (w) >> 16 ); \
                                         (( BYTE * )( p ))[3] = ( BYTE )( (w) >> 24 ); \
                                         (( BYTE * )( p ))[4] = ( BYTE )( (w) >> 32 ); \
                                         (( BYTE * )( p ))[5] = ( BYTE )( (w) >> 40 ); \
                                         (( BYTE * )( p ))[6] = ( BYTE )( (w) >> 48 ); \
                                         (( BYTE * )( p ))[7] = ( BYTE )( (w) >> 56 ); \
                                       } while ( 0 )
#endif

#if defined( HB_STRICT_ALIGNMENT ) || !defined( HB_BIG_ENDIAN )

   #define HB_GET_BE_UINT16( p )    ( ( UINT16 ) \
                                      ( ( UINT16 ) (( BYTE * )( p ))[0] << 8 | \
                                        ( UINT16 ) (( BYTE * )( p ))[1] ) )
   #define HB_GET_BE_UINT32( p )    ( ( UINT32 ) \
                                      ( ( UINT32 ) (( BYTE * )( p ))[0] << 24 | \
                                        ( UINT32 ) (( BYTE * )( p ))[1] << 16 | \
                                        ( UINT32 ) (( BYTE * )( p ))[2] <<  8 | \
                                        ( UINT32 ) (( BYTE * )( p ))[3] ) )
   #define HB_GET_BE_UINT64( p )    ( ( UINT64 ) \
                                      ( ( UINT64 ) (( BYTE * )( p ))[0] << 56 | \
                                        ( UINT64 ) (( BYTE * )( p ))[1] << 48 | \
                                        ( UINT64 ) (( BYTE * )( p ))[2] << 40 | \
                                        ( UINT64 ) (( BYTE * )( p ))[3] << 32 | \
                                        ( UINT64 ) (( BYTE * )( p ))[4] << 24 | \
                                        ( UINT64 ) (( BYTE * )( p ))[5] << 16 | \
                                        ( UINT64 ) (( BYTE * )( p ))[6] <<  8 | \
                                        ( UINT64 ) (( BYTE * )( p ))[7] ) )

   #define HB_PUT_BE_UINT16( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( (w) >>  8 ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( w ); \
                                       } while ( 0 )
   #define HB_PUT_BE_UINT32( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( (w) >> 24 ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (w) >> 16 ); \
                                         (( BYTE * )( p ))[2] = ( BYTE )( (w) >>  8 ); \
                                         (( BYTE * )( p ))[3] = ( BYTE )( w ); \
                                       } while ( 0 )
   #define HB_PUT_BE_UINT64( p, w )    do { \
                                         (( BYTE * )( p ))[0] = ( BYTE )( (w) >> 56 ); \
                                         (( BYTE * )( p ))[1] = ( BYTE )( (w) >> 48 ); \
                                         (( BYTE * )( p ))[2] = ( BYTE )( (w) >> 40 ); \
                                         (( BYTE * )( p ))[3] = ( BYTE )( (w) >> 32 ); \
                                         (( BYTE * )( p ))[4] = ( BYTE )( (w) >> 24 ); \
                                         (( BYTE * )( p ))[5] = ( BYTE )( (w) >> 16 ); \
                                         (( BYTE * )( p ))[6] = ( BYTE )( (w) >>  8 ); \
                                         (( BYTE * )( p ))[7] = ( BYTE )( w ); \
                                       } while ( 0 )
#endif

/*
 * 24 bit integers are not directly supported by any processor we used so far
 * so we always have to build them from BYTEs and cannot use C casting
 */
#define HB_GET_LE_INT24( p )        ( ( INT32 ) \
                                      ( ( INT32 ) (( BYTE * )( p ))[0] | \
                                        ( INT32 ) (( BYTE * )( p ))[1] <<  8 | \
                                        ( INT32 ) (( BYTE * )( p ))[2] << 16 | \
                                        ( INT32 ) ((( BYTE * )( p ))[2] & 0x80 ? 0xFF : 0x00 ) << 24 ) )
#define HB_GET_LE_UINT24( p )       ( ( UINT32 ) \
                                      ( ( UINT32 ) (( BYTE * )( p ))[0] | \
                                        ( UINT32 ) (( BYTE * )( p ))[1] <<  8 | \
                                        ( UINT32 ) (( BYTE * )( p ))[2] << 16 ) )
#define HB_PUT_LE_UINT24( p, w )    do { \
                                       (( BYTE * )( p ))[0] = ( BYTE )( w ); \
                                       (( BYTE * )( p ))[1] = ( BYTE )( (w) >>  8 ); \
                                       (( BYTE * )( p ))[2] = ( BYTE )( (w) >> 16 ); \
                                    } while ( 0 )
#define HB_GET_BE_INT24( p )        ( ( INT32 ) \
                                      ( ( INT32 ) (( BYTE * )( p ))[2] | \
                                        ( INT32 ) (( BYTE * )( p ))[1] <<  8 | \
                                        ( INT32 ) (( BYTE * )( p ))[0] << 16 | \
                                        ( INT32 ) ((( BYTE * )( p ))[0] & 0x80 ? 0xFF : 0x00 ) << 24 ) )
#define HB_GET_BE_UINT24( p )       ( ( UINT32 ) \
                                      ( ( UINT32 ) (( BYTE * )( p ))[2] | \
                                        ( UINT32 ) (( BYTE * )( p ))[1] <<  8 | \
                                        ( UINT32 ) (( BYTE * )( p ))[0] << 16 ) )
#define HB_PUT_BE_UINT24( p, w )    do { \
                                       (( BYTE * )( p ))[2] = ( BYTE )( w ); \
                                       (( BYTE * )( p ))[1] = ( BYTE )( (w) >>  8 ); \
                                       (( BYTE * )( p ))[0] = ( BYTE )( (w) >> 16 ); \
                                    } while ( 0 )



#if defined( HB_PDP_ENDIAN )
   #error PDP-Endian support unimplemented. If you have such machine do it yourself.
#elif defined( HB_BIG_ENDIAN )
   /* We use Big-Endian here */

#  ifndef HB_STRICT_ALIGNMENT

   #define HB_GET_BE_UINT16( p )    ( *( UINT16 * )( p ) )
   #define HB_PUT_BE_UINT16( p, w ) ( *( UINT16 * )( p ) = ( UINT16 ) ( w ) )
   #define HB_GET_BE_UINT32( p )    ( *( UINT32 * )( p ) )
   #define HB_PUT_BE_UINT32( p, l ) ( *( UINT32 * )( p ) = ( UINT32 ) ( l ) )
   #define HB_GET_BE_UINT64( p )    ( *( UINT64 * )( p ) )
   #define HB_PUT_BE_UINT64( p, l ) ( *( UINT64 * )( p ) = ( UINT64 ) ( l ) )

#  endif

   #define HB_USHORT_FROM_LE( w )   HB_MKUSHORT( HB_HIBYTE( w ), HB_LOBYTE( w ) )
   #define HB_ULONG_FROM_LE( l )    HB_MKULONG( HB_HIBYTE( HB_HIWORD( l ) ), HB_LOBYTE( HB_HIWORD( l ) ), HB_HIBYTE( l ), HB_LOBYTE( l ) )
   #define HB_USHORT_TO_LE( w )     HB_USHORT_FROM_LE( w )
   #define HB_ULONG_TO_LE( l )      HB_ULONG_FROM_LE( l )

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

/*
   #define HB_ORD2DBL( o, d )       do { \
         *( double * )( d ) = *( double * )( o ); \
         if ( ( ( BYTE * ) ( d ) )[ 0 ] & 0x80 ) { \
            ( ( BYTE * ) ( d ) )[ 0 ] ^= 0x80; \
         } else { \
            ( ( UINT32 * ) ( d ) )[ 0 ] ^= 0xFFFFFFFFL; \
            ( ( UINT32 * ) ( d ) )[ 1 ] ^= 0xFFFFFFFFL; \
         } } while ( 0 )
   #define HB_DBL2ORD( d, o )       do { \
         *( double * )( o ) = *( double * )( d ); \
         if ( *( double * )( o ) >= 0.0 ) { \
            ( ( BYTE * ) ( o ) )[ 0 ] ^= 0x80; \
         } else { \
            ( ( UINT32 * ) ( o ) )[ 0 ] ^= 0xFFFFFFFFL; \
            ( ( UINT32 * ) ( o ) )[ 1 ] ^= 0xFFFFFFFFL; \
         } } while ( 0 )
*/

#if defined( __GNUC__ )
/* Be careful with double conversion. Some machines can use mixed form
   (Little/Big) for BYTE ORDER and WORD ORDER or even completely differ
   internal representation */

   #define HB_GET_LE_DOUBLE( p )    \
       ( { \
            union { \
               double dbl; \
               BYTE buffer[ 8 ]; \
            } u; \
            u.buffer[ 0 ] = (( BYTE * )( p ))[ 7 ]; \
            u.buffer[ 1 ] = (( BYTE * )( p ))[ 6 ]; \
            u.buffer[ 2 ] = (( BYTE * )( p ))[ 5 ]; \
            u.buffer[ 3 ] = (( BYTE * )( p ))[ 4 ]; \
            u.buffer[ 4 ] = (( BYTE * )( p ))[ 3 ]; \
            u.buffer[ 5 ] = (( BYTE * )( p ))[ 2 ]; \
            u.buffer[ 6 ] = (( BYTE * )( p ))[ 1 ]; \
            u.buffer[ 7 ] = (( BYTE * )( p ))[ 0 ]; \
            u.dbl; \
         } )
   #define HB_PUT_LE_DOUBLE( p, d )    \
         do { \
            union { \
               double dbl; \
               BYTE buffer[ 8 ]; \
            } u; \
            u.dbl = ( d ); \
            (( BYTE * )( p ))[ 7 ] = u.buffer[ 0 ]; \
            (( BYTE * )( p ))[ 6 ] = u.buffer[ 1 ]; \
            (( BYTE * )( p ))[ 5 ] = u.buffer[ 2 ]; \
            (( BYTE * )( p ))[ 4 ] = u.buffer[ 3 ]; \
            (( BYTE * )( p ))[ 3 ] = u.buffer[ 4 ]; \
            (( BYTE * )( p ))[ 2 ] = u.buffer[ 5 ]; \
            (( BYTE * )( p ))[ 1 ] = u.buffer[ 6 ]; \
            (( BYTE * )( p ))[ 0 ] = u.buffer[ 7 ]; \
         } while ( 0 )
#else
   #error Little-Endian IEEE 754 double type conversion unimplemented with a non-GCC compiler
#endif

#else /* HB_LITTLE_ENDIAN */
      /* We use Little-Endian here */

#  ifndef HB_STRICT_ALIGNMENT

   #define HB_GET_LE_UINT16( p )    ( *( UINT16 * )( p ) )
   #define HB_PUT_LE_UINT16( p, w ) ( *( UINT16 * )( p ) = ( UINT16 ) ( w ) )
   #define HB_GET_LE_UINT32( p )    ( *( UINT32 * )( p ) )
   #define HB_PUT_LE_UINT32( p, l ) ( *( UINT32 * )( p ) = ( UINT32 ) ( l ) )
   #define HB_GET_LE_UINT64( p )    ( *( UINT64 * )( p ) )
   #define HB_PUT_LE_UINT64( p, l ) ( *( UINT64 * )( p ) = ( UINT64 ) ( l ) )

#  endif

   #define HB_GET_LE_DOUBLE( p )    ( *( double * )( p ) )
   #define HB_PUT_LE_DOUBLE( p, d ) ( *( double * )( p ) = ( double ) ( d ) )

   #define HB_USHORT_FROM_LE( w )   ( ( USHORT )( w ) )
   #define HB_ULONG_FROM_LE( l )    ( ( ULONG )( l ) )
   #define HB_USHORT_TO_LE( w )     ( ( USHORT )( w ) )
   #define HB_ULONG_TO_LE( l )      ( ( ULONG )( l ) )

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

#endif

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
   #define UINT64_MAXDBL            ( (( double ) UINT32_MAX + 1.0) * (( double ) UINT32_MAX + 1.0) - 1 )
   #define HB_GET_LE_INT64( p )     ( ( double ) HB_GET_LE_UINT32( p ) + \
                                      ( double ) HB_GET_LE_UINT32( p + 4 ) * UINT32_MAX - \
                                      ((( BYTE * )( p ))[7] & 0x80 ? UINT64_MAXDBL : 0 ) )
   #define HB_GET_LE_UINT64( p )    ( ( double ) HB_GET_LE_UINT32( p ) + \
                                      ( double ) HB_GET_LE_UINT32( p + 4 ) * UINT32_MAX )
   #define HB_PUT_LE_UINT64( p, w ) do { \
                                       double _d = ( double ) ( w ); \
                                       if ( _d < 0 ) \
                                          _d += UINT64_MAXDBL; \
                                       HB_PUT_LE_UINT32( p, ( UINT32 ) _d ); \
                                       HB_PUT_LE_UINT32( p + 4, ( UINT32 ) ( _d / ( double ) UINT32_MAX ) ); \
                                    } while ( 0 )
   #define HB_PCODE_MKLONGLONG( p )    (( double ) HB_GET_LE_INT64( p ))
   #define HB_PCODE_MKULONGLONG( p )   (( double ) HB_GET_LE_UINT64( p ))
   #define HB_DBL_LIM_INT64(d)      ( (HB_MAXDBL) -UINT64_MAXDBL / 2 - 1 <= (HB_MAXDBL) (d) && (HB_MAXDBL) (d) <= (HB_MAXDBL) UINT64_MAXDBL / 2 )
#endif


#define HB_SYMBOL_UNUSED( symbol )  ( void ) symbol

/* ***********************************************************************
 * The name of starting procedure
 * Note: You have to define it in case when Harbour cannot find the proper
 * starting procedure (due to incorrect order of static data initialization)
 *
 * The list of compilers that require it:
 * - Watcom C/C++ 10.0
 * - GCC on Linux
 *
 * By default we are using automatic lookup (symbol not defined)
*/
#if defined(__WATCOMC__) || ( defined(__GNUC__) && !defined(__DJGPP__) && !defined(HARBOUR_GCC_OS2) )
   #define HARBOUR_START_PROCEDURE "MAIN"
#endif

#if defined(HB_FUNC_CALLCONV)
   #define HARBOUR void HB_FUNC_CALLCONV
#else
   #define HARBOUR void
#endif

#if ! defined(__HARBOUR__)
   #define __HARBOUR__
#endif

typedef HARBOUR ( * PHB_FUNC )( void );
typedef PHB_FUNC HB_FUNC_PTR;

#if defined( __EXPORT__ )
   #if defined( __RSXNT__ )
      /* RSXNT does not support any type of export keyword.
         Exported (i.e., public) names can be obtained via
         the emxexp utility and the output can be used for
         input to a module definition file. See emxdev.doc
         in the RSXNT doc/ directory for more information. */
      #define HB_EXPORT

   #elif defined( __GNUC__ ) && defined( HB_OS_WIN_32 )
      #define HB_EXPORT __attribute__ (( dllexport ))

   #elif defined( __BORLANDC__ )
      #define HB_EXPORT _declspec( dllexport )

   #elif defined( __WATCOMC__ )
      #define HB_EXPORT __declspec( dllexport )

   #elif defined( ASANLM ) || defined( ASANT )
      #define HB_EXPORT

   #elif defined( WIN32 )
      #define HB_EXPORT _declspec( dllexport )

   #else
      #define HB_EXPORT

   #endif
#else
   #define HB_EXPORT
#endif

/* Function declaration macros */

/* NOTE: The prefix is "HB_FUN_" currently, this is needed to
         avoid collision with any other declared symbol.
         Note that "HB_" is not enough, since the Harbour internals
         are also prefixed with HB_. [vszakats] */

#define HB_FUNCNAME( funcname )    HB_FUN_##funcname
#define HB_INIT_FUNCNAME( funcname )    HB_FUN_init_##funcname
#define HB_EXIT_FUNCNAME( funcname )    HB_FUN_exit_##funcname

#if ( defined( _MSC_VER ) || defined( __WATCOMC__ ) ) && defined( HB_FUNC_NO_DECORATION )
   #define HB_EXTERN_C_ extern "C"
#else
   #define HB_EXTERN_C_
#endif

#define HB_FUNC_EXEC( funcname )   HB_FUN_##funcname();
#define HB_FUNC( funcname )        HB_EXTERN_C_ HARBOUR HB_EXPORT HB_FUN_##funcname ( void )
#define HB_FUNC_STATIC( funcname ) HB_EXTERN_C_ static HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_EXTERN( funcname ) HB_EXTERN_C_ extern HARBOUR HB_FUN_##funcname ( void )
#define HB_FUNC_INIT( funcname )   HB_EXTERN_C_ static HARBOUR HB_FUN_init_##funcname ( void )
#define HB_FUNC_EXIT( funcname )   HB_EXTERN_C_ static HARBOUR HB_FUN_exit_##funcname ( void )

typedef ULONG HB_HANDLE;        /* handle to memvar value */
typedef char  HB_SYMBOLSCOPE;   /* stores symbol's scope */

typedef BYTE HB_CHAR;
typedef BYTE HB_ATTR;

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

#endif /* HB_DEFS_H_ */
