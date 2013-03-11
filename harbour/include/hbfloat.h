/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour floating point math macros
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_FLOAT_H_
#define HB_FLOAT_H_

#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif

#ifndef __EXTENSIONS__
#  define __EXTENSIONS__
#endif

/* workaround for some missing C99 math macros in SunOS GCC
 * used in C++ mode
 */
#if ! defined( __C99FEATURES__ ) && defined( __GNUC__ ) && defined( __sun__ )
#  define __C99FEATURES__
#endif

#include "hbapi.h"

#if defined( __DJGPP__ )
#  include <libm/math.h>
   /* _LIB_VERSION_TYPE _LIB_VERSION = _XOPEN_; */
#else
#  include <math.h>
#  if defined( __BORLANDC__ ) || defined( __WATCOMC__ ) || defined( _MSC_VER ) || defined( HB_OS_MINIX )
#     include <float.h>
#  elif defined( HB_OS_SUNOS )
#     include <ieeefp.h>    /* for finite() */
#  endif
#endif


#if defined( HB_LONG_DOUBLE_OFF ) && ! defined( __NO_LONGDOUBLE__ )
#  define __NO_LONGDOUBLE__
#endif


#define _HB_NUM_NAN     1
#define _HB_NUM_PINF    2
#define _HB_NUM_NINF    4


#if defined( __BORLANDC__ ) && 0
   /* do not use Borland C _fpclass[l]() function.
    * it switches internal logic used for floating point calculation
    * in this compiler reducing the precision to 'float' type.
    */
#  ifdef __NO_LONGDOUBLE__
#     define hb_fpclassify( d )     _fpclass( d )
#  else
#     define hb_fpclassify( d )     ( sizeof( d ) ==  sizeof( double ) ? \
                                      _fpclass( d ) : _fpclassl( d ) )
#  endif
#endif


/* on some platforms signbit() needs _GNU_SOURCE defined.
 * If it's not available on given platform then it can be replaced by
 * 'value < 0' but in such case -0.0 will be shown as 0.0
 */
#if defined( _ISOC99_SOURCE ) || defined( _STDC_C99 ) || defined( signbit )

#  define hb_signbit( d )     signbit( d )

#elif defined( __BORLANDC__ ) && defined( hb_fpclassify )

#  define hb_signbit( d )     ( ( hb_fpclassify( d ) & ( _FPCLASS_NINF | _FPCLASS_NZ ) ) != 0 )

#elif 0 /* TODO: add other C compilers here (check their version number) */
#else

#  define hb_signbit( d )     ( d < 0 )

#endif



#if defined( _ISOC99_SOURCE ) || defined( _STDC_C99 ) || \
    ( defined( isfinite ) && defined( isnan ) && defined( isinf ) )

   /* use C99 macros */
#  define hb_isfinite( d )    isfinite( d )
#  define HB_NUMTYPE( v, d )  do { \
                                 v = ( isfinite( d ) ? 0 : \
                                       ( isnan( d ) ? _HB_NUM_NAN : \
                                         ( isinf( d ) < 0 ? _HB_NUM_NINF : \
                                           _HB_NUM_PINF ) ) ); \
                              } while( 0 )

#elif ( defined( __GNUC__ ) || \
        defined( __SUNPRO_C ) || defined( __SUNPRO_CC ) ) && \
      ( defined( _BSD_SOURCE ) || defined( _SVID_SOURCE ) || \
        defined( _XOPEN_SOURCE ) )

   /* use BSD floating point functions */

#  define hb_isfinite( d )    finite( d )
#  define HB_NUMTYPE( v, d )  do { \
                                 v = ( finite( d ) ? 0 : \
                                       ( isnan( d ) ? _HB_NUM_NAN : \
                                         ( isinf( d ) < 0 ? _HB_NUM_NINF : \
                                           _HB_NUM_PINF ) ) ); \
                              } while( 0 )
#  if ! defined( __NO_LONGDOUBLE__ ) && ! defined( HB_OS_SUNOS )
#     define HB_NUMTYPEL( v, d ) do { \
                                    v = ( finitel( d ) ? 0 : \
                                          ( isnanl( d ) ? _HB_NUM_NAN : \
                                            ( isinfl( d ) < 0 ? _HB_NUM_NINF : \
                                              _HB_NUM_PINF ) ) ); \
                                 } while( 0 )
#  endif

#elif defined( __BORLANDC__ )

#  define hb_isfinite( d )       _finite( d )
#  if defined( hb_fpclassify )
#     define HB_NUMTYPE( v, d )  do { \
                                    int t = hb_fpclassify( d ); \
                                    v = ( ( t & ( _FPCLASS_UNSUP | _FPCLASS_SNAN | _FPCLASS_QNAN ) ) ? _HB_NUM_NAN : \
                                          ( ( t & _FPCLASS_NINF ) ? _HB_NUM_NINF : \
                                            ( ( t & _FPCLASS_PINF ) ? _HB_NUM_PINF : 0 ) ) ); \
                                 } while( 0 )
#  else
#     define HB_NUMTYPE( v, d )  do { \
                                    v = ( _finite( d ) ? 0 : \
                                          ( _isnan( d ) ? _HB_NUM_NAN : \
                                              _HB_NUM_PINF ) ); \
                                 } while( 0 )
#     if ! defined( __NO_LONGDOUBLE__ )
#        define HB_NUMTYPEL( v, d ) do { \
                                       v = ( _finitel( d ) ? 0 : \
                                             ( _isnanl( d ) ? _HB_NUM_NAN : \
                                                 _HB_NUM_PINF ) ); \
                                    } while( 0 )
#     endif
#  endif

#elif 0 /* TODO: add other C compilers here (check their version number) */
#else

#  if defined( __RSXNT__ ) || defined( __EMX__ ) || \
      defined( __XCC__ ) || defined( __POCC__ ) || \
      defined( __MINGW32__ ) || defined( HB_OS_HPUX ) || defined( HB_OS_MINIX )
#     define hb_isfinite( d )       isfinite( d )
#  elif defined( _MSC_VER )
#     define hb_isfinite( d )       _finite( ( double ) d )
#  elif defined( __BORLANDC__ ) || defined( __WATCOMC__ )
#     define hb_isfinite( d )       _finite( d )
#  elif defined( __GNUC__ ) || defined( __DJGPP__ ) || defined( __LCC__ ) || \
      defined( HB_OS_SUNOS )
#     define hb_isfinite( d )       finite( d )
#  endif

#  if defined( hb_isfinite )
#     define HB_NUMTYPE( v, d )  do { \
                                    v = hb_isfinite( d ) ? 0 : _HB_NUM_NAN ; \
                                 } while( 0 )
#  else
#     define hb_isfinite( d )       HB_TRUE
#     define HB_NUMTYPE( v, d )  do { \
                                    int iTODO; \
                                    v = hb_isfinite( d ) ? 0 : _HB_NUM_NAN ; \
                                 } while( 0 )
#  endif

#endif

#if ! defined( HB_NUMTYPEL )
#  define HB_NUMTYPEL( v, d ) HB_NUMTYPE( v, d )
#endif


/* NOTE: Workaround for Pellec C 5.00 not having an 'inf' (HUGE_VAL)
         in '-Tarm-coff' mode. [vszakats] */
#if defined( __POCC__ ) && defined( HB_OS_WIN_CE )
   #undef HUGE_VAL
   #define HUGE_VAL   ( 1.0 / ( 1.0, 0.0 ) )
#endif

#endif /* HB_FLOAT_H_ */
