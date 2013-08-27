/*
 * Harbour Project source code:
 * Harbour math functions and API
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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

#ifndef HB_MATHER_H_
#define HB_MATHER_H_

#include "hbmath.h"

HB_EXTERN_BEGIN

#if defined( __WATCOMC__ )
   #define HB_MATH_HANDLER
   #if ( __WATCOMC__ > 1000 ) /* && defined( __cplusplus ) */
      #define exception _exception
   #endif
#elif defined( __BORLANDC__ )
   #if ( __BORLANDC__ == 1328 ) && defined( __cplusplus )
      /* NOTE: There seem to be a bug in Borland C++ 5.3 C++ mode which prevents
               the redefinition of matherr, because nor "_exception" neither
               "exception" will work. [vszakats] */
   #else
      #define HB_MATH_HANDLER
      #define matherr _matherr
      /* NOTE: This is needed for Borland C++ 5.5 in C++/STDC mode. [vszakats] */
      #if ( __BORLANDC__ >= 1360 )
         #define exception _exception
      #endif
   #endif
#elif defined( __MINGW32CE__ )
   #define HB_MATH_HANDLER
   #define matherr _matherr
   #define exception _exception
/* it seems that MinGW has some problem with MATH HANDLER
   use HB_MATH_ERRNO instead */
#elif defined( __MINGW32__ ) && 0
   #define HB_MATH_HANDLER
   #define matherr _matherr
   #define exception _exception
#elif defined( __DJGPP__ )
   #if ! defined( __cplusplus )
      #define HB_MATH_HANDLER
   #endif
#endif

#if ! defined( HB_MATH_HANDLER ) && \
    ( defined( __GNUC__ ) || defined( HB_OS_UNIX ) )
   #define HB_MATH_ERRNO
#endif

typedef struct _HB_MATH_EXCEPTION
{
   int            type;
   const char *   funcname;
   const char *   error;
   double         arg1;
   double         arg2;
   double         retval;
   int            retvalwidth;
   int            retvaldec;
   int            handled;
} HB_MATH_EXCEPTION;

typedef int ( * HB_MATH_HANDLERPROC )( HB_MATH_EXCEPTION * err );

extern HB_EXPORT void hb_mathResetError( HB_MATH_EXCEPTION * phb_exc );
extern HB_EXPORT HB_BOOL hb_mathGetError( HB_MATH_EXCEPTION * phb_exc, const char *szFunc, double arg1, double arg2, double dResult );

extern HB_EXPORT int hb_mathSetErrMode( int imode );
extern HB_EXPORT int hb_mathGetErrMode( void );

extern HB_EXPORT HB_MATH_HANDLERPROC hb_mathSetHandler( HB_MATH_HANDLERPROC handlerproc );
extern HB_EXPORT HB_MATH_HANDLERPROC hb_mathGetHandler( void );

/* include defines from hbmath.ch */
#include "hbmath.ch"

HB_EXTERN_END

#endif /* HB_MATHER_H_ */
