/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Math functions
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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

#include <math.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#if defined(__WATCOMC__)
   #define HB_MATH_HANDLER
   #define exception _exception
#elif defined(__BORLANDC__)
   #if (__BORLANDC__ == 1328) && defined(__cplusplus)
      /* NOTE: There seem to be a bug in Borland C++ 5.3 C++ mode which prevents
               the redefinition of matherr, because nor "_exception" neither
               "exception" will work. [vszakats] */
   #else
      #define HB_MATH_HANDLER
      #define matherr _matherr
      /* NOTE: This is needed for Borland C++ 5.5 in C++/STDC mode. [vszakats] */
      #if (__BORLANDC__ >= 1360)
         #define exception _exception
      #endif
   #endif
#elif defined(__MINGW32__)
   #define HB_MATH_HANDLER
   #define matherr _matherr
   #define exception _exception
#endif

#if defined(HB_MATH_HANDLER)

static int s_internal_math_error = 0; /* TOFIX: This is not thread safe. */

int hb_getMathError( void )
{
   return( s_internal_math_error );
}

void hb_resetMathError( void )
{
   s_internal_math_error = 0;
}

/* define harbour specific error handler for math errors
 */
int matherr( struct exception * err )
{
   HB_TRACE(HB_TR_DEBUG, ("matherr(%p)", err));

   switch( err->type )
   {
      case DOMAIN:
         /* a domain error has occured, such as sqrt( -1 ) */
         s_internal_math_error = EG_ARG;
         break;
      case SING:
         /* a singularity will result, such as pow( 0, -2 ) */
         s_internal_math_error = EG_ARG;
         break;
      case OVERFLOW:
         /* an overflow will result, such as pow( 10, 100 ) */
         s_internal_math_error = EG_NUMOVERFLOW;
         break;
      case UNDERFLOW:
         /* an underflow will result, such as pow( 10, -100 ) */
         s_internal_math_error = EG_NUMOVERFLOW;
         break;
      case TLOSS:
         /* total loss of significance will result, such as exp( 1000 ) */
         s_internal_math_error = EG_NUMERR;
         break;
      case PLOSS:
         /* partial loss of significance will result, such as sin( 10e70 ) */
         s_internal_math_error = EG_NUMERR;
         break;
      default:
         s_internal_math_error = EG_NUMERR;
         break;
   }

   err->retval = 0.0;

   return 1;   /* don't print any message and don't set errno */
}
#endif

HB_FUNC( EXP )
{
   if( ISNUM( 1 ) )
   {
#if defined(HB_MATH_HANDLER)
      double dResult = exp( hb_parnd( 1 ) );

      if( s_internal_math_error )
      {
         hb_errRT_BASE_SubstR( s_internal_math_error, 1096, NULL, "EXP", 1, hb_paramError( 1 ) );
         s_internal_math_error = 0;
      }
      else
         hb_retnd( dResult );
#else
      hb_retnd( exp( hb_parnd( 1 ) ) );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1096, NULL, "EXP", 1, hb_paramError( 1 ) );
}

HB_FUNC( LOG )
{
   if( ISNUM( 1 ) )
   {
#if defined(HB_MATH_HANDLER)
      double dResult = log( hb_parnd( 1 ) );

      if( s_internal_math_error )
      {
         hb_errRT_BASE_SubstR( s_internal_math_error, 1095, NULL, "LOG", 1, hb_paramError( 1 ) );
         s_internal_math_error = 0;
      }
      else
         hb_retnd( dResult );
#else
      double dNumber = hb_parnd( 1 );

      if( dNumber <= 0.0 )
         /* Indicate overflow if called with an invalid argument */
         hb_retndlen( log( dNumber ), 99, -1 );
      else
         hb_retnd( log( dNumber ) );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1095, NULL, "LOG", 1, hb_paramError( 1 ) );
}

HB_FUNC( SQRT )
{
   if( ISNUM( 1 ) )
   {
#if defined(HB_MATH_HANDLER)
      double dResult = sqrt( hb_parnd( 1 ) );

      if( s_internal_math_error )
      {
         hb_errRT_BASE_SubstR( s_internal_math_error, 1097, NULL, "SQRT", 1, hb_paramError( 1 ) );
         s_internal_math_error = 0;
      }
      else
         hb_retnd( dResult );
#else
      double dNumber = hb_parnd( 1 );

      hb_retnd( dNumber > 0 ? sqrt( dNumber ) : 0 ); /* Clipper doesn't error! */
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1097, NULL, "SQRT", 1, hb_paramError( 1 ) );
}

