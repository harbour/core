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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include <math.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#if defined(__WATCOMC__)
   #define HB_MATH_HANDLER
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
         PHB_ITEM pResult = hb_errRT_BASE_Subst( s_internal_math_error, 1096, NULL, "EXP" );

         s_internal_math_error = 0;

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
         hb_retnd( dResult );
#else
      hb_retnd( exp( hb_parnd( 1 ) ) );
#endif
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1096, NULL, "EXP" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HB_FUNC( LOG )
{
   if( ISNUM( 1 ) )
   {
#if defined(HB_MATH_HANDLER)
      double dResult = log( hb_parnd( 1 ) );

      if( s_internal_math_error )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( s_internal_math_error, 1095, NULL, "LOG" );

         s_internal_math_error = 0;

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
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
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1095, NULL, "LOG" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HB_FUNC( SQRT )
{
   if( ISNUM( 1 ) )
   {
#if defined(HB_MATH_HANDLER)
      double dResult = sqrt( hb_parnd( 1 ) );

      if( s_internal_math_error )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( s_internal_math_error, 1097, NULL, "SQRT" );

         s_internal_math_error = 0;

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
         hb_retnd( dResult );
#else
      double dNumber = hb_parnd( 1 );

      hb_retnd( dNumber > 0 ? sqrt( dNumber ) : 0 ); /* Clipper doesn't error! */
#endif
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1097, NULL, "SQRT" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

