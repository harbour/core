/*
 * $Id$
 */

/* Harbour Project source code
   http://www.Harbour-Project.org/

   The following function is Copyright 1999 David G. Holm:
      HB_ROUND().

   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include <math.h>
#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"

static int internal_math_error = 0;

#if defined( __WATCOMC__ )
/* define harbour specific error handler for math errors
 */
int matherr( struct exception *err )
{
   switch( err->type )
   {
      case DOMAIN:
         /* a domain error has occured, such as sqrt( -1 ) */
         internal_math_error = EG_ARG;
         break;
      case SING:
         /* a singularity will result, such as pow( 0, -2 ) */
         internal_math_error = EG_ARG;
         break;
      case OVERFLOW:
         /* an overflow will result, such as pow( 10, 100 ) */
         internal_math_error = EG_NUMOVERFLOW;
         break;
      case UNDERFLOW:
         /* an underflow will result, such as pow( 10, -100 ) */
         internal_math_error = EG_NUMOVERFLOW;
         break;
      case TLOSS:
         /* total loss of significance will result, such as exp( 1000 ) */
         internal_math_error = EG_NUMERR;
         break;
      case PLOSS:
         /* partial loss of significance will result, such as sin( 10e70 ) */
         internal_math_error = EG_NUMERR;
         break;
      default:
         internal_math_error = EG_NUMERR;
         break;
   }
   err->retval = 0.0;
   return 1;   /* don't print any message and don't ser errno */
}
#endif

HARBOUR HB_ABS( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pNumber = hb_param( 1, IT_NUMERIC );

      if( pNumber )
      {
         int iWidth;
         int iDec;

         hb_itemGetNLen( pNumber, &iWidth, &iDec );

         if( IS_INTEGER( pNumber ) )
         {
            int iNumber = hb_itemGetNI( pNumber );

            if( iNumber >= 0 )
               hb_retnilen( iNumber, iWidth );
            else
               hb_retni( -iNumber );
         }
         else if( IS_LONG( pNumber ) )
         {
            long lNumber = hb_itemGetNL( pNumber );

            if( lNumber >= 0 )
               hb_retnllen( lNumber, iWidth );
            else
               hb_retnl( -lNumber );
         }
         else if( IS_DOUBLE( pNumber ) )
         {
            double dNumber = hb_itemGetND( pNumber );

            hb_retndlen( dNumber >= 0.0 ? dNumber : -dNumber, 0, iDec );
         }
      }
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1089, NULL, "ABS" );

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "ABS" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_EXP( void )
{
   if( hb_pcount() == 1 )
   {
      if( ISNUM( 1 ) )
      {
         double dResult = exp( hb_parnd( 1 ) );

         if( internal_math_error )
         {
            PHB_ITEM pResult = hb_errRT_BASE_Subst( internal_math_error, 1096, NULL, "EXP" );

            internal_math_error = 0;
            if( pResult )
            {
               hb_itemReturn( pResult );
               hb_itemRelease( pResult );
            }
         }
         else
            hb_retnd( dResult );
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
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "EXP" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_INT( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pNumber = hb_param( 1, IT_NUMERIC );

      if( pNumber )
      {
         double dNumber = hb_itemGetND( pNumber );
         int iWidth;

         hb_itemGetNLen( pNumber, &iWidth, NULL );

         hb_retndlen( dNumber >= 0 ? floor( dNumber ) : ceil( dNumber ), iWidth, 0 );
      }
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1090, NULL, "INT" );

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "INT" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_LOG( void )
{
   if( hb_pcount() == 1 )
   {
      if( ISNUM( 1 ) )
      {
#if defined( __WATCOMC__ )
         double dResult = log( hb_parnd( 1 ) );
         if( internal_math_error )
         {
            PHB_ITEM pResult = hb_errRT_BASE_Subst( internal_math_error, 1095, NULL, "LOG" );

            internal_math_error = 0;
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
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "LOG" ); /* NOTE: Clipper catches this at compile time! */
}

/* returns the maximum of two date or numerics */
HARBOUR HB_MAX( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM p1 = hb_param( 1, IT_ANY );
      PHB_ITEM p2 = hb_param( 2, IT_ANY );

      if( IS_NUMERIC( p1 ) && IS_NUMERIC( p2 ) )
      {
         /* NOTE: The order of these if() branches is significant, */
         /*       Please, don't change it. */

         if( IS_DOUBLE( p1 ) || IS_DOUBLE( p2 ) )
         {
            double d1 = hb_itemGetND( p1 );
            double d2 = hb_itemGetND( p2 );

            int iDec1;
            int iDec2;

            hb_itemGetNLen( p1, NULL, &iDec1 );
            hb_itemGetNLen( p2, NULL, &iDec2 );

            hb_retndlen( d1 >= d2 ? d1 : d2, 0, ( d1 >= d2 ? iDec1 : iDec2 ) );
         }
         else if( IS_LONG( p1 ) || IS_LONG( p2 ) )
         {
            long l1 = hb_itemGetNL( p1 );
            long l2 = hb_itemGetNL( p2 );

            hb_retnl( l1 >= l2 ? l1 : l2 );
         }
         else
         {
            int i1 = hb_itemGetNI( p1 );
            int i2 = hb_itemGetNI( p2 );

            hb_retni( i1 >= i2 ? i1 : i2 );
         }
      }
      else if( IS_DATE( p1 ) && IS_DATE( p2 ) )
         hb_retds( hb_itemGetNL( p1 ) >= hb_itemGetNL( p2 ) ? hb_pards( 1 ) : hb_pards( 2 ) );

      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1093, NULL, "MAX" );

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "MAX" ); /* NOTE: Clipper catches this at compile time! */
}

/* returns the minimum of two date or numerics */
HARBOUR HB_MIN( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM p1 = hb_param( 1, IT_ANY );
      PHB_ITEM p2 = hb_param( 2, IT_ANY );

      if( IS_NUMERIC( p1 ) && IS_NUMERIC( p2 ) )
      {
         /* NOTE: The order of these if() branches is significant, */
         /*       Please, don't change it. */

         if( IS_DOUBLE( p1 ) || IS_DOUBLE( p2 ) )
         {
            double d1 = hb_itemGetND( p1 );
            double d2 = hb_itemGetND( p2 );

            int iDec1;
            int iDec2;

            hb_itemGetNLen( p1, NULL, &iDec1 );
            hb_itemGetNLen( p2, NULL, &iDec2 );

            hb_retndlen( d1 <= d2 ? d1 : d2, 0, ( d1 <= d2 ? iDec1 : iDec2 ) );
         }
         else if( IS_LONG( p1 ) || IS_LONG( p2 ) )
         {
            long l1 = hb_itemGetNL( p1 );
            long l2 = hb_itemGetNL( p2 );

            hb_retnl( l1 <= l2 ? l1 : l2 );
         }
         else
         {
            int i1 = hb_itemGetNI( p1 );
            int i2 = hb_itemGetNI( p2 );

            hb_retni( i1 <= i2 ? i1 : i2 );
         }
      }
      else if( IS_DATE( p1 ) && IS_DATE( p2 ) )
         hb_retds( hb_itemGetNL( p1 ) <= hb_itemGetNL( p2 ) ? hb_pards( 1 ) : hb_pards( 2 ) );

      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1092, NULL, "MIN" );

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "MIN" ); /* NOTE: Clipper catches this at compile time! */
}

/* TOFIX: In Clipper this is written in Clipper, see the source below, */
/*        and the error handling is NOT made here, but in the % operator */

HARBOUR HB_MOD( void )
{
/*
FUNCTION MOD(cl_num, cl_base)

   LOCAL cl_result

        cl_result = cl_num % cl_base

        RETURN IF( cl_base = 0, ;
                           cl_num,;
                           IF(cl_result * cl_base < 0, cl_result + cl_base, cl_result) )
*/
   PHB_ITEM pNumber = hb_param( 1, IT_NUMERIC );

   if( pNumber && ISNUM( 2 ) )
   {
      double dNumber = hb_itemGetND( pNumber );
      double dBase = hb_parnd( 2 ); /* dBase! Cool! */

      if( dBase )
      {
         double dResult = dNumber - ( ( long )( dNumber / dBase ) * dBase );

         if( dResult * dBase < 0 )
            hb_retnd( dResult + dBase );
         else
            hb_retnd( dResult );
      }
      else
      {
         int iDec;

         hb_itemGetNLen( pNumber, NULL, &iDec );

         hb_retndlen( dNumber, 0, iDec );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 1085, NULL, "%" );
}

double hb_numRound( double dResult, int iDec )
{
   if( dResult != 0.0 )
   {
      double dAdjust;

      if( iDec == 0 )
      {
         if( dResult < 0.0 )
            dResult = ceil( dResult - 0.5 );
         else
            dResult = floor( dResult + 0.5 );
      }
      else if( iDec < 0 )
      {
         dAdjust = pow( 10, -iDec );
         if( dResult < 0.0 )
            dResult = ceil( ( dResult / dAdjust ) - 0.5 );
         else
            dResult = floor( ( dResult / dAdjust ) + 0.5 );
         dResult = dResult * dAdjust;
      }
      else
      {
         dAdjust = pow( 10, iDec );
         if( dResult < 0.0 )
            dResult = ceil( ( dResult * dAdjust ) - 0.5 );
         else
            dResult = floor( ( dResult * dAdjust ) + 0.5 );
         dResult = dResult / dAdjust;
      }
   }

   return dResult;
}

HARBOUR HB_ROUND( void )
{
   if( hb_pcount() == 2 )
   {
      if( ISNUM( 1 ) && ISNUM( 2 ) )
      {
         int iDec = hb_parni( 2 );

         hb_retndlen( hb_numRound( hb_parnd( 1 ), iDec ), 0, MAX( iDec, 0 ) );
      }
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1094, NULL, "ROUND" );

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "ROUND" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_SQRT( void )
{
   if( hb_pcount() == 1 )
   {
      if( ISNUM( 1 ) )
      {
#if defined( __WATCOMC__ )
         double dResult = sqrt( hb_parnd( 1 ) );
         if( internal_math_error )
         {
            PHB_ITEM pResult = hb_errRT_BASE_Subst( internal_math_error, 1097, NULL, "SQRT" );

            internal_math_error = 0;
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

         if( dNumber > 0 )
            hb_retnd( sqrt( dNumber ) );
         else
            hb_retnd( 0 ); /* Clipper doesn't error! */
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
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "SQRT" ); /* NOTE: Clipper catches this at compile time! */
}

