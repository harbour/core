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
#include "set.h"
#include "itemapi.h"
#include "errorapi.h"

HARBOUR HB_ABS( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pNumber = hb_param( 1, IT_NUMERIC );

      if( pNumber ) switch( pNumber->type )
      {
         case IT_INTEGER:
            if( pNumber->item.asInteger.value >= 0 )
               hb_retni( pNumber->item.asInteger.value );
            else
               hb_retni( -pNumber->item.asInteger.value );
            break;

         case IT_LONG:
            if( pNumber->item.asLong.value >= 0 )
               hb_retnl( pNumber->item.asLong.value );
            else
               hb_retnl( -pNumber->item.asLong.value );
            break;

         case IT_DOUBLE:
            if( pNumber->item.asDouble.value >= 0.0 )
               hb_retndlen( pNumber->item.asDouble.value, 0, pNumber->item.asDouble.decimal );
            else
               hb_retndlen( -pNumber->item.asDouble.value, 0, pNumber->item.asDouble.decimal );
      }
      else
         hb_errRT_BASE( EG_ARG, 1089, NULL, "ABS" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "ABS" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_EXP( void )
{
   if( hb_pcount() == 1 )
   {
      if( ISNUM( 1 ) )
         hb_retnd( exp( hb_parnd( 1 ) ) );
      else
         hb_errRT_BASE( EG_ARG, 1096, NULL, "EXP" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "EXP" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_INT( void )
{
   if( hb_pcount() == 1 )
   {
      if( ISNUM( 1 ) )
         hb_retnl( hb_parnd( 1 ) );
      else
         hb_errRT_BASE( EG_ARG, 1090, NULL, "INT" );
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
         double dNumber = hb_parnd( 1 );

         if( dNumber <= 0.0 )
            /* Indicate overflow if called with an invalid argument */
            hb_retndlen( log( dNumber ), 99, ( WORD ) -1 );
         else
            hb_retnd( log( dNumber ) );
      }
      else
         hb_errRT_BASE( EG_ARG, 1095, NULL, "LOG" );
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
         WORD wType1 = p1->type;
         WORD wType2 = p1->type;

         /* NOTE: The order of these if() branches is significant, */
         /*       Don't change it */

         if( wType1 == IT_DOUBLE || wType2 == IT_DOUBLE )
         {
            double d1 = hb_parnd( 1 );
            double d2 = hb_parnd( 2 );

            WORD wDec1;
            WORD wDec2;

            hb_itemGetNLen( p1, NULL, &wDec1 );
            hb_itemGetNLen( p2, NULL, &wDec2 );

            hb_retndlen( d1 >= d2 ? d1 : d2, 0, ( d1 >= d2 ? wDec1 : wDec2 ) );
         }
         else if( wType1 == IT_LONG || wType2 == IT_LONG )
         {
            long l1 = hb_parnl( 1 );
            long l2 = hb_parnl( 2 );

            hb_retnl( l1 >= l2 ? l1 : l2 );
         }
         else
         {
            int i1 = hb_parni( 1 );
            int i2 = hb_parni( 2 );

            hb_retni( i1 >= i2 ? i1 : i2 );
         }
      }
      else if( IS_DATE( p1 ) && IS_DATE( p2 ) )
      {
         long l1 = p1->item.asDate.value;
         long l2 = p2->item.asDate.value;

         hb_retds( l1 >= l2 ? hb_pards( 1 ) : hb_pards( 2 ) );
      }
      else
         hb_errRT_BASE( EG_ARG, 1093, NULL, "MAX" );
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
         WORD wType1 = p1->type;
         WORD wType2 = p1->type;

         /* NOTE: The order of these if() branches is significant, */
         /*       Don't change it */

         if( wType1 == IT_DOUBLE || wType2 == IT_DOUBLE )
         {
            double d1 = hb_parnd( 1 );
            double d2 = hb_parnd( 2 );

            WORD wDec1;
            WORD wDec2;

            hb_itemGetNLen( p1, NULL, &wDec1 );
            hb_itemGetNLen( p2, NULL, &wDec2 );

            hb_retndlen( d1 <= d2 ? d1 : d2, 0, ( d1 <= d2 ? wDec1 : wDec2 ) );
         }
         else if( wType1 == IT_LONG || wType2 == IT_LONG )
         {
            long l1 = hb_parnl( 1 );
            long l2 = hb_parnl( 2 );

            hb_retnl( l1 <= l2 ? l1 : l2 );
         }
         else
         {
            int i1 = hb_parni( 1 );
            int i2 = hb_parni( 2 );

            hb_retni( i1 <= i2 ? i1 : i2 );
         }
      }
      else if( IS_DATE( p1 ) && IS_DATE( p2 ) )
      {
         long l1 = p1->item.asDate.value;
         long l2 = p2->item.asDate.value;

         hb_retds( l1 <= l2 ? hb_pards( 1 ) : hb_pards( 2 ) );
      }
      else
         hb_errRT_BASE( EG_ARG, 1092, NULL, "MIN" );
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
      double dNumber = hb_parnd( 1 );
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
         WORD wDec;

         hb_itemGetNLen( pNumber, NULL, &wDec );

         hb_retndlen( dNumber, 0, wDec );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 1085, NULL, "%" );
}

double hb_numRound( double dResult, int iDec )
{
   int iSize = 64;
   char * szResult;

   if( dResult != 0.0 )
   {
      double dAdjust;

      if( iDec == 0 )
      {
         dResult = floor( dResult + 0.5 );
      }
      else if( iDec < 0 )
      {
         dAdjust = pow( 10, -iDec );
         dResult = floor( dResult / dAdjust + 0.5 );
         dResult = dResult * dAdjust;
      }
      else
      {
         dAdjust = pow( 10, iDec );
         dResult = floor( dResult * dAdjust + 0.5 );
         dResult = dResult / dAdjust;
      }
   }

   szResult = ( char * ) hb_xgrab( iSize + iDec + 1 );

   if( szResult )
   {
      sprintf( szResult, "%*.*f", iSize, iDec, dResult );
      dResult = atof( szResult );
      hb_xfree( szResult );
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

         hb_retndlen( hb_numRound( hb_parnd( 1 ), iDec ), 0, iDec );
      }
      else
         hb_errRT_BASE( EG_ARG, 1094, NULL, "ROUND" );
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
         double dNumber = hb_parnd( 1 );

         if( dNumber > 0 )
            hb_retnd( sqrt( dNumber ) );
         else
            hb_retnd( 0 ); /* Clipper doesn't error! */
      }
      else
         hb_errRT_BASE( EG_ARG, 1097, NULL, "SQRT" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "SQRT" ); /* NOTE: Clipper catches this at compile time! */
}

