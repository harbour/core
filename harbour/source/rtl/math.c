/*
 * $Id$
 */
/* Harbour Project source code
   http://www.Harbour-Project.org/
   The following function is Copyright 1999 David G. Holm:
      HB_ROUND().
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include "extend.h"
#include "init.h"
#include "set.h"
#include "errorapi.h"
#include <math.h>

HARBOUR HB_ABS( void );
HARBOUR HB_EXP( void );
HARBOUR HB_INT( void );
HARBOUR HB_LOG( void );
HARBOUR HB_MAX( void );
HARBOUR HB_MIN( void );
HARBOUR HB_MOD( void );
HARBOUR HB_ROUND( void );
HARBOUR HB_SQRT( void );


HB_INIT_SYMBOLS_BEGIN( Math__InitSymbols )
{ "MOD"  , FS_PUBLIC, HB_MOD  , 0 }
HB_INIT_SYMBOLS_END( Math__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup Math__InitSymbols
#endif

/* The rest of functions is pulled automatically by initsymb.c */

HARBOUR HB_ABS( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pNumber = hb_param(1, IT_NUMERIC);

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
               hb_retnd( pNumber->item.asDouble.value );
            else
               hb_retnd( -pNumber->item.asDouble.value );
            stack.Return.item.asDouble.decimal = pNumber->item.asDouble.decimal;
      }
      else
      {
         hb_errorRT_BASE(EG_ARG, 1089, NULL, "ABS");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errorRT_BASE(EG_ARGCOUNT, 3000, NULL, "ABS");
   }
}

HARBOUR HB_EXP( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pNumber = hb_param(1, IT_NUMERIC);

      if( pNumber )
      {
         hb_retnd( exp( hb_parnd( 1 ) ) );
         /* Always set default number of decimals after EXP() */
         stack.Return.item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
      }
      else
      {
         hb_errorRT_BASE(EG_ARG, 1096, NULL, "EXP");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errorRT_BASE(EG_ARGCOUNT, 3000, NULL, "EXP");
   }
}

HARBOUR HB_INT( void )
{
   if( hb_pcount() == 1 )
   {
      if( hb_param(1, IT_NUMERIC) )
         hb_retnl( hb_parnd( 1 ) );
      else
      {
         hb_errorRT_BASE(EG_ARG, 1090, NULL, "INT");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errorRT_BASE(EG_ARGCOUNT, 3000, NULL, "INT");
   }
}

HARBOUR HB_LOG( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pNumber = hb_param(1, IT_NUMERIC);

      if( pNumber )
      {
         double dNumber = hb_parnd( 1 );
         hb_retnd( log( dNumber ) );
         /* Always set default number of decimals after LOG() */
         stack.Return.item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
         if( dNumber <= 0.0 )
            /* Indicate overflow if called with an invalid argument */
            stack.Return.item.asDouble.length = 99;
      }
      else
      {
         hb_errorRT_BASE(EG_ARG, 1095, NULL, "LOG");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errorRT_BASE(EG_ARGCOUNT, 3000, NULL, "LOG");
   }
}

/* returns the maximum of two date or numerics */
HARBOUR HB_MAX( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM p1 = hb_param(1, IT_ANY), p2 = hb_param(2, IT_ANY);

      if( ( IS_NUMERIC( p1 ) && IS_NUMERIC( p2 ) ) || ( IS_DATE( p1 ) && IS_DATE( p2 ) ) )
      {
         if( p1->type == IT_DATE )
         {
            long l1 = p1->item.asDate.value, l2 = p2->item.asDate.value;
            hb_retds(l1 >= l2? hb_pards(1): hb_pards(2));
         }
         else
         {
            double d1 = hb_parnd(1), d2 = hb_parnd(2);
            hb_retnd(d1 >= d2? d1: d2);
            stack.Return.item.asDouble.decimal = (d1 >= d2? p1->item.asDouble.decimal : p2->item.asDouble.decimal);
         }
      }
      else
      {
         hb_errorRT_BASE(EG_ARG, 1093, NULL, "MAX");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errorRT_BASE(EG_ARGCOUNT, 3000, NULL, "MAX");
   }
}

/* returns the minimum of two date or numerics */
HARBOUR HB_MIN( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM p1 = hb_param(1, IT_ANY), p2 = hb_param(2, IT_ANY);

      if( ( IS_NUMERIC( p1 ) && IS_NUMERIC( p2 ) ) || ( IS_DATE( p1 ) && IS_DATE( p2 ) ) )
      {
         if( p1->type == IT_DATE )
         {
            long l1 = p1->item.asDate.value, l2 = p2->item.asDate.value;
            hb_retds(l1 <= l2? hb_pards(1): hb_pards(2));
         }
         else
         {
            double d1 = hb_parnd(1), d2 = hb_parnd(2);
            hb_retnd(d1 <= d2? d1: d2);
            stack.Return.item.asDouble.decimal = (d1 <= d2? p1->item.asDouble.decimal : p2->item.asDouble.decimal);
         }
      }
      else
      {
         hb_errorRT_BASE(EG_ARG, 1092, NULL, "MIN");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errorRT_BASE(EG_ARGCOUNT, 3000, NULL, "MIN");
   }
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
   PHB_ITEM pNumber = hb_param(1, IT_NUMERIC);
   PHB_ITEM pBase = hb_param(2, IT_NUMERIC);

   if( pNumber && pBase )
   {
      double dNumber = hb_parnd(1);
      double dBase = hb_parnd(2); /* dBase! Cool! */
      double dResult;

      if( dBase )
      {
         dResult = dNumber - ((long)(dNumber / dBase) * dBase);

         if( dResult * dBase < 0 )
            hb_retnd(dResult + dBase);
         else
            hb_retnd(dResult);
         /* Always set default number of decimals after computing mod */
         stack.Return.item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
      }
      else
      {
         hb_retnd(dNumber);
         /* Set the correct number of decimals */
         stack.Return.item.asDouble.decimal = (pNumber->item.asDouble.decimal);
      }
   }
   else
   {
      hb_errorRT_BASE(EG_ARG, 1085, NULL, "%");
   }
}

HARBOUR HB_ROUND( void )
{
   if( hb_pcount() == 2 )
   {
      if( hb_param(1, IT_NUMERIC) && hb_param( 2, IT_NUMERIC ) )
      {
         int iSize = 64, iDec = hb_parni( 2 );
         char * szResult;
         double dResult = hb_parnd( 1 );

         if( iDec < 1 ) iDec = 0;
         else if( dResult != 0.0 )
         {
            double dAdjust = pow( 10, iDec );
            dResult = floor( dResult * dAdjust + 0.5 );
            dResult = dResult / dAdjust;
         }
         szResult = ( char * ) hb_xgrab( iSize + iDec + 1 );
         if( szResult )
         {
            sprintf( szResult, "%*.*f", iSize, iDec, dResult );
            dResult = atof( szResult );
            hb_xfree( szResult );
         }
         hb_retnd( dResult );
         stack.Return.item.asDouble.decimal = iDec;
      }
      else
      {
         hb_errorRT_BASE(EG_ARG, 1094, NULL, "ROUND");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errorRT_BASE(EG_ARGCOUNT, 3000, NULL, "ROUND");
   }
}

HARBOUR HB_SQRT( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pNumber = hb_param(1, IT_NUMERIC);

      if( pNumber )
      {
         double dNumber = hb_parnd(1);

         if( dNumber > 0 )
         {
            hb_retnd( sqrt(dNumber) );
         }
         else
            /* Clipper doesn't error! */
            hb_retnd(0);
         /* Always set default number of decimals after SQRT() */
         stack.Return.item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
      }
      else
      {
         hb_errorRT_BASE(EG_ARG, 1097, NULL, "SQRT");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errorRT_BASE(EG_ARGCOUNT, 3000, NULL, "SQRT");
   }
}

