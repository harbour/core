/*
 * $Id$
 */

#include <set.h>
#include <errorapi.h>
#include <math.h>

extern STACK stack;

HARBOUR HB_ABS( void );
HARBOUR HB_EXP( void );
HARBOUR HB_INT( void );
HARBOUR HB_LOG( void );
HARBOUR HB_MAX( void );
HARBOUR HB_MIN( void );
HARBOUR HB_MOD( void );
HARBOUR HB_ROUND( void );
HARBOUR HB_SQRT( void );

static SYMBOL symbols[] = {
{ "ABS"  , FS_PUBLIC, HB_ABS  , 0 },
{ "EXP"  , FS_PUBLIC, HB_EXP  , 0 },
{ "INT"  , FS_PUBLIC, HB_INT  , 0 },
{ "LOG"  , FS_PUBLIC, HB_LOG  , 0 },
{ "MAX"  , FS_PUBLIC, HB_MAX  , 0 },
{ "MIN"  , FS_PUBLIC, HB_MIN  , 0 },
{ "MOD"  , FS_PUBLIC, HB_MOD  , 0 },
{ "ROUND", FS_PUBLIC, HB_ROUND, 0 },
{ "SQRT" , FS_PUBLIC, HB_SQRT , 0 }
};

void Math__InitSymbols( void )
{
   ProcessSymbols( symbols, sizeof(symbols)/sizeof( SYMBOL ) );
}

HARBOUR HB_ABS( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pNumber = hb_param(1, IT_NUMERIC);

      if( pNumber ) switch( pNumber->wType )
      {
         case IT_INTEGER:
            if( pNumber->value.iNumber >= 0 )
               hb_retni( pNumber->value.iNumber );
            else
               hb_retni( -pNumber->value.iNumber );
            break;

         case IT_LONG:
            if( pNumber->value.lNumber >= 0 )
               hb_retnl( pNumber->value.lNumber );
            else
               hb_retnl( -pNumber->value.lNumber );
            break;

         case IT_DOUBLE:
            if( pNumber->value.dNumber >= 0.0 )
               hb_retnd( pNumber->value.dNumber );
            else
               hb_retnd( -pNumber->value.dNumber );
      }
      else
      {
         PHB_ITEM pError = hb_errNew();
         hb_errPutDescription(pError, "Argument error: ABS");
         hb_errLaunch(pError);
         hb_errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: ABS");
      hb_errLaunch(pError);
      hb_errRelease(pError);
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
         stack.Return.wDec = hb_set.HB_SET_DECIMALS;
      }
      else
      {
         PHB_ITEM pError = hb_errNew();
         hb_errPutDescription(pError, "Argument error: EXP");
         hb_errLaunch(pError);
         hb_errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: EXP");
      hb_errLaunch(pError);
      hb_errRelease(pError);
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
         PHB_ITEM pError = hb_errNew();
         hb_errPutDescription(pError, "Argument error: INT");
         hb_errLaunch(pError);
         hb_errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: INT");
      hb_errLaunch(pError);
      hb_errRelease(pError);
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
         stack.Return.wDec = hb_set.HB_SET_DECIMALS;
         if( dNumber <= 0.0 )
            /* Indicate overflow if called with an invalid argument */
            stack.Return.wLength = 99;
      }
      else
      {
         PHB_ITEM pError = hb_errNew();
         hb_errPutDescription(pError, "Argument error: LOG");
         hb_errLaunch(pError);
         hb_errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: LOG");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

/* returns the maximum of two date or numerics */
HARBOUR HB_MAX( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM p1 = hb_param(1, IT_NUMERIC + IT_DATE), p2 = hb_param(2, IT_NUMERIC + IT_DATE);

      if( p1 && p2 && p1->wType == p2->wType )
      {
         if( p1->wType == IT_DATE )
         {
            long l1 = p1->value.lDate, l2 = p2->value.lDate;
            hb_retds(l1 > l2? hb_pards(1): hb_pards(2));
         }
         else
         {
            double d1 = hb_parnd(1), d2 = hb_parnd(2);
            hb_retnd(d1 > d2? d1: d2);
         }
      }
      else
      {
         PHB_ITEM pError = hb_errNew();
         hb_errPutDescription(pError, "Argument error: MAX");
         hb_errLaunch(pError);
         hb_errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: MAX");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

/* returns the minimum of two date or numerics */
HARBOUR HB_MIN( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM p1 = hb_param(1, IT_NUMERIC + IT_DATE), p2 = hb_param(2, IT_NUMERIC + IT_DATE);

      if( p1 && p2 && p1->wType == p2->wType )
      {
         if( p1->wType == IT_DATE )
         {
            long l1 = p1->value.lDate, l2 = p2->value.lDate;
            hb_retds(l1 < l2? hb_pards(1): hb_pards(2));
         }
         else
         {
            double d1 = hb_parnd(1), d2 = hb_parnd(2);
            hb_retnd(d1 < d2? d1: d2);
         }
      }
      else
      {
         PHB_ITEM pError = hb_errNew();
         hb_errPutDescription(pError, "Argument error: MIN");
         hb_errLaunch(pError);
         hb_errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: MIN");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

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
      }
      else
         hb_retnd(dNumber);
   }
   else
   {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Argument error: %");
      hb_errLaunch(pError);
      hb_errRelease(pError);
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
         szResult = ( char * ) hb_xgrab( iSize + iDec );
         if( szResult )
         {
            sprintf( szResult, "%*.*f", iSize, iDec, dResult );
            dResult = atof( szResult );
            hb_xfree( szResult );
         }
         hb_retnd( dResult );
         stack.Return.wDec = iDec;
      }
      else
      {
         PHB_ITEM pError = hb_errNew();
         hb_errPutDescription(pError, "Argument error: ROUND");
         hb_errLaunch(pError);
         hb_errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: INT");
      hb_errLaunch(pError);
      hb_errRelease(pError);
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
         stack.Return.wDec = hb_set.HB_SET_DECIMALS;
      }
      else
      {
         PHB_ITEM pError = hb_errNew();
         hb_errPutDescription(pError, "Argument error: SQRT");
         hb_errLaunch(pError);
         hb_errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Incorrect number of arguments: SQRT");
      hb_errLaunch(pError);
      hb_errRelease(pError);
   }
}

