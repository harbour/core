/*
 * $Id$
 */

#include <extend.h>
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
   if( _pcount() == 1 )
   {
      PHB_ITEM pNumber = _param(1, IT_NUMERIC);
      
      if( pNumber ) switch( pNumber->wType )
      {
         case IT_INTEGER:
            if( pNumber->value.iNumber >= 0 )
               _retni( pNumber->value.iNumber );
            else
               _retni( -pNumber->value.iNumber );
            break;
         
         case IT_LONG:
            if( pNumber->value.lNumber >= 0 )
               _retnl( pNumber->value.lNumber );
            else
               _retnl( -pNumber->value.lNumber );
            break;
         
         case IT_DOUBLE:
            if( pNumber->value.dNumber >= 0.0 )
               _retnd( pNumber->value.dNumber );
            else
               _retnd( -pNumber->value.dNumber );
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: ABS");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: ABS");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR HB_EXP( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pNumber = _param(1, IT_NUMERIC);

      if( pNumber )
      {
         _retnd( exp(_parnd(1)) );
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: EXP");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: EXP");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR HB_INT( void )
{
   if( _pcount() == 1 )
   {
      if( _param(1, IT_NUMERIC) )
         _retnl( _parnd( 1 ) );
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: INT");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: INT");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR HB_LOG( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pNumber = _param(1, IT_NUMERIC);

      if( pNumber )
      {
         double dNumber = _parnd(1);
         if( dNumber > 0 )
            _retnd( log(dNumber) );
         else
            /* TODO: return OVERFLOW */
            _retnd(0);
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: LOG");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: LOG");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns the maximum of two date or numerics */
HARBOUR HB_MAX( void )
{
   if( _pcount() == 2 )
   {
      PHB_ITEM p1 = _param(1, IT_NUMERIC + IT_DATE), p2 = _param(2, IT_NUMERIC + IT_DATE);

      if( p1 && p2 && p1->wType == p2->wType )
      {
         if( p1->wType == IT_DATE )
         {
            long l1 = p1->value.lDate, l2 = p2->value.lDate;
            _retds(l1 > l2? _pards(1): _pards(2));
         }
         else
         {
            double d1 = _parnd(1), d2 = _parnd(2);
            _retnd(d1 > d2? d1: d2);
         }
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: MAX");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: MAX");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns the minimum of two date or numerics */
HARBOUR HB_MIN( void )
{
   if( _pcount() == 2 )
   {
      PHB_ITEM p1 = _param(1, IT_NUMERIC + IT_DATE), p2 = _param(2, IT_NUMERIC + IT_DATE);

      if( p1 && p2 && p1->wType == p2->wType )
      {
         if( p1->wType == IT_DATE )
         {
            long l1 = p1->value.lDate, l2 = p2->value.lDate;
            _retds(l1 < l2? _pards(1): _pards(2));
         }
         else
         {
            double d1 = _parnd(1), d2 = _parnd(2);
            _retnd(d1 < d2? d1: d2);
         }
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: MIN");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: MIN");
      _errLaunch(pError);
      _errRelease(pError);
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
   PHB_ITEM pNumber = _param(1, IT_NUMERIC);
   PHB_ITEM pBase = _param(2, IT_NUMERIC);

   if( pNumber && pBase )
   {
      double dNumber = _parnd(1);
      double dBase = _parnd(2); /* dBase! Cool! */
      double dResult;

      if( dBase )
      {
         dResult = dNumber - ((long)(dNumber / dBase) * dBase);
  
         if( dResult * dBase < 0 )
            _retnd(dResult + dBase);
         else
            _retnd(dResult);
      }
      else
         _retnd(dNumber);
   }
   else
   {
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Argument error: %");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR HB_ROUND( void )
{
   if( _pcount() == 2 )
   {
      if( _param(1, IT_NUMERIC) && _param( 2, IT_NUMERIC ) )
      {
         int iSize = 64, iDec = _parni( 2 );
         char * szResult;
         double dResult = _parnd( 1 );

         if( iDec < 1 ) iDec = 0;
         else if( dResult != 0.0 )
         {
            double dAdjust = pow( 10, iDec );
            dResult = floor( dResult * dAdjust + 0.5 );
            dResult = dResult / dAdjust;
         }
         szResult = _xgrab( iSize + iDec );
         if( szResult )
         {
            sprintf( szResult, "%*.*f", iSize, iDec, dResult );
            dResult = atof( szResult );
            _xfree( szResult );
         }
         _retnd( dResult );
         stack.Return.wDec = iDec;
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: ROUND");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: INT");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR HB_SQRT( void )
{
   if( _pcount() == 1 )
   {
      PHB_ITEM pNumber = _param(1, IT_NUMERIC);

      if( pNumber )
      {
         double dNumber = _parnd(1);

         if( dNumber > 0 )
            _retnd( sqrt(dNumber) );
         else
            /* Clipper doesn't error! */
            _retnd(0);
      }
      else
      {
         PHB_ITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: SQRT");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PHB_ITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: SQRT");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

