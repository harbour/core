/*
 * $Id$
 */

#include <extend.h>
#include <math.h>

extern STACK stack;

HARBOUR ABS( void )
{
   if( _pcount() == 1 )
   {
      PITEM pNumber = _param(1, IT_NUMERIC);
      
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
         PITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: ABS");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: ABS");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR EXP( void )
{
   if( _pcount() == 1 )
   {
      PITEM pNumber = _param(1, IT_NUMERIC);

      if( pNumber )
      {
         _retnd( exp(_parnd(1)) );
      }
      else
      {
         PITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: EXP");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: EXP");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR INT( void )
{
   if( _pcount() == 1 )
   {
      if( _param(1, IT_NUMERIC) )
         _retnl( _parnd( 1 ) );
      else
      {
         PITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: INT");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: INT");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR LOG( void )
{
   if( _pcount() == 1 )
   {
      PITEM pNumber = _param(1, IT_NUMERIC);

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
         PITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: LOG");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: LOG");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns the maximum of two date or numerics */
HARBOUR MAX( void )
{
   if( _pcount() == 2 )
   {
      PITEM p1 = _param(1, IT_NUMERIC + IT_DATE), p2 = _param(2, IT_NUMERIC + IT_DATE);

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
         PITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: MAX");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: MAX");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

/* returns the minimum of two date or numerics */
HARBOUR MIN( void )
{
   if( _pcount() == 2 )
   {
      PITEM p1 = _param(1, IT_NUMERIC + IT_DATE), p2 = _param(2, IT_NUMERIC + IT_DATE);

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
         PITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: MIN");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: MIN");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR MOD( void )
{
/*
FUNCTION MOD(cl_num, cl_base)
   
   LOCAL cl_result

	cl_result = cl_num % cl_base

	RETURN IF( cl_base = 0, ;
			   cl_num,;
			   IF(cl_result * cl_base < 0, cl_result + cl_base, cl_result) )
*/
   PITEM pNumber = _param(1, IT_NUMERIC);
   PITEM pBase = _param(2, IT_NUMERIC);

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
      PITEM pError = _errNew();
      _errPutDescription(pError, "Argument error: %");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

HARBOUR SQRT( void )
{
   if( _pcount() == 1 )
   {
      PITEM pNumber = _param(1, IT_NUMERIC);

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
         PITEM pError = _errNew();
         _errPutDescription(pError, "Argument error: SQRT");
         _errLaunch(pError);
         _errRelease(pError);
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      PITEM pError = _errNew();
      _errPutDescription(pError, "Incorrect number of arguments: SQRT");
      _errLaunch(pError);
      _errRelease(pError);
   }
}

