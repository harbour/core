/*
 * $Id$
 */

#include <extend.h>
#include <math.h>

#ifndef M_PI_2
#define M_PI_2      1.57079632679489661923
#endif

HARBOUR ACOS( void )
{
   if( _pcount() > 0 )
   {
      double dNumber = _parnd(1);

      if( dNumber >= -1 && dNumber <= 1  )
         _retnd( acos(dNumber) );
      else
         /* TODO: Error or return NAN */
         _retnd(0);
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

HARBOUR ASIN( void )
{
   if( _pcount() > 0 )
   {
      double dNumber = _parnd(1);

      if( dNumber >= -1 && dNumber <= 1  )
         _retnd( asin(dNumber) );
      else
         /* TODO: Error or return NAN */
         _retnd(0);
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

HARBOUR ATAN( void )
{
   if( _pcount() > 0 )
   {
      double dNumber = _parnd(1);

      if( dNumber >= -M_PI_2 && dNumber <= M_PI_2  )
         _retnd( atan(dNumber) );
      else
         /* TODO: Error or return NAN */
         _retnd(0);
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

HARBOUR COS( void )
{
   if( _pcount() > 0 )
   {
      _retnd( cos( _parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

HARBOUR COSH( void )
{
   if( _pcount() > 0 )
   {
      _retnd( cosh( _parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

HARBOUR LOG10( void )
{
   if( _pcount() > 0 )
   {
      _retnd( log10( _parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

HARBOUR SIN( void )
{
   if( _pcount() > 0 )
   {
      _retnd( sin( _parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

HARBOUR SINH( void )
{
   if( _pcount() > 0 )
   {
      _retnd( sinh( _parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

HARBOUR TAN( void )
{
   if( _pcount() > 0 )
   {
      _retnd( tan( _parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

HARBOUR TANH( void )
{
   if( _pcount() > 0 )
   {
      _retnd( tanh( _parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      _retnd(0);
}

