/*
 * $Id$
 */

#include "hbapi.h"
#include <math.h>

#ifndef M_PI_2
#define M_PI_2      1.57079632679489661923
#endif

HARBOUR HB_ACOS( void )
{
   if( hb_pcount() > 0 )
   {
      double dNumber = hb_parnd(1);

      if( dNumber >= -1 && dNumber <= 1  )
         hb_retnd( acos(dNumber) );
      else
         /* TODO: Error or return NAN */
         hb_retnd(0);
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_ASIN( void )
{
   if( hb_pcount() > 0 )
   {
      double dNumber = hb_parnd(1);

      if( dNumber >= -1 && dNumber <= 1  )
         hb_retnd( asin(dNumber) );
      else
         /* TODO: Error or return NAN */
         hb_retnd(0);
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_ATAN( void )
{
   if( hb_pcount() > 0 )
   {
      double dNumber = hb_parnd(1);

      if( dNumber >= -M_PI_2 && dNumber <= M_PI_2  )
         hb_retnd( atan(dNumber) );
      else
         /* TODO: Error or return NAN */
         hb_retnd(0);
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_COS( void )
{
   if( hb_pcount() > 0 )
   {
      hb_retnd( cos( hb_parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_COSH( void )
{
   if( hb_pcount() > 0 )
   {
      hb_retnd( cosh( hb_parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_LOG10( void )
{
   if( hb_pcount() > 0 )
   {
      hb_retnd( log10( hb_parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_SIN( void )
{
   if( hb_pcount() > 0 )
   {
      hb_retnd( sin( hb_parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_SINH( void )
{
   if( hb_pcount() > 0 )
   {
      hb_retnd( sinh( hb_parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_TAN( void )
{
   if( hb_pcount() > 0 )
   {
      hb_retnd( tan( hb_parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_TANH( void )
{
   if( hb_pcount() > 0 )
   {
      hb_retnd( tanh( hb_parnd(1) ) );
   }
   else
      /* TODO: Error or return NAN */
      hb_retnd(0);
}

HARBOUR HB_PI( void )
{
   hb_retnd( 3.141592653589793 );
}
