/*
 * $Id$
 */

function Test()

   ? Ceiling( 3.2 )
   ? DToR( 180 )
   ? Floor( 3.2 )
   ? NumAsCurrency( 3000, "$", -1 )
   ? NumAsCurrency( 3000, "$", 1 )
   ? NumAsCurrency( 3000, "Euro", -1 )
   ? NumAsCurrency( 3000, "Euro", 1 )
   ? NumAsLog10( 1 )
   ? NumGetDecimals( 1 )
   ? NumGetDecimals( 1.2345 )
   ? NumGetLen( 1 )
   ? NumGetLen( 1.2345 )
   ? RToD( 180 )
   ? Sign( 0 )
   ? Sign( 33 )
   ? Sign( -33 )

return nil
