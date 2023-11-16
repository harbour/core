/*
 * BBS.......: The Dark Knight Returns
 * Date......: 1993-05-23
 *
 * This is an original work by Andy M Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HB_FUNC( GT_ASCPOS )
{
   if( HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) )
   {
      const char * s = hb_parc( 1 );
      HB_SIZE      p = hb_parns( 2 );

      p--;                             /* decrement p to adjust for c strings */
                                       /* starting at position 0 */

      if( p > hb_parclen( 1 ) )        /* oh oh p > length of passed string */
         hb_retni( -2 );               /* error -2 */
      else
         hb_retni( ( int ) s[ p ] );   /* return ASCII code of appropriate */
                                       /* character in string */
   }
   else
      hb_retni( -1 );
}
