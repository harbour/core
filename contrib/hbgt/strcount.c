/*
 * GT CLIPPER STANDARD HEADER
 *
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 * Date......: 1993-05-23
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HB_FUNC( GT_STRCOUNT )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * s1 = hb_parc( 1 );
      const char * s2 = hb_parc( 2 );
      HB_ISIZ      l1 = hb_parclen( 1 );
      HB_ISIZ      l2 = hb_parclen( 2 );
      HB_ISIZ      count, p1, p2;
      int          match;

      /* loop through s2 matching passed character (s1) with
         each character of s1 */

      for( count = 0, p2 = 0; p2 <= ( l2 - l1 ); p2++ )
      {
         match = 1;

         for( p1 = 0; p1 < l1; p1++ )
         {
            if( s1[ p1 ] != s2[ p2 + p1 ] )
               match = 0;
         }

         if( match )
            count++;
      }

      hb_retns( count );
   }
   else
      hb_retns( -1 );
}
