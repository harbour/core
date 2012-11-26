/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 * Date......: 1993.05.23
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HB_FUNC( GT_CHRCOUNT )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * s1  = hb_parc( 1 );
      const char * s2  = hb_parc( 2 );
      HB_ISIZ      len = hb_parclen( 2 );
      HB_ISIZ      count, pos2;

      /* loop through s2 matching passed character (s1) with
         each character of s1 */
      for( count = 0, pos2 = 1; pos2 <= len; s2++, pos2++ )
         if( *s1 == *s2 )  /* character matches s1 */
            count++;  /* increment counter */

      hb_retns( count );
   }
   else
      hb_retns( -1 );
}
