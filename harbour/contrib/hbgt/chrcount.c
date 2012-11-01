/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: chrcount.c
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 * Net/Node..: 050/069
 * User Name.: Andy Leighton
 * Date......: 23/05/93
 * Revision..: 1.00
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HB_FUNC( GT_CHRCOUNT )
{
   const char * s1, * s2;
   HB_ISIZ      count, pos2, len;

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      s1  = hb_parc( 1 );
      s2  = hb_parc( 2 );
      len = hb_parclen( 2 );

      /* loop through s2 matching passed character (s1) with
         each character of s1 */
      for( count = 0, pos2 = 1; pos2 <= len; s2++, pos2++ )
         if( *s1 == *s2 )  /* character matches s1 */
            count++;
      /* increment counter */

      hb_retns( count );   /* return result */
   }
   else
   {
      hb_retns( -1 );                 /* parameter mismatch - error -1 */
   }
}
