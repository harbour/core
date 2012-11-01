/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: chrtotal.c
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

HB_FUNC( GT_CHRTOTAL )
{
   const char * s1, * s2;
   HB_ISIZ      count, p1, p2, l2, l1;

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      s1 = hb_parc( 1 );
      s2 = hb_parc( 2 );
      l2 = hb_parclen( 2 );
      l1 = hb_parclen( 1 );

      for( count = 0, p2 = 0; p2 < l2; p2++ )
         for( p1 = 0; p1 < l1; p1++ )
            if( s1[ p1 ] == s2[ p2 ] )
               count++;
      /* increment counter */

      hb_retns( count );   /* return result */
   }
   else
   {
      hb_retns( -1 );                 /* parameter mismatch - error -1 */
   }
}
