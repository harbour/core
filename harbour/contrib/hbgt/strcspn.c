/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: strcspn.c
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

HB_FUNC( GT_STRCSPN )
{
   const char * string;
   const char * cset;
   HB_ISIZ      l1, l2;
   HB_ISIZ      p1, p2;

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      string = hb_parc( 1 );
      cset   = hb_parc( 2 );
      l1     = hb_parclen( 1 );
      l2     = hb_parclen( 2 );

      for( p1 = 0; p1 < l1; ++p1 )
      {
         for( p2 = 0; ( p2 < l2 ) && ( string[ p1 ] != cset[ p2 ] ); ++p2 )
            ;

         if( p2 < l2 )
            break;
      }
      hb_retns( p1 );
   }
   else
   {
      hb_retns( -1 );                 /* parameter mismatch - error -1 */
   }
}
