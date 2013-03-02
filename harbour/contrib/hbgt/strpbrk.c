/*
 * $Id$
 */

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

HB_FUNC( GT_STRPBRK )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * string = hb_parc( 1 );
      const char * cset   = hb_parc( 2 );
      HB_ISIZ      l1     = hb_parclen( 1 );
      HB_ISIZ      l2     = hb_parclen( 2 );
      HB_ISIZ      p1, p2;

      p1 = p2 = 0;

      do
      {
         for( p2 = 0; ( p2 < l2 ) && ( cset[ p2 ] != string[ p1 ] ); ++p2 )
            ;

         if( p2 < l2 )
         {
            hb_retc( string + p1 );
            break;
         }
      }
      while( p1++ < l1 );

      if( p2 >= l2 )
         hb_retc_null();
   }
   else
      hb_retc_null();
}
