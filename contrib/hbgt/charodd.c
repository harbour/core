/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 * Date......: 1993-05-24
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HB_FUNC( GT_CHARODD )
{
   if( HB_ISCHAR( 1 ) )
   {
      const char * s1 = hb_parc( 1 );
      char *       s2;
      HB_ISIZ      len = hb_parclen( 1 );
      HB_ISIZ      i;

      s2 = ( char * ) hb_xgrab( len / 2 + 1 );   /* grab us some mem to work with */

      for( i = 0; i <= len; i += 2 )
         s2[ i / 2 ] = s1[ i ] & 0x7f;

      hb_retclen_buffer( s2, len / 2 );
   }
   else
      hb_retc_null();
}
