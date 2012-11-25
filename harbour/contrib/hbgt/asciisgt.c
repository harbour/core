/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HB_FUNC( GT_ASCIISUM )
{
   const char * str = hb_parcx( 1 );
   HB_ISIZ      len = hb_parclen( 1 );
   HB_ISIZ      i;
   HB_MAXUINT   ascSum = 0;

   for( i = 0; i <= len; i++, str++ )
      ascSum += *str;

   hb_retnint( ascSum );
}
