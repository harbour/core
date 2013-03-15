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

HB_FUNC( GT_STRDIFF )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * s1 = hb_parc( 1 );
      const char * s2 = hb_parc( 2 );
      HB_ISIZ      pos;
      HB_ISIZ      len = hb_parclen( 2 );

      /*
         loop through comparing both strings

         NOTE: pos starts at 1, so as to return a string index
               for CLIPPER
       */

      for( pos = 1; ( pos <= len ) && ( *s1 == *s2 ); s2++, s1++ )
         pos++;

      if( pos > len )   /* strings match exactly!!! */
         hb_retc_null();
      else
         hb_retc( s2 );
   }
   else
      hb_ret();
}
