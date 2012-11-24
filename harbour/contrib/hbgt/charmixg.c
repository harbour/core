/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * Author....: Andy M Leighton
 * BBS.......: The Dark Knight Returns
 * Net/Node..: 050/069
 * User Name.: Andy Leighton
 * Date......: 24/05/93
 * Revision..: 1.00
 *
 * This is an original work by Andy Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HB_FUNC( GT_CHARMIX )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * s1 = hb_parc( 1 );
      const char * s2 = hb_parc( 2 );
      HB_ISIZ      l1 = hb_parclen( 1 );
      HB_ISIZ      l2 = hb_parclen( 2 );
      char *       s3;
      HB_ISIZ      i, pos3 = 0;

      s3 = ( char * ) hb_xgrab( l1 + l2 + 1 );    /* grab us some mem to work with */

      for( i = 0; i < l1; i++ )
      {
         s3[ pos3++ ] = s1[ i ];

         if( i < l2 )
            s3[ pos3++ ] = s2[ i ];
      }

      if( l2 > l1 )
         for(; i < l2; i++ )
            s3[ pos3++ ] = s2[ i ];

      s3[ pos3 ] = '\0';
      hb_retclen_buffer( s3, l1 + l2 );
   }
   else
      hb_retc_null();                 /* parameter mismatch - error NullStr */
}
