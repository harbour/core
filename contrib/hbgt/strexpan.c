/*
 * BBS.......: The Dark Knight Returns
 * Date......: 1993-05-24
 *
 * This is an original work by Andy M Leighton and is placed in the
 * public domain.
 */

#include "hbapi.h"

HB_FUNC( GT_STREXPAND )
{
   if( HB_ISCHAR( 1 ) && ( HB_ISNUM( 2 ) || hb_pcount() < 2 ) && ( HB_ISCHAR( 3 ) || hb_pcount() < 3 ) )
   {
      const char * in = hb_parc( 1 );
      char *       out;
      int          nIns   = hb_parnidef( 2, 1 );
      const char * insert = " ";
      HB_ISIZ      len    = hb_parclen( 1 );
      HB_ISIZ      i, p;
      int          j;

      if( HB_ISCHAR( 3 ) )
         insert = hb_parc( 3 );

      out = ( char * ) hb_xgrab( len * ( nIns + 1 ) );  /* grab us some memory to work with */

      /* loop thru input */
      for( i = 0, p = 0; i < len; i++ )
      {
         out[ p++ ] = in[ i ];  /* insert a character from input */

         /* do not insert fill chars on last char of input */
         if( i < ( len - 1 ) )
         {
            /* insert the fill characters */
            for( j = 1; j <= nIns; j++ )
               out[ p++ ] = insert[ 0 ];
         }
      }
      out[ p ] = '\0';   /* Add terminating NUL */

      hb_retc_buffer( out );
   }
   else
      hb_retc_null();
}
