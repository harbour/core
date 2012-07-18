/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: strexpan.c
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

HB_FUNC( GT_STREXPAND )
{
   const char * in;
   char * out;
   int nIns = 1;
   const char * insert = " ";
   HB_ISIZ len;
   HB_ISIZ i, p;
   int j;

   if( HB_ISCHAR( 1 ) && ( HB_ISNUM( 2 ) || hb_pcount() < 2 ) && ( HB_ISCHAR( 3 ) || hb_pcount() < 3 ) )
   {
      in    = hb_parc( 1 );
      len   = hb_parclen( 1 );

      if( HB_ISNUM( 2 ) )
         nIns = hb_parni( 2 );

      if( HB_ISCHAR( 3 ) )
         insert = hb_parc( 3 );

      out = ( char * ) hb_xgrab( len * ( nIns + 1 ) );   /* alloc us some memory */

      for( i = 0, p = 0; i < len; i++ )                  /* loop thru input */
      {
         out[ p++ ] = in[ i ];                           /* insert a character from input */

         if( i < ( len - 1 ) )                           /* do not insert fill chars on last */
         {                             /* char of input */
            for( j = 1; j <= nIns; j++ )                 /* insert the fill characters */
               out[ p++ ] = insert[ 0 ];
         }
      }
      out[ p ] = '\0';                 /* Add terminating NUL */

      hb_retc( out );
      hb_xfree( out );                 /* free alloc'ed mem */
   }
   else
   {
      hb_retc_null();                  /* parameter mismatch - error NullStr */
   }
}
