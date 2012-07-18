/*
 * $Id$
 */

/*
 * GT CLIPPER STANDARD HEADER
 *
 * File......: charodd.c
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

HB_FUNC( GT_CHARODD )
{
   const char * s1;
   char * s2;
   HB_ISIZ len, i;

   if( HB_ISCHAR( 1 ) )
   {
      s1    = hb_parc( 1 );
      len   = hb_parclen( 1 );

      s2    = ( char * ) hb_xgrab( len / 2 ); /* grab us some mem to work with */

      for( i = 0; i <= len; i += 2 )
         s2[ i / 2 ] = s1[ i ] & 0x7f;

      hb_retclen( s2, len );
      hb_xfree( s2 );                 /* free alloc'ed mem */
   }
   else
   {
      hb_retc_null();                 /* parameter mismatch - error NullStr */
   }
}
