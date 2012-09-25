/*
 * $Id$
 */

/*
 * File......: poke.c
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   07 Feb 1994 20:13:22   GLENN
 * Ted re-wrote to make it CPMI compliant.
 *
 *    Rev 1.2   15 Aug 1991 23:08:20   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:48   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:54   GLENN
 * Nanforum Toolkit
 *
 *
 */

#include "hbapi.h"

#include "cpmi.h"

#define FP_SEG( fp ) ( *( ( unsigned int * ) &( fp ) + 1 ) )
#define FP_OFF( fp ) ( *( ( unsigned int * ) &( fp ) ) )

HB_FUNC( FT_POKE )
{
   auto unsigned int    ProtMode = hb_cpmiIsProtected();
   auto unsigned char * bytePtr;

   if( PCOUNT >= 3 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      FP_SEG( bytePtr ) = hb_parni( 1 );
      FP_OFF( bytePtr ) = hb_parni( 2 );

      if( ProtMode )
      {
         FP_SEG( bytePtr ) = hb_cpmiProtectedPtr( bytePtr, 1 );
         FP_OFF( bytePtr ) = 0;

         if( FP_SEG( bytePtr ) == 0 )
            goto Bogus;
      }

      *bytePtr = ( unsigned char ) hb_parni( 3 );

      if( ProtMode )
         hb_cpmiFreeSelector( FP_SEG( bytePtr ) );

      hb_retl( HB_TRUE );
   }
   else
 Bogus: hb_retl( HB_FALSE );
}
