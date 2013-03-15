/*
 * $Id$
 */

/*
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *     Rev 1.4   16 Oct 1992 00:00:56   GLENN
 *  Just making sure we have Ted's latest revisions.
 *
 *     Rev 1.3   01 Jul 1992 01:07:02   GLENN
 *  putkey.asm now bypasses the BIOS completely and uses Clipper's
 *  internal event handler to stuff the keystroke.  Modifications by
 *  Ted Means.
 *
 *     Rev 1.2   15 Aug 1991 23:07:10   GLENN
 *  Forest Belt proofread/edited/cleaned up doc
 *
 *     Rev 1.1   14 Jun 1991 19:54:56   GLENN
 *  Minor edit to file header
 *
 *     Rev 1.0   01 Apr 1991 01:03:48   GLENN
 *  Nanforum Toolkit
 */

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( FT_PUTKEY )
{
   HB_BOOL lSuccess = HB_FALSE;

   if( HB_ISNUM( 1 ) )
   {
      hb_inkeyPut( hb_parni( 1 ) );
      lSuccess = HB_TRUE;
   }

   hb_retl( lSuccess );
}
