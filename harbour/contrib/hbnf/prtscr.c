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
 *    Rev 1.3   01 Jan 1995 03:01:00   TED
 * Added dual-mode compatibility.
 *
 *    Rev 1.2   15 Aug 1991 23:08:24   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:54   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:58   GLENN
 * Nanforum Toolkit
 *
 */

#include "hbapi.h"

HB_FUNC( FT_PRTSCR )
{
#if defined( HB_OS_DOS )

   #define pbyte *( ( char * ) 0x00400100 )

   if( HB_ISLOG( 1 ) )
      pbyte = hb_parl( 1 ) ? 0 : 1;

   hb_retl( pbyte != 1 );
#else
   hb_retl( HB_FALSE );
#endif
}
