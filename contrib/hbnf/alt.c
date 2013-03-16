/*
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This function is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.5   16 Apr 2000 17:30:00   Luiz Rafael Culik
 * Ported to Harbour
 *
 *    Rev 1.4   15 Jul 1993 23:48:00   GLENN
 * Dropped _MK_FP for preferred 0x00400017
 *
 *    Rev 1.3   13 Jul 1993 22:28:58   GLENN
 * Added call to _MK_FP in order to be compatible in protected mode.
 *
 *    Rev 1.2   15 Aug 1991 23:08:34   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:36   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:42   GLENN
 * Nanforum Toolkit
 *
 *
 */

#include "hbapigt.h"
#include "hbapiitm.h"

HB_FUNC( FT_ALT )
{
   HB_GT_INFO gtInfo;

   memset( &gtInfo, 0, sizeof( gtInfo ) );
   hb_gtInfo( HB_GTI_KBDSHIFTS, &gtInfo );
   hb_retl( ( hb_itemGetNI( gtInfo.pResult ) & HB_GTI_KBD_ALT ) != 0 );
   if( gtInfo.pResult )
      hb_itemRelease( gtInfo.pResult );
}
