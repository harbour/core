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
 *    Rev 1.3   15 Jul 1993 00:08:46   GLENN
 * Changed reference to status_byte in order to make this work in
 * protected mode.
 *
 *    Rev 1.2   15 Aug 1991 23:08:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   27 May 1991 14:43:20   GLENN
 * Ted added a parameter to toggle the Numlock on or off.
 *
 *    Rev 1.0   01 Apr 1991 01:02:50   GLENN
 * Nanforum Toolkit
 *
 *
 */

#include "hbapigt.h"
#include "hbapiitm.h"

HB_FUNC( FT_NUMLOCK )
{
   int        iState = 0;
   HB_GT_INFO gtInfo;

   memset( &gtInfo, 0, sizeof( gtInfo ) );
   hb_gtInfo( HB_GTI_KBDSHIFTS, &gtInfo );
   if( gtInfo.pResult )
   {
      iState         = hb_itemGetNI( gtInfo.pResult );
      gtInfo.pNewVal = gtInfo.pResult;
      gtInfo.pResult = NULL;
   }

   if( HB_ISLOG( 1 ) )
   {
      int iNewState = hb_parl( 1 ) ?
                      ( iState | HB_GTI_KBD_NUMLOCK ) :
                      ( iState & ~HB_GTI_KBD_NUMLOCK );
      gtInfo.pNewVal = hb_itemPutNI( gtInfo.pNewVal, iNewState );
      hb_gtInfo( HB_GTI_KBDSHIFTS, &gtInfo );
   }

   if( gtInfo.pNewVal )
      hb_itemRelease( gtInfo.pNewVal );
   if( gtInfo.pResult )
      hb_itemRelease( gtInfo.pResult );

   hb_retl( ( iState & HB_GTI_KBD_NUMLOCK ) != 0 );
}
