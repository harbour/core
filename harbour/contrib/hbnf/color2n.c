/*
 * $Id$
 */

/*
 * Author....: David Richardson
 * CIS ID....: 72271,53
 *
 * This function is an original work by David Richardson and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   22 Apr 2004 15:44:00   DGH
 * Fixed compiler warnings about pointer vs. integer by changing NULL to 0.
 * Commented out #ifdef and #endif lines, because there is nothing that is
 * even remotely platform-specific in the code.
 *    Rev 1.0   01 Jan 1995 03:01:00   TED
 * Initial release
 *
 */

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( FT_COLOR2N )
{
   int iRet = 0;

   if( HB_ISCHAR( 1 ) )
   {
      iRet = hb_gtColorToN( hb_parc( 1 ) );
      if( iRet == -1 )
         iRet = 0;
   }

   hb_retni( iRet );
}
