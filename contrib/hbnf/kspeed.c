/* This is an original work by James R. Zack and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:06:54   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:54:40   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:03:28   GLENN
   Nanforum Toolkit
 */

#include "hbapi.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
#endif

HB_FUNC( FT_SETRATE )
{
#if defined( HB_OS_DOS )
   {
      union REGS regs;

      regs.h.ah = 0x03;
      regs.h.al = 0x05;
      regs.h.bh = ( HB_U8 ) hb_parni( 1 );  /* speed */
      regs.h.bl = ( HB_U8 ) hb_parni( 2 );  /* repeat */
      HB_DOS_INT86( 0x16, &regs, &regs );
   }
#endif
}
