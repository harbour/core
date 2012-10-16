/*
 * $Id$
 */

/*
 * Author....: James R. Zack
 * CIS ID....: 75410,1567
 *
 * This is an original work by James R. Zack and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *     Rev 1.2   15 Aug 1991 23:06:54   GLENN
 *  Forest Belt proofread/edited/cleaned up doc
 *
 *     Rev 1.1   14 Jun 1991 19:54:40   GLENN
 *  Minor edit to file header
 *
 *     Rev 1.0   01 Apr 1991 01:03:28   GLENN
 *  Nanforum Toolkit
 *
 */

#include "hbapi.h"

#if defined( HB_OS_DOS )
#  include "dos.h"
#endif

HB_FUNC( FT_SETRATE )
{
#if defined( HB_OS_DOS )
   {
      union REGS  registers;
      int         tempo = 0, nrepete = 0;

      switch( hb_pcount() )
      {
         case 0:
            tempo    = 0;
            nrepete  = 0;
            break;
         case 1:
            tempo    = hb_parni( 1 );
            nrepete  = 0;
            break;
         case 2:
            tempo    = hb_parni( 1 );
            nrepete  = hb_parni( 2 );
            break;
      }

      registers.h.ah = 0x03;
      registers.h.al = 0x05;
      registers.h.bh = tempo;
      registers.h.bl = nrepete;
      HB_DOS_INT86( 0x16, &registers, &registers );
   }
#endif
}
