/* Rewritten in 2008 by Viktor Szakats (vszakats.net/harbour) and kept in the
   public domain.
   This is an original work by Ted Means and is placed in the public domain.
   I got the idea from Norm Mongeau, but the code is all mine.

      Rev 1.3   16 Jul 1993 00:00:18   GLENN
   Modified for compatibility in protected mode under ExoSpace.  Should
   work in real mode as well.

      Rev 1.2   15 Aug 1991 23:07:56   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:54:38   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:03:26   GLENN
   Nanforum Toolkit
 */

#include "hbapi.h"

#if defined( HB_OS_UNIX )
   #define _P_  NULL
#else
   #define _P_  "PRN"
#endif

HB_FUNC( FT_ISPRINT )
{
   hb_retl( hb_printerIsReady( HB_ISCHAR( 1 ) ? hb_parc( 1 ) : _P_ ) );
}
