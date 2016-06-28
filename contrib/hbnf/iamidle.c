/* Rewritten in 2008 by Viktor Szakats (vszakats.net/harbour) and kept in the
   public domain.
   This is an original work by Ted Means and is placed in the public domain.

      Rev 1.0   01 Jan 1995 03:01:00   TED
   Nanforum Toolkit
 */

#include "hbapi.h"

HB_FUNC( FT_IAMIDLE )
{
   hb_releaseCPU();
}
