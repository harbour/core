/*
 * $Id$
 */

#include "hbsetup.h"

#if defined(HARBOUR_USE_MSAPI)
   #if defined(HARBOUR_USE_DOS_MSAPI)
      #include "mouse/mousedos.c"
   #elif defined(HARBOUR_USE_OS2_MSAPI)
      #include "mouse/mouseos2.c"
   #elif defined(HARBOUR_USE_WIN_MSAPI)
      #include "mouse/mousewin.c"
   #else
      #error The MOUSE API platform was not specified via HARBOUR_USE_<platform>_MSAPI
      #error See include/hbsetup.h for details
   #endif
#endif
