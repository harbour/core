/*
 * $Id$
 */

#if defined(HARBOUR_USE_GTAPI)
   #if defined(HARBOUR_USE_DOS_GTAPI)
      #include "gt/gtdos.c"
   #elif defined(HARBOUR_USE_OS2_GTAPI)
      #include "gt/gtos2.c"
   #elif defined(HARBOUR_USE_WIN_GTAPI)
      #include "gt/gtwin.c"
   #else
      #error The GT API platform was not specified via HARBOUR_USE_<platform>_GT
      #error See include/hbsetup.h for details
   #endif
#endif
