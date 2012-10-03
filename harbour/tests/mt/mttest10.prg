/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for using independent console window in
 *    different thread. It needs GT driver which supports such functionality.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

#include "inkey.ch"

#ifdef __PLATFORM__WINDOWS
   REQUEST HB_GT_WVT_DEFAULT
   #define THREAD_GT hb_gtVersion()
#else
   REQUEST HB_GT_STD_DEFAULT
   #define THREAD_GT "XWC"
#endif

proc main()
   local i, aThreads

   if ! hb_mtvm()
      ? "This program needs HVM with MT support"
      quit
   endif

   ? "Starting threads..."
   aThreads := {}
   for i := 1 to 3
      aadd( aThreads, hb_threadStart( @thFunc() ) )
      ? i, "=>", atail( aThreads )
   next

   ? "Waiting for threads"
   while inkey() != K_ESC
      if hb_threadWait( aThreads, 0.1, .t. ) == len( aThreads )
         wait
         exit
      endif
      ?? "."
   enddo
return

proc thFunc()
   /* allocate own GT driver */
   hb_gtReload( THREAD_GT )
   use test shared
   browse()
return
